import fcntl
import struct
import functools

# From linux/i2c-dev.h
I2C_SLAVE = 0x0703          # Use this slave address

DISPLAY_STATUS_ON = 0x40

ADDRESSING_MODE_HORIZONTAL  = 0
ADDRESSING_MODE_VERTICAL    = 1
ADDRESSING_MODE_PAGE        = 2

# Spec names this the continuation bit, which when cleared means the rest of
# the transfer is only display data. So bits 7 and 6 are mutualy exclusive,
# so name 7 the command bit and 6 the data bit.
CONTROL_TYPE_COMMAND = 0x80
CONTROL_TYPE_DATA    = 0x40

class Framebuffer(object):
    __slots__ = (
        '_buf',
        '_width',
        '_height',
    )

    def __init__(self, width, height):
        """
        width: (int)
            Framebuffer width in pixels.
        height: (int)
            Framebuffer heigth in pages.
        """
        self._buf = bytearray(width * height + 1)
        self._buf[0] = CONTROL_TYPE_DATA
        self._width = width
        self._height = height

    def putPixel(self, x, y, color):
        width = self._width
        if x >= width:
            return
        page, bit = divmod(y, 8)
        self._maskWord(
            self._buf,
            page * width + x + 1,
            1 << bit,
            color,
        )

    def getPixel(self, x, y):
        width = self._width
        if not 0 <= x < width or y < 0:
            raise IndexError
        page, bit = divmod(y, 8)
        return (self._buf[page * width + x + 1] >> bit) & 1

    def putWord(self, x, page, word):
        width = self._width
        if x >= width:
            return
        offset = page * width + x + 1
        buf = self._buf
        if 0 < offset < len(buf):
            buf[offset] = word

    def getWord(self, x, page):
        width = self._width
        if not 0 <= x < width or y < 0:
            raise IndexError
        return self._buf[page * width + x + 1]

    @staticmethod
    def _maskWord(buf, offset, word, color):
        if 0 < offset:
            try:
                if color == 1:
                    buf[offset] |= word
                elif color == -1:
                    buf[offset] ^= word
                else:
                    buf[offset] &= word ^ 0xff
            except IndexError:
                pass

    def getBlitter(self, display, xmin=0, pagemin=0, lonely=False):
        """
        Return a callable which can blit this framebuffer's content to given
        on-screen coordinates.
        Changes made to the framebuffer after this method has returned
        continue affeting the blitter at no cost.
        Sets the display in horizontal addressing mode, and expects it to stay
        that way.

        display: (SSD1306)
            The display to which blitting will happen.
        xmin: (int)
            Left screen coordinate.
        pagemin: (int)
            Top screen coordinate.
        lonely: (bool)
            Set to True if this is the only blitter affecting given display,
            to save a tiny bit of speed (avoid reconfiguring display cissors
            on each blit).
        """
        width = self._width
        height = self._height
        assert 0 <= xmin <= display.width - width
        assert 0 <= pagemin <= display.height - height
        display.setAddressingMode(ADDRESSING_MODE_HORIZONTAL)
        xmax = xmin + width - 1
        pagemax = pagemin + height - 1
        if lonely:
            display.setColumnAddressRange(xmin, xmax)
            display.setPageAddressRange(pagemin, pagemax)
            return functools.partial(display.blitRaw, self._buf)
        return functools.partial(self.__blitRaw, display, xmin, pagemin, xmax, pagemax, self._buf)

    @staticmethod
    def __blitRaw(display, xmin, pagemin, xmax, pagemax, buf):
        display.setColumnAddressRange(xmin, xmax)
        display.setPageAddressRange(pagemin, pagemax)
        display.blitRaw(buf)

    def blank(self, color):
        assert color in (0, 1)
        if color:
            color = 0xff
        buf = self._buf
        for index in xrange(self._width * self._height):
            buf[index + 1] = color

    def line(self, ax, ay, bx, by, color):
        delta_x = bx - ax
        delta_y = by - ay
        if delta_x == delta_y == 0:
            self.putPixel(ax, ay, color)
            return
        inc_x = 1 if delta_x > 0 else -1
        if delta_y < 0:
            ax = bx
            ay = by
            inc_x = -inc_x
        abs_delta_x = abs(delta_x)
        abs_delta_y = abs(delta_y)
        err_limit = 0.5
        width = self._width
        page, bit = divmod(ay, 8)
        offset = page * width + ax + 1
        buf = self._buf
        maskWord = self._maskWord
        err = 0
        if abs_delta_y >= abs_delta_x:
            err_delta = float(abs_delta_x) / abs_delta_y
            word = 0
            offset_inc = 0
            for _ in xrange(abs_delta_y + 1):
                word |= 1 << bit
                bit += 1
                if bit == 8:
                    offset_inc = width
                    bit = 0
                err += err_delta
                if err > err_limit:
                    offset_inc += inc_x
                    err -= 1
                if offset_inc:
                    maskWord(buf, offset, word, color)
                    offset += offset_inc
                    offset_inc = 0
                    word = 0
            if word:
                maskWord(buf, offset, word, color)
        else:
            err_delta = float(abs_delta_y) / abs_delta_x
            for _ in xrange(abs_delta_x + 1):
                maskWord(buf, offset, 1 << bit, color)
                offset += inc_x
                err += err_delta
                if err > err_limit:
                    bit += 1
                    if bit == 8:
                        offset += width
                        bit = 0
                    err -= 1

    def rect(self, ax, ay, bx, by, color, fill=False):
        line = self.line
        if fill:
            delta_x = bx - ax
            delta_y = by - ay
            abs_delta_x = abs(delta_x)
            abs_delta_y = abs(delta_y)
            # Note: could be accelerated (after line is) by favoring y lines
            if abs_delta_y >= abs_delta_x:
                inc = 1 if delta_x > 0 else -1
                for _ in xrange(abs_delta_x + 1):
                    line(ax, ay, ax, by, color)
                    ax += inc
            else:
                inc = 1 if delta_y > 0 else -1
                for _ in xrange(abs_delta_y + 1):
                    line(ax, ay, bx, ay, color)
                    ay += inc
        else:
            line(ax, ay, bx - 1, ay, color)
            line(bx, ay, bx, by - 1, color)
            line(bx, by, ax + 1, by, color)
            line(ax, by, ax, ay + 1, color)

    def circle(self, x, y, r, color, fill=False):
        deltax = r
        deltay = 0
        err = 0
        if fill:
            line = self.line
            def draw(bx, by, color=color):
                line(bx, y, bx, by, color)
        else:
            putPixel = self.putPixel
            def draw(bx, by, color=color):
                putPixel(bx, by, color)
        while deltax >= deltay:
            draw(x + deltax, y + deltay)
            draw(x + deltay, y + deltax)
            draw(x - deltay, y + deltax)
            draw(x - deltax, y + deltay)
            draw(x - deltax, y - deltay)
            draw(x - deltay, y - deltax)
            draw(x + deltay, y - deltax)
            draw(x + deltax, y - deltay)
            if err <= 0:
                deltay += 1
                err += 2 * deltay + 1
            if err > 0:
                deltax -= 1
                err -= 2 * deltax + 1

class SSD1306(object):
    width = 128
    height = 64
    page_count = height / 8

    def __init__(self, bus, sa0):
        if sa0 not in (0, 1):
            raise ValueError('sa0 can be either 0 or 1')
        self._bus = bus = open('/dev/i2c-%i' % bus, 'r+b', 0)
        result = fcntl.ioctl(bus, I2C_SLAVE, 0b0111100 | sa0)
        if result:
            raise IOError(-result)

    def _read(self, control, length):
        # TODO: figure reads out
        raise NotImplementedError

    def _writeCommand(self, *command):
        buf = bytearray(len(command) * 2)
        for index, payload_byte in enumerate(command):
            index *= 2
            buf[index] = CONTROL_TYPE_COMMAND
            buf[index + 1] = payload_byte
        self._bus.write(buf)

    def _readCommand(self, length):
        return self._read(CONTROL_TYPE_COMMAND, length)

    def blitRaw(self, buf):
        # TODO: use AIO to not keep CPU waiting for transfer completion.
        # Requires implementing a-SA_SIGINFO-aware signal handler,
        # likely meaning ctypes-based registering.
        assert buf[0] == CONTROL_TYPE_DATA
        self._bus.write(buf)

    def blit(self, data):
        buf = bytearray(len(data) + 1)
        buf[0] = CONTROL_TYPE_DATA
        buf[1:] = data
        self._bus.write(buf)

    def _readData(self, length):
        return self._read(CONTROL_TYPE_DATA, length)

    def getStatus(self):
        return [ord(x) for x in self._read(1)]

    def setContrast(self, contrast):
        """
        contrast:
            0x00: dimest
            0xff: brightest
        """
        self._writeCommand(0x81, contrast)

    def setAllOn(self, all_on):
        """
        all_on:
            True: Turn all pixels on.
            False: Follow framebuffer content.

        Not affected by setInverse.
        """
        self._writeCommand(0xa4 | bool(all_on))

    def setInverse(self, inverse):
        """
        inverse:
            True: 0b0 turns pixel on.
            False: 0b1 turns pixel on.

        Take effect immediately on all pixels.
        """
        self._writeCommand(0xa6 | bool(inverse))

    def power(self, on):
        """
        on:
            True: Follow framebuffer content.
            False: Turn all pixels off.
        """
        self._writeCommand(0xae | bool(on))

    def setupContinuousHorizontalScroll(self, left, start_page, stop_page, frame_count):
        """
        left:
            True: scrolls to the left.
            False: scrolls to the right.
        start_page (0..f)
            First page to scroll.
        stop_page (start_page..f)
            Last page to scroll.
        frame_count:
            Number of frames between 1-pixel shifts.

        Stops scrolling (if already started).
        To start the effect, call scroll(True) after this.
        """
        start_page &= 0x7
        stop_page &= 0x7
        if start_page > stop_page:
            raise ValueError('Scroll area is empty')
        self.scroll(False)
        self._writeCommand(
            0x26 | bool(left),
            0x00,
            start_page,
            {
                  2: 0b111,
                  3: 0b100,
                  4: 0b101,
                  5: 0b000,
                 25: 0b110,
                 64: 0b001,
                128: 0b010,
                256: 0b011,
            }[frame_count],
            stop_page,
            0x00,
            0xff,
        )

    def setupContinuousDiagonalScroll(self, left, start_page, stop_page, frame_count, vertical_scroll):
        """
        left:
            True: scrolls to the left.
            False: scrolls to the right.
        start_page (0..f)
            First page to scroll.
        stop_page (start_page..f)
            Last page to scroll.
        frame_count:
            Number of frames between 1-pixel shifts.
        vertical_scroll:
            How many pixels to scroll vertically for one horizontal shift.

        Stops scrolling (if already started).
        To start the effect, call scroll(True) after this.
        """
        if start_page > stop_page:
            raise ValueError('Scroll area is empty')
        self.scroll(False)
        self._writeCommand(
            0x28 | {
                0: 0b01,
                1: 0b10,
            }[left],
            0x00,
            start_page & 0x07,
            {
                  2: 0b111,
                  3: 0b100,
                  4: 0b101,
                  5: 0b000,
                 25: 0b110,
                 64: 0b001,
                128: 0b010,
                256: 0b011,
            }[frame_count],
            stop_page & 0x07,
            vertical_scroll & 0x3f,
        )

    def setupContinuousVerticalScroll(self, start_row, height):
        """
        start_row:
            First row to scroll vertically.
        height:
            Number of rows to scroll.
        """
        self.scroll(False)
        self._writeCommand(
            0xa3,
            start_row & 0x3f,
            height & 0x7f,
        )

    def scroll(self, scroll):
        self._writeCommand(0x2e | bool(scroll))

    def setColumnStartAddressLow(self, address):
        self._writeCommand(0x00 | (address & 0xf))

    def setColumnStartAddressHigh(self, address):
        self._writeCommand(0x10 | (address & 0xf))

    def setAddressingMode(self, mode):
        if mode not in (
            ADDRESSING_MODE_HORIZONTAL,
            ADDRESSING_MODE_VERTICAL,
            ADDRESSING_MODE_PAGE,
        ):
            raise ValueError('Invalid addressing mode')
        self._writeCommand(0x20, mode)

    def setColumnAddressRange(self, start, stop):
        self._writeCommand(0x21, start & 0x7f, stop & 0x7f)

    def setPageAddressRange(self, start, stop):
        self._writeCommand(0x22, start & 0x7, stop & 0x7)

    def setPageStartAddress(self, page):
        self._writeCommand(0xb0 | (page & 0x07))

    def setDisplayStartLine(self, line):
        self._writeCommand(0x40 | (line & 0x3f))

    def setSegmentRemap(self, remap):
        self._writeCommand(0xa0 | bool(remap))

    def setMultiplexRatio(self, ratio):
        if ratio < 15:
            raise ValueError('Invalid ratio')
        self._writeCommand(0xa8, ratio & 0x3f)

    def setCOMOutputScanDirection(self, reverse):
        self._writeCommand(0xc0 | (bool(reverse) << 3))

    def setDisplayOffset(self, offset):
        self._writeCommand(0xd3, offset & 0x3f)

    def setCOMPinsHardwareConfiguration(self, progressive, left_right_remap):
        self._writeCommand(
            0xda,
            0x02 | (bool(left_right_remap) << 5) | (bool(progressive) << 4),
        )

    def setDisplayClock(self, frequency, divisor):
        # TODO: take frequency as input, compute freq & divisor internaly
        if not 0 <= frequency <= 0xf:
            raise ValueError('Frequency must be in 15..0 range')
        if not 1 <= divisor < 0x10:
            raise ValueError('Divisor must be in 16..1 range')
        self._writeCommand(
            0xd5,
            ((frequency & 0x0f) << 4)| ((divisor - 1) & 0x0f),
        )

    def setPrechargePeriod(self, phase1, phase2):
        if not phase1 or not phase2:
            raise ValueError()
        self._writeCommand(0xd9, (phase2 & 0x0f) << 4 | (phase1 & 0x0f))

    def setVcomhDeselectLevel(self, level):
        self._writeCommand(0xdb, (level & 0x03) << 4)

    def nop(self):
        self._writeCommand(0xe3)

    def setChargePump(self, enable):
        self._writeCommand(0x8d, 0x10 | (bool(enable) << 2))

    def reset(self):
        # Attempt to reset without access to the reset pin
        # Turn display off first
        self.power(False)
        self.setChargePump(False)

        self.setContrast(0x7f)
        self.setAllOn(False)
        self.setInverse(False)
        self.scroll(False)
        self.setColumnStartAddressLow(0)
        self.setColumnStartAddressHigh(0)
        self.setColumnAddressRange(0, self.width - 1)
        self.setPageAddressRange(0, self.page_count - 1)
        # Now that non-paged cissor is reset, empty framebuffer
        self.setAddressingMode(ADDRESSING_MODE_HORIZONTAL)
        self.blit([0x00] * self.width * self.page_count)
        # And set default addressing mode
        self.setAddressingMode(ADDRESSING_MODE_PAGE)
        self.setPageStartAddress(0) # XXX: reset value not specified
        self.setDisplayStartLine(0)
        self.setSegmentRemap(False)
        self.setMultiplexRatio(self.height - 1)
        self.setCOMOutputScanDirection(False)
        self.setDisplayOffset(0)
        self.setCOMPinsHardwareConfiguration(True, False)
        self.setDisplayClock(8, 1)
        self.setPrechargePeriod(2, 2)
        self.setVcomhDeselectLevel(2)

    def initialise(self, remap, reverse_scan):
        # SSD1306 App note Rev 0.4
        self.setMultiplexRatio(0x3f)
        self.setDisplayOffset(0)
        self.setDisplayStartLine(0)
        self.setSegmentRemap(remap)
        self.setCOMOutputScanDirection(reverse_scan)
        self.setCOMPinsHardwareConfiguration(True, False)
        self.setContrast(0x7f)
        self.setAllOn(False)
        self.setInverse(False)
        self.setDisplayClock(8, 1)
        self.setChargePump(True)
        self.power(True)

    def adafruit_reset(self):
        # Attempt to reset without access to the reset pin
        self.power(False)
        self.setDisplayClock(0x8, 0x0)
        self.setMultiplexRatio(0x3f) # height - 1
        self.setDisplayOffset(0)
        self.setDisplayStartLine(0)
        self.setChargePump(True) # XXX: not reset default
        self.setAddressingMode(ADDRESSING_MODE_HORIZONTAL) # XXX not reset default
        self.setSegmentRemap(1) # XXX: not reset default
        self.setCOMOutputScanDirection(True) # XXX: not reset default
        self.setCOMPinsHardwareConfiguration(True, False)
        self.setContrast(0xcf) # XXX: not reset default
        self.setPrechargePeriod(0x1, 0xf) # XXX: not reset default
        self.setVcomhDeselectLevel(0x4) # XXX: non-specified ?!
        self.setAllOn(False)
        self.setInverse(False)
        self.power(True) # XXX: not reset default

    def getFramebuffer(self):
        return Framebuffer(self.width, self.page_count)

def test():
    dev = SSD1306(1, 1)
    dev.reset()
    dev.initialise(False, False)
    dev.setAddressingMode(ADDRESSING_MODE_HORIZONTAL)
    #dev.blit([x & 0xff for x in xrange(128 * 8)])

    # Dry-testing the low-level Framebuffer API
    fb = dev.getFramebuffer()
    orig_buf = bytearray(fb._buf)
    # Must not raise, while doing nothing
    fb.putPixel(128, 0, 1)
    assert fb._buf == orig_buf
    fb.putPixel(0, 64, 1)
    assert fb._buf == orig_buf
    fb.putPixel(-1, 0, 1)
    assert fb._buf == orig_buf
    fb.putPixel(-2, 0, 1)
    assert fb._buf == orig_buf
    fb.putWord(-1, 0, 0xff)
    assert fb._buf == orig_buf
    fb.putWord(-2, 0, 0xff)
    assert fb._buf == orig_buf
    try:
        fb.getPixel(128, 0)
    except IndexError:
        pass
    else:
        raise AssertionError('Should have raised IndexError')
    try:
        fb.getPixel(0, 64)
    except IndexError:
        pass
    else:
        raise AssertionError('Should have raised IndexError')
    try:
        fb.getPixel(-1, 0)
    except IndexError:
        pass
    else:
        raise AssertionError('Should have raised IndexError')
    try:
        fb.getPixel(-2, 0)
    except IndexError:
        pass
    else:
        raise AssertionError('Should have raised IndexError')
    try:
        fb.getWord(-1, 0)
    except IndexError:
        pass
    else:
        raise AssertionError('Should have raised IndexError')
    try:
        fb.getWord(-2, 0)
    except IndexError:
        pass
    else:
        raise AssertionError('Should have raised IndexError')

    # Some actual drawing
    blit = fb.getBlitter(dev, lonely=True)

    fb.putPixel(0, 4, 1)
    fb.putPixel(9, 4, 1)
    fb.putPixel(0, 14, 1)
    fb.putPixel(9, 14, 1)

    x = 20
    y = 4
    fb.rect(x, y, x + 9, y + 9, 1)
    fb.putPixel(x, y - 2, 1)
    fb.putPixel(x - 2, y, 1)
    fb.putPixel(x + 9 + 2, y + 9, 1)
    fb.putPixel(x + 9, y + 9 + 2, 1)

    x = 40
    y = 4
    fb.rect(x, y, x + 9, y + 9, 1, True)
    fb.putPixel(x, y - 2, 1)
    fb.putPixel(x - 2, y, 1)
    fb.putPixel(x + 9 + 2, y + 9, 1)
    fb.putPixel(x + 9, y + 9 + 2, 1)

    fb.circle(20, 30, 10, 1)
    fb.putPixel(20, 30, 1)
    fb.putPixel(10, 20, 1)
    fb.putPixel(12, 20, 1)
    fb.putPixel(14, 20, 1)
    fb.putPixel(10, 22, 1)
    fb.putPixel(10, 24, 1)
    fb.putPixel(30, 40, 1)
    fb.putPixel(28, 40, 1)
    fb.putPixel(26, 40, 1)
    fb.putPixel(30, 38, 1)
    fb.putPixel(30, 36, 1)

    fb.circle(50, 30, 10, 1, True)
    fb.putPixel(50, 30, 0)
    fb.putPixel(40, 20, 1)
    fb.putPixel(42, 20, 1)
    fb.putPixel(44, 20, 1)
    fb.putPixel(40, 22, 1)
    fb.putPixel(40, 24, 1)
    fb.putPixel(60, 40, 1)
    fb.putPixel(58, 40, 1)
    fb.putPixel(56, 40, 1)
    fb.putPixel(60, 38, 1)
    fb.putPixel(60, 36, 1)

    cx = 100
    cy = 30
    for offset in xrange(0, 42, 4):
        fb.line(cx, cy, cx - 20 + offset, cy + 20, 1)
        fb.line(cx, cy, cx - 20 + offset, cy - 20, 1)
        fb.line(cx, cy, cx + 20, cy - 20 + offset, 1)
        fb.line(cx, cy, cx - 20, cy - 20 + offset, 1)
    fb.rect(cx - 21, cy - 21, cx + 21, cy + 21, -1)

    fb.putPixel(0, 0, 1)
    fb.putPixel(127, 0, 1)
    fb.putPixel(0, 63, 1)
    fb.putPixel(127, 63, 1)

    blit()

    import time

    BEGIN = time.time()
    fb.blank(1)
    for n in xrange(2, 32, 2):
        fb.rect(n, n, 127 - n, 63 - n, -1, True)
        blit()
    duration = time.time() - BEGIN
    print 'Alternating rects: %.4fs (%i fps)' % (duration, 15 / duration)

    BEGIN = time.time()
    fb.blank(1)
    for n in xrange(2, 32, 2):
        fb.rect(n, n, 127 - n, 63 - n, -1, True)
    blit()
    duration = time.time() - BEGIN
    print 'Alternating rects single blit: %.4fs (%i fps)' % (duration, 15 / duration)

    BEGIN = time.time()
    fb.blank(0)
    for x in xrange(64):
        fb.line(x, 0, x, x, 1)
        blit()
    duration = time.time() - BEGIN
    print 'Vertical lines: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank(0)
    for x in xrange(64):
        fb.line(x, 0, x, x, 1)
    blit()
    duration = time.time() - BEGIN
    print 'Vertical lines single blit: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank(0)
    for y in xrange(64):
        fb.line(0, y, y, y, 1)
        blit()
    duration = time.time() - BEGIN
    print 'Horizontal lines: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank(0)
    for y in xrange(64):
        fb.line(0, y, y, y, 1)
    blit()
    duration = time.time() - BEGIN
    print 'Horizontal lines single blit: %.4fs (%i fps)' % (duration, 64 / duration)

    dev.power(False)

if __name__ == '__main__':
    test()
