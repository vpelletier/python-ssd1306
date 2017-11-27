# Copyright (C) 2017  Vincent Pelletier <plr.vincent@gmail.com>
#
# This file is part of python-ssd1306.
# python-ssd1306 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# python-ssd1306 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with python-ssd1306.  If not, see <http://www.gnu.org/licenses/>.
from __future__ import division
import fcntl
import struct
import functools
try:
    import freetype
except ImportError:
    freetype = None

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

COLOR_OFF = 0
COLOR_ON = 1
COLOR_XOR = -1

class Framebuffer(object):
    __slots__ = (
        '_buf',
        '_width',
        '_height',
        '_pixel_height',
        '_display',
        '_start_line',
        '_start_line_dirty',
    )

    def __init__(self, display):
        self._display = display
        self._start_line = 0
        self._start_line_dirty = True
        self._width = width = display.width
        self._height = height = display.page_count
        self._pixel_height = height * 8
        display.setAddressingMode(ADDRESSING_MODE_HORIZONTAL)
        display.setColumnAddressRange(0, width - 1)
        display.setPageAddressRange(0, height - 1)
        self._buf = buf = bytearray(width * height + 1)
        buf[0] = CONTROL_TYPE_DATA

    @property
    def width(self):
        return self._width

    @property
    def height(self):
        return self._pixel_height

    def blit(self):
        display = self._display
        display.blitRaw(self._buf)
        if self._start_line_dirty:
            display.setDisplayStartLine(self._start_line)
            self._start_line_dirty = False

    def putPixel(self, x, y, color=COLOR_ON):
        width = self._width
        if not 0 <= x < width or not 0 <= y < self._pixel_height:
            raise IndexError
        page, bit = divmod(y + self._start_line, 8)
        self._maskWord(
            self._buf,
            (page % self._height) * width + x + 1,
            1 << bit,
            color,
        )

    def getPixel(self, x, y):
        width = self._width
        if not 0 <= x < width or not 0 <= y < self._pixel_height:
            raise IndexError
        page, bit = divmod(y + self._start_line, 8)
        return (self._buf[(page % self._height)  * width + x + 1] >> bit) & 1

    @staticmethod
    def _maskWord(buf, offset, word, color):
        assert 0 < offset
        if color == COLOR_ON:
            buf[offset] |= word
        elif color == COLOR_XOR:
            buf[offset] ^= word
        elif color == COLOR_OFF:
            buf[offset] &= word ^ 0xff
        else:
            raise ValueError

    def blank(self, color=COLOR_OFF):
        if color not in (COLOR_OFF, COLOR_ON):
            raise ValueError
        if color:
            color = 0xff
        buf = self._buf
        for index in xrange(self._width * self._height):
            buf[index + 1] = color

    def line(self, ax, ay, bx, by, color=COLOR_ON):
        width = self._width
        pixel_height = self._pixel_height
        if (
            not 0 <= ax < width or
            not 0 <= ay < pixel_height or
            not 0 <= bx < width or
            not 0 <= by < pixel_height
        ):
            raise IndexError
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
        page, bit = divmod(ay + self._start_line, 8)
        offset = (page % self._height) * width + ax + 1
        buf_len = len(self._buf)
        buf_inc = buf_len - 1
        buf = self._buf
        maskWord = self._maskWord
        err = 0
        if abs_delta_y >= abs_delta_x:
            err_delta = float(abs_delta_x) // abs_delta_y
            word = 0
            offset_inc = 0
            for _ in xrange(abs_delta_y + 1):
                word |= 1 << bit
                bit += 1
                if bit == 8:
                    offset_inc = width
                    bit = 0
                err += err_delta
                if err > 0.5:
                    offset_inc += inc_x
                    err -= 1
                if offset_inc:
                    maskWord(buf, offset, word, color)
                    offset += offset_inc
                    if offset >= buf_len:
                        offset -= buf_inc
                    offset_inc = 0
                    word = 0
            if word:
                maskWord(buf, offset, word, color)
        else:
            err_delta = float(abs_delta_y) // abs_delta_x
            for _ in xrange(abs_delta_x + 1):
                maskWord(buf, offset, 1 << bit, color)
                offset += inc_x
                err += err_delta
                if err > 0.5:
                    bit += 1
                    if bit == 8:
                        offset += width
                        if offset >= buf_len:
                            offset -= buf_inc
                        bit = 0
                    err -= 1

    def rect(self, ax, ay, bx, by, color=COLOR_ON, fill=False):
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

    def circle(self, x, y, r, color=COLOR_ON, fill=False):
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

    def blitRowImage(self, x, y, width, data, color=COLOR_ON, packed=False, big_endian=False):
        """
        x:
            Left image border screen coordinate.
        y:
            Top image border screen coordinate.
        width:
            Image width, in pixels.
        data:
            Image data. First byte contains the leftmost 8 pixels of the
            topmost row, then the 8 following pixel of the topmost row,
            and so on until given width.
        packed:
            When False and image right border is not on a byte boundary,
            discard remaining bits, next byte starts the next line.
        big_endian:
            When True image data MSbs are leftmost pixels.
        """
        putPixel = self.putPixel
        bit_shift = range(8)
        if big_endian:
            bit_shift.reverse()
        dx = dy = 0
        for word in data:
            for bit in bit_shift:
                if (word >> bit) & 1:
                    putPixel(x + dx, y + dy, color)
                dx += 1
                if dx == width:
                    dx = 0
                    dy += 1
                    if not packed:
                        break

    def blitColumnImage(self, x, y, height, data, color=COLOR_ON, packed=False, big_endian=False):
        """
        x:
            Left image border screen coordinate.
        y:
            Top image border screen coordinate.
        height:
            Image height, in pixels.
        data:
            Image data. First byte contains the topmost 8 pixels of the
            leftmost column, then the 8 following pixel of the leftmost column,
            and so on until given height.
        packed:
            When False and image right border is not on a byte boundary,
            discard remaining bits, next byte starts the next column.
        big_endian:
            When True image data MSbs are topmost pixels.
        """
        putPixel = self.putPixel
        bit_shift = range(8)
        if big_endian:
            bit_shift.reverse()
        dx = dy = 0
        for word in data:
            for bit in bit_shift:
                if (word >> bit) & 1:
                    putPixel(x + dx, y + dy, color)
                dy += 1
                if dy == height:
                    dy = 0
                    dx += 1
                    if not packed:
                        break

    def scroll(self, line_count, color=COLOR_OFF):
        """
        Move screen content up by line_count lines (or down if negative).
        color:
            The color to paint the new lines in.
        """
        if line_count == 0:
            return
        pixel_height = self._pixel_height
        if abs(line_count) >= pixel_height:
            bottom = 0
            self.blank(color)
        else:
            bottom = self._start_line + line_count
            if color != -1:
                if line_count < 0:
                    ymin = pixel_height + line_count
                    ymax = pixel_height - 1
                else:
                    ymin = 0
                    ymax = line_count - 1
                self.rect(0, ymin, self._width - 1, ymax, color, True)
        self._start_line_dirty = True
        self._start_line = bottom % pixel_height

    @staticmethod
    def _repackAndScissor(bitmap, crop_top=0, crop_bottom=0):
        # freetype bitmaps may contain all-empty bytes at end of line, which
        # are not handled by blitting, so repack when needed.
        # Also, height not being guaranteed, crop top & bottom to not escape
        # intended rendering rect.
        pitch = (7 + bitmap.width) // 8
        if pitch == bitmap.pitch:
            result = list(bitmap.buffer)
        else:
            result = []
            x = 0
            for data in bitmap.buffer:
                if x < pitch:
                    result.append(data)
                x += 1
                x %= bitmap.pitch
        if crop_top:
            result = result[crop_top * pitch:]
        if crop_bottom:
            result = result[:-crop_bottom * pitch]
        return result

    def printLineAt(self, face, x, y, text, width=None, height=12, color=COLOR_ON):
        """
        Render <text> as a single line, with top-left corner at (<x>, <y>) and
        bottom-right corner at most at (<x> + <width>, <y> + <height>),
        in <color> and using font <face> (a freetype.Face instance).
        Returns the number of chars which could fit.
        Stops printing when encountering a \\t (newline) char.
        """
        if width is None:
            width = self._width - x
        baseline = int(height * 4 / 5)
        blitRowImage = self.blitRowImage
        face.set_pixel_sizes(0, height - 2)
        previous_char = None
        rendered = 0
        for char in text:
            if char == u'\n':
                rendered += 1
                break
            face.load_char(
                char,
                freetype.FT_LOAD_RENDER |
                freetype.FT_LOAD_MONOCHROME |
                freetype.FT_LOAD_TARGET_MONO,
            )
            glyph = face.glyph
            bitmap = glyph.bitmap
            char_width = max(
                glyph.advance.x // 64,
                glyph.bitmap_left + bitmap.width,
            ) + face.get_kerning(previous_char, char).x // 64
            width -= char_width
            if width < 0:
                if not rendered:
                    raise ValueError('Too narrow for first char')
                break
            y_offset = baseline - glyph.bitmap_top
            crop_top = -min(0, y_offset)
            y_offset = max(0, y_offset)
            blitRowImage(
                x + glyph.bitmap_left,
                y + y_offset,
                bitmap.width,
                self._repackAndScissor(
                    bitmap,
                    crop_top,
                    max(0, (y_offset + bitmap.rows) - height),
                ),
                color=color,
                big_endian=True,
            )
            x += char_width
            previous_char = char
            rendered += 1
        return rendered

    def printAt(self, face, x, y, text, width=None, height=None, line_height=12, color=COLOR_ON, scroll=False):
        """
        Render multi-line <text>, wrapping on \\n and when next char would not
        fit, with top-left corner at (<x>, <y>) and bottom-right corner at most
        at (<x> + <width>, <y> + <height>), in <color> and using font <face>
        (a freetype.Face instance), each line having a height of <line_height>.
        Returns the number of chars which could fit.
        Stop printing before reaching screen bottom, unless <scroll> is true,
        in which case it will scroll the screen up.
        """
        if height is None:
            height = self._pixel_height - y
        total_printed = 0
        while text:
            y_overflow = y + line_height - height
            if y_overflow > 0:
                if scroll:
                    self.scroll(y_overflow, not color)
                    y -= y_overflow
                else:
                    break
            printed = self.printLineAt(face, x, y, text, width, line_height, color)
            total_printed += printed
            text = text[printed:]
            y += line_height
        return total_printed

class SSD1306(object):
    width = 128
    height = 64
    page_count = height // 8

    def __init__(self, bus, sa0):
        if sa0 not in (0, 1):
            raise ValueError('sa0 can be either 0 or 1')
        self._bus = bus = open('/dev/i2c-%i' % bus, 'r+b', 0)
        result = fcntl.ioctl(bus, I2C_SLAVE, 0b0111100 | sa0)
        if result:
            raise IOError(-result)

    def _writeCommand(self, *command):
        buf = bytearray(len(command) * 2)
        for index, payload_byte in enumerate(command):
            index *= 2
            buf[index] = CONTROL_TYPE_COMMAND
            buf[index + 1] = payload_byte
        self._bus.write(buf)

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

    def getStatus(self):
        status, = self._bus.read(1)
        return ord(status)

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
        return Framebuffer(self)

def test():
    dev = SSD1306(1, 1)
    dev.reset()
    dev.initialise(False, False)
    dev.setAddressingMode(ADDRESSING_MODE_HORIZONTAL)

    # Dry-testing the low-level Framebuffer API
    fb = dev.getFramebuffer()

    # Some actual drawing
    blit = fb.blit

    fb.putPixel(0, 4)
    fb.putPixel(9, 4)
    fb.putPixel(0, 14)
    fb.putPixel(9, 14)

    x = 20
    y = 4
    fb.rect(x, y, x + 9, y + 9)
    fb.putPixel(x, y - 2)
    fb.putPixel(x - 2, y)
    fb.putPixel(x + 9 + 2, y + 9)
    fb.putPixel(x + 9, y + 9 + 2)

    x = 40
    y = 4
    fb.rect(x, y, x + 9, y + 9, fill=True)
    fb.putPixel(x, y - 2)
    fb.putPixel(x - 2, y)
    fb.putPixel(x + 9 + 2, y + 9)
    fb.putPixel(x + 9, y + 9 + 2)

    fb.circle(20, 30, 10)
    fb.putPixel(20, 30)
    fb.putPixel(10, 20)
    fb.putPixel(12, 20)
    fb.putPixel(14, 20)
    fb.putPixel(10, 22)
    fb.putPixel(10, 24)
    fb.putPixel(30, 40)
    fb.putPixel(28, 40)
    fb.putPixel(26, 40)
    fb.putPixel(30, 38)
    fb.putPixel(30, 36)

    fb.circle(50, 30, 10, fill=True)
    fb.putPixel(50, 30, COLOR_OFF)
    fb.putPixel(40, 20)
    fb.putPixel(42, 20)
    fb.putPixel(44, 20)
    fb.putPixel(40, 22)
    fb.putPixel(40, 24)
    fb.putPixel(60, 40)
    fb.putPixel(58, 40)
    fb.putPixel(56, 40)
    fb.putPixel(60, 38)
    fb.putPixel(60, 36)

    cx = 100
    cy = 30
    for offset in xrange(0, 42, 4):
        fb.line(cx, cy, cx - 20 + offset, cy + 20)
        fb.line(cx, cy, cx - 20 + offset, cy - 20)
        fb.line(cx, cy, cx + 20, cy - 20 + offset)
        fb.line(cx, cy, cx - 20, cy - 20 + offset)
    fb.rect(cx - 21, cy - 21, cx + 21, cy + 21, COLOR_XOR)

    fb.putPixel(0, 0)
    fb.putPixel(127, 0)
    fb.putPixel(0, 63)
    fb.putPixel(127, 63)

    fb.blitRowImage(
        5,
        45,
        26,
        [
            0b10001011, 0b11010000, 0b10000011, 0b00101010,
            0b10001010, 0b00010000, 0b10000100, 0b10010101,
            0b10001010, 0b00010000, 0b10001000, 0b01101010,
            0b11111010, 0b00010000, 0b10001000, 0b01010101,
            0b10001011, 0b10010000, 0b10001000, 0b01101010,
            0b10001010, 0b00010000, 0b10001000, 0b01010101,
            0b10001010, 0b00010000, 0b10000100, 0b10101010,
            0b10001011, 0b11011110, 0b11110011, 0b00010101,
        ],
        big_endian=True,
    )

    fb.blitColumnImage(
        33,
        44,
        10,
        [
            0b01110000, 0b10101010,
            0b11111101, 0b11010101,
            0b01110000, 0b10101010,
        ],
        big_endian=True,
    )

    blit()

    def testCard(fb):
        fb.blank()
        fb.rect(0, 0, 127, 63)
        for row in xrange(0, 33, 16):
            fb.line(0, row, 127, row)
            fb.line(0, 63 - row, 127, 63 - row)
        for column in xrange(0, 65, 16):
            fb.line(column, 0, column, 63)
            fb.line(127 - column, 0, 127 - column, 63)
        for radius, color, fill in (
            (30, 0, True),
            (27, 1, True),
            (30, 1, False),
        ):
            for x, y in (
                (63, 31),
                (63, 32),
                (64, 32),
                (64, 31),
            ):
                fb.circle(x, y, radius, color, fill)
        fb.line(2, 2, 12, 12)
        fb.line(125, 2, 115, 12)
        fb.line(2, 61, 12, 51)
        fb.line(125, 61, 115, 51)
        fb.line(0, 0, 127, 63, COLOR_XOR)
        fb.line(127, 0, 0, 63, COLOR_XOR)
        fb.blitRowImage(
            2, 18,
            13,
            [
                0b11111111, 0b11111000,
                0b10000000, 0b00010000,
                0b10000000, 0b00000000,
                0b10000000, 0b00101000,
                0b10000000, 0b00010000,
                0b10000000, 0b00101000,
                0b10000000, 0b00000000,
                0b10000000, 0b00000000,
                0b10000000, 0b00000000,
                0b10010100, 0b00000000,
                0b11001000, 0b00000000,
                0b10001000, 0b00000000,
            ],
            big_endian=True,
        )

    testCard(fb)
    blit()
    import time

    # Scrolling
    BEGIN = time.time()
    for _ in xrange(32):
        fb.scroll(1)
        blit()
    duration = time.time() - BEGIN
    print 'Scroll up: %.4fs (%i fps)' % (duration, 32 / duration)

    testCard(fb)
    blit()
    for _ in xrange(52):
        fb.scroll(1)
        blit()

    BEGIN = time.time()
    for _ in xrange(10):
        fb.scroll(-2)
        blit()
    duration = time.time() - BEGIN
    print 'Scroll down: %.4fs (%i fps)' % (duration, 10 / duration)

    # Rectangle fill
    BEGIN = time.time()
    fb.blank(COLOR_ON)
    for n in xrange(2, 32, 2):
        fb.rect(n, n, 127 - n, 63 - n, COLOR_XOR, True)
        blit()
    duration = time.time() - BEGIN
    print 'Alternating rects: %.4fs (%i fps)' % (duration, 15 / duration)

    BEGIN = time.time()
    fb.blank(COLOR_ON)
    for n in xrange(2, 32, 2):
        fb.rect(n, n, 127 - n, 63 - n, COLOR_XOR, True)
    blit()
    duration = time.time() - BEGIN
    print 'Alternating rects single blit: %.4fs (%i fps)' % (duration, 15 / duration)

    BEGIN = time.time()
    for n in xrange(64):
        blit()
    duration = time.time() - BEGIN
    print 'Blit loop: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank()
    for x in xrange(64):
        fb.line(x, 0, x, x)
        blit()
    duration = time.time() - BEGIN
    print 'Vertical lines: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank()
    for x in xrange(64):
        fb.line(x, 0, x, x)
    blit()
    duration = time.time() - BEGIN
    print 'Vertical lines single blit: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank()
    for y in xrange(64):
        fb.line(0, y, y, y)
        blit()
    duration = time.time() - BEGIN
    print 'Horizontal lines: %.4fs (%i fps)' % (duration, 64 / duration)

    BEGIN = time.time()
    fb.blank()
    for y in xrange(64):
        fb.line(0, y, y, y)
    blit()
    duration = time.time() - BEGIN
    print 'Horizontal lines single blit: %.4fs (%i fps)' % (duration, 64 / duration)

    try:
        face = freetype.Face('/usr/share/fonts/truetype/noto/NotoMono-Regular.ttf')
    except AttributeError:
        print 'freetype module missing, skipping font demo'
    except freetype.ft_errors.FT_Exception:
        print 'Noto-Mono-Regular not found, skipping font demo'
    else:
        fb.blank()
        BEGIN = time.time()
        fb.printAt(face, 0, 0, u"Hello, world !")
        duration = time.time() - BEGIN
        print 'Print single line, no blit: %.4fs (%i fps, %i cps)' % (duration, 1 / duration, 14 / duration)
        blit()
        time.sleep(0.2)

        fb.blank(COLOR_ON)
        fb.printAt(face, 0, 0, u"Hello, world !", color=COLOR_OFF)
        blit()
        time.sleep(0.2)

        fb.blank()
        fb.printAt(face, 0, 0, u"The quick brown fox jumps over the lazy dog. " * 3, scroll=True)
        blit()
        time.sleep(0.2)

        BEGIN = time.time()
        for size in xrange(64, 4, -1):
            fb.blank()
            fb.printAt(face, 0, 0, u"Hello, world !\n1234567890\nabcdefghijklmnopqrstuvwxyz", line_height=size)
            blit()
        duration = time.time() - BEGIN
        print 'Print: %.4fs (%i fps)' % (duration, (64 - 4) / duration)

    dev.power(False)

if __name__ == '__main__':
    test()
