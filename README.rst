python-ssd1306 - Python userland driver for I²C-connected SSD1306 oled display

Drives the SSD1306 oled display in i2c mode. Control of non-i2c pins
(ex: reset) are out of the scope of this module.

Supports off-screen rendering of simple geometric primitives.
Supports font rendering if freetype module is installed.
Allows controlling hardware features (mirroring, constant scrolling, power on/off,
luminosity, ...).

Tested with Adafruit's 1.3" 128x64 board:
  https://www.adafruit.com/products/938
