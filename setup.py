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
from setuptools import setup
from codecs import open
import os
import sys

extra = {}
if sys.version_info >= (3, ):
    extra['use_2to3'] = True

long_description = open(
    os.path.join(os.path.dirname(__file__), 'README.rst'),
    encoding='utf8',
).read()

setup(
    name='ssd1306',
    description=next(x for x in long_description.splitlines() if x.strip()),
    long_description='.. contents::\n\n' + long_description,
    keywords='oled ssd1306',
    version='0.1',
    author='Vincent Pelletier',
    author_email='plr.vincent@gmail.com',
    url='http://github.com/vpelletier/python-ssd1306',
    license='GPLv3+',
    platforms=['linux'],
    packages=['ssd1306'],
    classifiers=[
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy',
        'Topic :: System :: Hardware :: Hardware Drivers',
    ],
    zip_safe=True,
    **extra
)
