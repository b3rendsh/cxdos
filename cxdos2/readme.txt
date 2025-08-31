CXDOS 2
=======

DOS ROM for MSX1 and newer machines with limited ROM or RAM resources, 
the kernel functions are compatible with MSX-DOS 2.

The kernel requires a 32K ROM and 64K system RAM. Mapper RAM is optional.
It will work with the standard MSX-DOS 2 command interpreter (command2.com)
and MSX system BIOS/BASIC. The msxdos2.sys system file is not used.

Substantial parts of the kernel are based on:
+ MSX-DOS 2.2 kernel functions by ASCII
+ CP/M 2.2 design principles by Digital Research
+ Commented source code in msxsysrc repository by Arjen Zeilemaker
It is believed that these works are being used in accordance with the intentions
and/or licensing of their creators. 


Enhancements
------------
+ Large disk support (FAT12 / FAT16) with up to 8 partitions and faster 
  execution of DOS functions.
+ If used on MSX2 all RAM mapper segments are available for the user application.
  I.e. on a machine with 128K RAM, 64K RAM (4 segments) will be free.
+ Includes BEER, SODA or JIO driver.


Limitations
-----------
- Not 100% compatible with all MSX-DOS 2 applications.
- MSX-DOS 1 boot and command interpreter are not supported.
- No Kanji support.
- No undelete or disk serial number check.
- No format and ramdisk functions (use external tools).
- Reduced free TPA RAM available for BASIC and disk buffers.


Source modules
--------------
The kernel source code is split into following modules.

Module	Description
SYS	Main entry points / system functions 
IPL	Initial program load 
PAG	Paging helper routines
XIO	BDOS wrapper (replaces msxdos2.sys)
BAS	Disk BASIC
MSG	DOS messages / error texts
RAM	RAM mapper, buffers and memory allocation
DRV	Disk driver
CON	Console functions
DSK	Common disk subroutines
FCB	Disk functions (cp/m or msxdos 1)
FHS	Disk functions (msxdos 2)
TIM	Time/date and timer functions
ENV	Environment item functions

The ROM code is split into two segments:
CS0	0x4000 - 0x7FFF
CS1	0x8000 - 0xBFFF


Disclaimer
----------
CXDOS 2 is provided freely and "as it is" in the hope that it will be useful, 
but without any warranty of any kind, either expressed or implied. 
Use at own risk!
