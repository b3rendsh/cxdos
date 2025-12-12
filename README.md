# cxdos
msx compact extended dos

## Introduction

This repository contains alternatives for the MSX DOS 1 and 2 disk ROM's.

## CXDOS 1

RAM loadable DOS, the kernel functions are compatible with MSX DOS 1.  
This DOS version can be loaded from MSX-DOS (diskette) or from BASIC (tape) and includes a driver for either the JIO, SODA or BEER IDE interface.  
[![CXDOS 1 JIO](cxdos1/pictures/cxdos_1_jio_small.png)](cxdos1/pictures/cxdos_1_jio.png)
  
**Enhancements**  
Large disk support (FAT12 / FAT16) with up to 8 partitions and faster execution of DOS functions.
  
**Limitations**
- BASIC ROM not used / BASIC not available
- No FORMAT command
- No msxdos.sys, the sys code is part of the kernel at a fixed address
- Any other disk interfaces will be de-activated on start
- The free TPA memory is 48KB

**Use cases**
- Learning and experimentation
- Test / entry level disk system for MSX with JIO server 
- DIY small Z80 (CP/M) retrocomputers without MSX BASIC ROM
- Reflash disk ROM from MSX

[**Binaries**](cxdos1/bin)  
The .bin files can be loaded from tape with the msx2cas android app.  
The .com files can be started from the MSX-DOS command prompt.  

## CXDOS 2

**Demo only**  
DOS for MSX1 and newer machines with limited ROM or RAM resources, the kernel functions are compatible with MSX-DOS 2.
A demo version is built that will run on a MSX1 with 32K disk ROM and 64K system RAM.

## Contributions
Please create an issue if you want to report a bug or have any requests or suggestions. Pull requests from forks are currently not supported.

This is a low priority experimental project and it may take a while before requests are reviewed and/or implemented.

## License
The Creative Commons license terms apply to all material, with the exception of code that is based on existing work by others. The origins of these parts are mentioned in the source code. It is believed that these works are being used in accordance with the intentions and/or licensing of their creators. 

The material is shared here for educational and non commercial purposes only. It is provided freely and "as it is" in the hope that it will be useful, but without any warranty of any kind, either expressed or implied. Use at own risk!



