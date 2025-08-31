; ------------------------------------------------------------------------------
; cxdos.inc
; CXDOS / MSX constants and definitions
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		DEFINE	_CXDOS2_	; prevent double declarations

; CXDOS definitions
; ------------------------------------------------------------------------------

; Settings
DEBUG		equ	$01		; Include debug information / integrity checks
CXDOSVER	equ	$20		; DOS version (should be between $20 and $22)
ROM32K		equ	$01		; size of ROM is 32K / no rom mapper
RAM64K		equ	$01		; size of RAM is 64K / no ram mapper segment for buffers
FASTALLOC	equ	$01		; fast alloc for FAT16 (cutoff free disk space calculation)
DISKBASIC	equ	$01		; Include DISK BASIC

IF CFIDE || PPIDE
	MOD1	equ	'i'	; Ide
	MOD2	equ	' '	; reserved
ENDIF

IFDEF CORE
	MOD1	equ	' '	;
	MOD2	equ	' '	; reserved
ENDIF

IFDEF JIO
	MOD1	equ	'r'	; Remote disk
	MOD2	equ	' '	; reserved
ENDIF

XIOBASE		equ	$D100			; XIO BIOS base address (msxdos2.sys)
XCODE		equ	$6CC			; Size of XIO code
VCODE		equ	702			; Size of paging helper code
VSTB		equ	300			; Size of BDOS stack
VSTI		equ	200			; Size of IRQ stack
VARSIZE		equ	$1000			; Minimum static + dynamic variable space (4KB)
VARDOS		equ	XIOBASE+XCODE+328	; Start of CXDOS code, variables and workarea
PAGCODE		equ	VARDOS+VSTB+VSTI	; Base of paging routines
VARBASE		equ	PAGCODE+VCODE		; Base of variables
DATABA		equ	$F1C9			; MSX DOS workarea (F1C9-F37F 439 bytes)
VARWRK		equ	$F380			; MSX workarea (F380-FFFF 3200 bytes)

; ----------------------------------------------
; ** Workarea RAM P3 **

; Relative variable addresses (134 bytes)
varIY		equ	VARBASE+$06		; Base for relative variable addressing (IY)
varBit16:	equ	varIY-$06		; FAT16	BIT16			bit16-23 for calculation of sector
varDskex:	equ	varIY-$05		; FAT16 DSKEX			bit16-23 for Disk buffer
varSdir1:	equ	varIY-$04		; FAT16	SDIR_1			BBE2h   bit16-23
varSdir2:	equ	varIY-$03		; FAT16	SDIR_2			BBD2h+4 bit16-23
varSdir3:	equ	varIY-$02		; FAT16	SDIR_3			BBC6h+4 bit16-23
varRW16:	equ	varIY-$01		; FAT16	RW_16			bit16-23 for DISKIO
varBB80		equ	varIY+$00		; 	BB80	con		last line pointer in history buffer
varBB82		equ	varIY+$02		; 	BB82	con		current line pointer in history buffer
varBB84		equ	varIY+$04		; 	BB84	con		character counter
varBB86		equ	varIY+$06		; 	BB86	con		saved character counter (?)
varBB87		equ	varIY+$07		; 	BB87	con		saved column (?)
varBB89		equ	varIY+$09		; iy	BB89	con,fhs		standard input/output redirect flags
varBB8A		equ	varIY+$0A		; iy	BB8A	con		echo to printer flag
varBB8B		equ	varIY+$0B		; 	BB8B	con		current column
varBB8D		equ	varIY+$0D		; iy	BB8D	con		stored input (1 character)
varBB8E		equ	varIY+$0E		; 	BB8E	fcb
varBB90		equ	varIY+$10		; iy	BB90	fcb,sys1
varBB91		equ	varIY+$11		; iy	BB91	fcb
varBB92		equ	varIY+$12		; iy	BB92	fcb
varBB95		equ	varIY+$15		; 	BB95	fcb
varBB96		equ	varIY+$16		; 	BB96	fcb
varBB98		equ	varIY+$18		; iy	BB98	fcb
varBB99		equ	varIY+$19		; iy	BB99	dsk
varBB9A		equ	varIY+$1A		; 	BB9A	dsk
varBB9C		equ	varIY+$1C		; 	BB9C	dsk,fhs
varBB9E		equ	varIY+$1E		; 	BB9E	dsk,fhs
						; iy	+$20	dsk
						; iy	+$21	dsk
varBBA2		equ	varIY+$22		; iy	BBA2	dsk
varBBA3		equ	varIY+$23		; 	BBA3	dsk
varBBA5		equ	varIY+$25		; 	BBA5	dsk
varBBA7		equ	varIY+$27		; 	BBA7	dsk
						; iy	+$29	dsk
varBBAA		equ	varIY+$2A		; 	BBAA	dsk
varBBAB		equ	varIY+$2B		; 	BBAB	dsk,fhs
varBBAD		equ	varIY+$2D		; 	BBAD	dsk,fhs
varBBAF		equ	varIY+$2F		; 	BBAF	fhs
varBBB2		equ	varIY+$32		; 	BBB2	dsk
varBBB4		equ	varIY+$34		; iy	BBB4	dsk
						; iy	+$35	dsk
varBBB6		equ	varIY+$36		; 	BBB6	dsk
varBBB8		equ	varIY+$38		; iy	BBB8	dsk
varBBB9		equ	varIY+$39		; 	BBB9	dsk
varBBBB		equ	varIY+$3B		; iy	BBBB	dsk
varBBBC		equ	varIY+$3C		; iy	BBBC	dsk
						; iy	+$3D	dsk
varBBBE		equ	varIY+$3E		; 	BBBE	dsk
						; iy	+$3F	dsk,fcb,fhs
varBBC0		equ	varIY+$40		; 	BBC0	dsk
						; iy	+$41	dsk
varBBC2		equ	varIY+$42		; 	BBC2	dsk
						; iy	+$43	dsk
varBBC4		equ	varIY+$44		; iy	BBC4	dsk
varBBC5		equ	varIY+$45		; 	BBC5	con
varBBC6		equ	varIY+$46		; 	BBC6	fhs
varBBD2		equ	varIY+$52		; 	BBD2	dsk
varBBDE		equ	varIY+$5E		; iy	BBDE	dsk,fhs
varBBDF		equ	varIY+$5F		; iy	BBDF	dsk
varBBE0		equ	varIY+$60		; iy	BBE0	dsk
varBBE1		equ	varIY+$61		; 	BBE1	dsk
varBBE2		equ	varIY+$62		; 	BBE2	fhs
varBBE4		equ	varIY+$64		; 	BBE4	dsk
varBBE6		equ	varIY+$66		; 	BBE6	dsk
varBBE8		equ	varIY+$68		; 	BBE8	dsk,fhs
varBBEA		equ	varIY+$6A		; 	BBEA	dsk
varBBEB		equ	varIY+$6B		; 	BBEB	dsk
;varBBED	equ	varIY+$6D		; 	BBED	env 		obsolete
varEnvPtr	equ	varIY+$6E		; 	BBEE	env		environment variable pointer
varBBF0		equ	varIY+$70		; 	BBF0	sys,fhs
varBBF2		equ	varIY+$72		; 	BBF2	fhs
varBBF4		equ	varIY+$74		; 	BBF4	sys,dsk		device chain
varBBF6		equ	varIY+$76		; 	BBF6	dsk,ram
varBBF8		equ	varIY+$78		; 	BBF8	dsk,ram		disk buffer chain
varBBFA		equ	varIY+$7A		; iy	BBFA	ram		disk buffer counter
varBBFB		equ	varIY+$7B		; 	BBFB	sys1
varBBFD		equ	varIY+$7D		; iy	BBFD	sys
varBBFE		equ	varIY+$7E		; iy	BBFE	sys,ram,fhs
varLazy		equ	varIY+$7F		; iy	+$7F	dsk		if 0 then skip checks

; Console data (622-100=522 bytes)
varCon		equ	VARBASE+$86
;varB06C	equ	varCon+$000		; B06C con	obsolete: screen output buffer (100)
varCon2		equ	varCon-$064
varB0D0		equ	varCon2+$064	 	; B0D0 con	history buffer (256 bytes to B1D0)
varB1CF		equ	varCon2+$163		; B1CF con	end of history buffer
varB1D0		equ	varCon2+$164	  	; B1D0 con	console line input buffer size (255)
varB1D1		equ	varCon2+$165	  	; B1D1 con	console line input buffer, 255 bytes
varB1D2		equ	varCon2+$166		; B1D2 con	position in line input buffer to clear buffer (0)
varBB76		equ	varCon2+$264		; BB76 con
varBB77		equ	varCon2+$265		; BB77 con
varBB78		equ	varCon2+$266		; BB78 con
varBB7A		equ	varCon2+$268		; BB7A con
varBB7B		equ	varCon2+$269		; BB7B con
varBB7C		equ	varCon2+$26A		; BB7C con
varBB7D		equ	varCon2+$26B		; BB7D con
varBB7F		equ	varCon2+$26D		; BB7F con

; Disk data (880 bytes)
varDisk		equ	VARBASE+$86+$20A
varB2D4		equ	varDisk+$000		; B2D4 fcb	cp/m buffer 512 bytes (old: 1024 bytes)
varB6D4		equ	varDisk+$200		; B6D4 dsk	boot sector buffer 48 bytes (old: 512 bytes)
varB8D4		equ	varDisk+$230		; B8D4 fhs	buffer to move directory entry 32 bytes
varB8F4		equ	varDisk+$250		; B8F4 dsk
varB901		equ	varDisk+$25D		; B901 fhs
varB910		equ	varDisk+$26C		; B910 fhs
varB91B		equ	varDisk+$277		; B91B fhs
varB926		equ	varDisk+$282		; B926 dsk,fhs
varB931		equ	varDisk+$28D		; B931 dsk,fhs	whole path buffer 68 bytes
varB975		equ	varDisk+$2D1		; B975 dsk
varB976		equ	varDisk+$2D2		; B976 dsk
varB977		equ	varDisk+$2D3		; B977 fcb
; Not used: B979-B999 (33 bytes)
varB99A		equ	varDisk+$2D5		; B99A fcb	64 bytes
varB9DA		equ	varDisk+$315		; B9DA fcb,fhs	64 bytes
varBA1A		equ	varDisk+$355		; BA1A dsk,fhs
varBA23		equ	varDisk+$35E		; BA23 sys,dsk,ipl

; RAM mapper / dynamic allocatable memory (64 bytes)
varRam		equ	VARBASE+$86+$20A+$370
varRamTab	equ	varRam+$000		; BA35 ram	memory mapper table

; end of static variables
varBottom	equ	varRam+$040		; bottom for dynamic var memory

; ----------------------------------------------
; ** Addresses **

A4010		equ	$4010			; DSKIO
A401F		equ	$401F			; MTOFF
A4043		equ	$4043			; EXTBIO handler DOS2
A4049		equ	$4049			; H.TIMI handler
AC01E		equ	$C01E			; boot loader (retrieved from drive boot sector)
AC206   	equ	$C206			; debugger breakpoint handler
ABE00		equ	$BE00			; boundary for cross-segment sector r/w
AFFFF		equ	$FFFF			; subslot select register

; ----------------------------------------------
; ** Constants **
BELL		equ	$07
BS		equ	$08
TAB		equ	$09
LF		equ	$0A
FF		equ	$0C
CR		equ	$0D
INS		equ	$12
CTRL_Z		equ	$1A
ESC		equ	$1B
SPACE		equ	$20
DEL		equ	$7F
R_SYNCHR	equ	$08
R_CHRGTR	equ	$10
R_OUTDO		equ	$18
R_DCOMPR	equ	$20
R_GETYPR	equ	$28
R_CALLF		equ	$30

; ----------------------------------------------
; *** MSX ***
; ----------------------------------------------

DBUF		equ	$0080			; default disk transfer address
BOT16K		equ	$C000			; lower of 16 KB RAM / boot loader address
BOT32K		equ	$8000			; lower of 32 KB RAM
TMPSTK		equ	BOT16K+$0200		; location of temporary stack
TBASE		equ	$0100			; start of MSX-DOS

;BDOS / BIOS
WBOOT		equ	$0000		; BDOS
KBDOS   	equ	$0005		; BDOS
RDSLT		equ	$000C		; BDOS + BIOS
WRSLT		equ	$0014		; BDOS + BIOS
CALSLT		equ	$001C		; BDOS + BIOS
ENASLT		equ	$0024		; BDOS + BIOS
IDBYT0		equ	$002B		; BIOS
IDBYT2		equ	$002D		; BIOS
CALLF		equ	$0030		; BDOS + BIOS
CHAR_16		equ	$0034		; BIOS
LOAD_FLAG	equ	$0037		; BDOS
KEYINT		equ	$0038		; BDOS + BIOS
SSLOT		equ	$003B		; BDOS
SSLOTL		equ	$0046		; BDOS
SSLOTE		equ	$004B		; BDOS
LDIRVM		equ	$005C		; BIOS
LDIRMV		equ	$0059		; BIOS
CHSNS		equ	$009C		; BIOS
CHGET		equ	$009F		; BIOS
CHPUT		equ	$00A2		; BIOS
LPTOUT		equ	$00A5		; BIOS
LPTSTT		equ	$00A8		; BIOS
BREAKX		equ	$00B7		; BIOS
CKCNTC		equ	$00BD		; BIOS
ERAFNK		equ	$00CC		; BIOS
TOTEXT		equ	$00D2		; BIOS
SNSMAT		equ	$0141		; BIOS
PHYDIO		equ	$0144		; BIOS
KILBUF		equ	$0156		; BIOS
CALBAS		equ	$0159		; BIOS

; DISK
BASENT		equ	$4022		; Start DiskBASIC

; BASIC
DECSUB		equ	$268C		; 
DECADD		equ	$269A		; 
DECDIV		equ	$289F		; 
VMOVE		equ	$2EF3		; 
VMOVFM		equ	$2F08		; 
VMOVMF		equ	$2F10		; 
MAKINT		equ	$2F99		; 
CONDS		equ	$3042		; 
INT		equ	$30D1		; 
SGNMUL		equ	$325C		; 
NULSTR		equ	$3FD6		; 
ERROR		equ	$406F		; BASIC error
READYR		equ	$409B		; restart BASIC
NTSTOP		equ	$4173		; execute statement
LINKER		equ	$4253		; recalculate linepointers
NEWSTT		equ	$4601		; execution loop
CHRGTR		equ	$4666		; CHRGTR
FLTLIN		equ	$46FF		; convert to SNG
INTID2		equ	$4756		; evaluate word operand and check for 0-32767 range
FINPRT		equ	$4AFF		; output back to screen
FRMEQL		equ	$4C5F		; evaluate =expression
FRMEVL		equ	$4C64		; evaluate expression
DOCNVF		equ	$517A		; convert DAC to other type
GETIN2		equ	$520F		; evaluate integer operand
GTBYTC		equ	$521B		; evaluate next byte operand
GETBYT		equ	$521C		; evaluate byte operand
CONINT		equ	$521F		; convert to byte
JPFOUT		equ	$537B		; convert DAC to text, unformatted (JPFOUT is not an offical name)
GETUIN		equ	$542F		; evaluate address operand
GETUI		equ	$5432		; convert address to integer
SCCPTR		equ	$54F7		; convert pointers to linenumbers
GETYPR		equ	$5597		; GETYPR
PTRGET		equ	$5EA4		; get address of variable
STRINI		equ	$6627		; allocate temp string
GETSPA		equ	$668E		; allocate string space
FRESTR		equ	$67D0		; free temporary string
FILEVL		equ	$6A0E		; evaluate filespecification
FILIDX		equ	$6A6D		; get i/o channel pointer
OPNFIL		equ	$6AFA		; open i/o channel
CLSFIL		equ	$6B24		; close i/o channel
CLSALL		equ	$6C1C		; close all i/o channels
NOSKCR		equ	$6E41		; resume character putback routine
BSAVE		equ	$6E92		; start of BSAVE routine
BLOAD		equ	$6EC6		; start of BLOAD routine
BLDFIN		equ	$6EF4		; finish BLOAD
ADRGET		equ	$6F0B		; evaluate address operand (BLOAD/SAVE)
PARDEV		equ	$6F15		; skip strong cassette devicecheck
CRDONZ		equ	$7323		; newline to OUTDO if not at start of line
CRDO		equ	$7328		; newline to OUTDO
M739A		equ	$739A		; quit loading & start (headloop/executing)
M7D17		equ	$7D17		; continue start of MSX-BASIC without executing BASIC programs in ROM
M7D2F		equ	$7D2F		; address initialize BASIC screen
M7D31		equ	$7D31		; BASIC initscreen (without INITXT and CNSDFG)
M7DE9		equ	$7DE9		; start MSX-BASIC program in ROM

; DOS work area (F1C9-F37F)
JUMPB		equ	$F1C9		; start of paging routines jump table (including PRTBUF)
SPRTBUF		equ	$F1C9		; print string in buffer (BDOS 9)
;RD_LDI		equ	$F1D3		; obsolete: transfer to/from page 1
RD_RAM		equ	$F1D3		; read byte from ram underneath disk rom
;P0_LDIR 	equ	$F1D6		; obsolete: transfer with page 0
WR_RAM		equ	$F1D6		; write byte to ram underneath disk rom
;
;P0_CALL 	equ	$F1D9		; obsolete: call main-bios
;SFLUSH  	equ	$F1DC		; obsolete: print string via chput
;GO_DRV  	equ	$F1DF		; obsolete: interslot call with prompt handler
;
JP_VEC		equ	$F1E2		; start DOS1 style handler
SIRQ    	equ	$F1E5		; KEYINT handler
SRDSLT  	equ	$F1E8		; RDSLT handler
SWRSLT  	equ	$F1EB		; WRSLT handler
SCALSLT		equ	$F1EE		; CALSLT handler
SENASLT		equ	$F1F1		; ENASLT handler
SCALLF		equ	$F1F4		; CALLF handler
;
;PUT_BD		equ	$F1F7		; obsolete: enable BDOS segments
;PUT_US		equ	$F1FA		; obsolete: enable DOS segments
;P0_RAM		equ	$F1FD		; obsolete: enable DOS RAM on page 0
;
MAP_VE  	equ	$F200		; RAM mapper jump table vector
ALL_SEG		equ	$F200		; ALL_SEG handler
FRE_SEG		equ	$F203		; FRE_SEG handler
RD_SEG		equ	$F206		; RD_SEG handler
WR_SEG		equ	$F209		; WR_SEG handler
PUT_P0		equ	$F218		; PUT_P0 handler
GET_P0		equ	$F21B		; GET_P0 handler
PUT_P1		equ	$F21E		; PUT_P1 handler
GET_P1		equ	$F221		; GET_P1 handler
PUT_P2		equ	$F224		; PUT_P2 handler
GET_P2		equ	$F227		; GET_P2 handler
PUT_P3		equ	$F22A		; PUT_P3 handler
GET_P3		equ	$F22D		; GET_P3 handler
VARHIMEM	equ	$F230		; CXDOS: highmem for variable memory allocation
VARLOMEM	equ	$F232		; CXDOS: lowmem for variable memory allocation
VARMEMPTR	equ	$F234		; CXDOS: memory block pointer (ipl,ram: B064)
VARMAPPER	equ	$F236		; CXDOS: mapper flags bit 0: ram mapper available
VAR_A		equ	$F237		; CXDOS: Save A register in BDOS dispatcher
; Free: F238-F23B (4 bytes)
CUR_DRV		equ	$F23C		; default (current) drive, 0=default, 1=A, 2=B, etc.
DTA_AD		equ	$F23D		; default transfer address
; Free: F23F-F24E (16 bytes)
HOOKBE		equ	$F24F		; 35 hooks of 3 bytes (105 bytes, not used by cxdos)
H_PROM  	equ	$F24F		; prompt for disk hook (used by DOS1 driver)
; Free: F252-F27B (41 bytes)
H_MUL1		equ	$F27C		; multiply hook (used by DOS1 driver)
H_DIV3		equ	$F27F		; divide hook (used by DOS1 driver)
; Free: F282-F2B7 (53 bytes)
TIM_RA  	equ	$F2B8		; number of VBLANK interrupts per 1/10 second
TIM_CO  	equ	$F2B9		;
RANDOM  	equ	$F2BA		; 'random' number
;
ST_COU  	equ	$F2BD		;
CH_COU  	equ	$F2BE		;
TIM_TI  	equ	$F2BF		;
TIMI_S		equ	$F2C0		; 5 bytes
MAP_TAB 	equ	$F2C5		; memory mapper table
P0_SEG		equ	$F2C7		; current segment page 0
P1_SEG		equ	$F2C8		; current segment page 1
P2_SEG		equ	$F2C9		; current segment page 2
P3_SEG		equ	$F2CA		; current segment page 3
P0_TPA		equ	$F2CB		; segment page 0 at BDOS entry
P1_TPA		equ	$F2CC		; segment page 1 at BDOS entry
P2_TPA		equ	$F2CD		; segment page 2 at BDOS entry
;DATA_S		equ	$F2CF		; obsolete: BDOS data segment
;CODE_S		equ	$F2D0		; obsolete: BDOS code segment
SP_IRQ  	equ	$F2D1		; temporary save stackpointer (KEYINT)
IRQ_ST		equ	$F2D3		; pointer to temporary stack (KEYINT)
FCALSA 		equ	$F2D5		; 5 bytes
;SERR_M  	equ	$F2DA		; obsolete: vector for get error message routine
SPROMPT 	equ	$F2DC		; pointer to prompt hander
SBDOS   	equ	$F2DE		;
SS_TEMP		equ	$F2E0		;
;
IX_BDOS 	equ	$F2E6		; temporary save IX register BDOS call
SP_BDOS 	equ	$F2E8		;
;PS_BDOS 	equ	$F2EA		; obsolete: same as F2EB, but for primary slots
;SS_BDOS 	equ	$F2EB		; obsolete: secundary slot status when BDOS function handler was executed
DSK_CHK 	equ	$F2EC		;
BLDCHK		equ	$F2F1		;
BSVCHK		equ	$F2F3		;
BUF_1		equ	$F2F5		; pointer to 100 bytes buffer
BUF_2		equ	$F2F7		; pointer to  64 bytes buffer
BUF_3		equ	$F2F9		; pointer to 100 bytes buffer
ERR_BUF		equ	$F2FB		; pointer to  64 bytes buffer
BOOT_D  	equ	$F2FD		;
ST_BDOS 	equ	$F2FE		; pointer to temporary stack (BDOS)
KDSK_V  	equ	$F300		;
KAB_VE		equ	$F302		;
; Free: F304-F30C (9 bytes)
RAWFLG  	equ	$F30D		; read after write (verify) flag
COUNTR		equ	$F30E		;
;KANJTA  	equ	$F30F		; obsolete: double byte header table (4 bytes)
DOSVER		equ	$F313		;
P0_64K  	equ	$F314		;
P1_64K  	equ	$F315		;
P2_64K  	equ	$F316		;
P3_64K  	equ	$F317		; (not used)
; Free: F318-F322 (11 bytes)
DISKVE		equ	$F323		;
BREAKV		equ	$F325		;
AUXBOD		equ	$F327		; 10 bytes
SDOS1   	equ	$F333		;
TIMFLG		equ	$F338		;
PATHNAM		equ	$F33B		;
RECSIZE 	equ	$F33D		;
TARGET		equ	$F33F		;
NOTFIR		equ	$F340		;
RAMAD0  	equ	$F341		;
RAMAD1		equ	$F342		;
RAMAD2		equ	$F343		;
RAMAD3		equ	$F344		;
DOSFLG		equ	$F346		;
SNUMDR		equ	$F347		;
MASTER		equ	$F348		;
HIMSAV		equ	$F349		;
DOSHIM  	equ	$F34B		;
SSECBUF		equ	$F34D		;
SMAXSEC		equ	$F34F		;
SDIRBUF		equ	$F351		;
SDPBLI		equ	$F355		; 16 bytes: drive parameter block (max 8 drives)
RSLREG		equ	$F365		; "in a,($a8) ; ret"
SDOSON		equ	$F368		; page in disk rom
SDOSOF		equ	$F36B		; page out disk rom
XFER		equ	$F36E		; transfer data under disk rom page 1
SAUXIN		equ	$F371		;
SAUXOUT		equ	$F374		;
GO_BIOS		equ	$F377		; call routine in BDOS code segment
GO_BDOS		equ	$F37A		; call BDOS handler
BDOS		equ	$F37D		; call BDOS from disk BASIC


; BIOS/BASIC work area (F380-FFFF)
RDPRIM		equ	$F380
WRPRIM		equ	$F385
RETRTN		equ	$F38B		; on this adres is a simple RET instruction
CLPRIM		equ	$F38C
CLPRM1		equ	$F398		; "jp (ix)" also used as "call CLPRM1+1" = "call (hl)"
LINLEN		equ	$F3B0
CRTCNT  	equ	$F3B1
CNSDFG		equ	$F3DE
REPCNT  	equ	$F3F7		; time interval until key-repeat
LPTPOS		equ	$F415
PRTFLG		equ	$F416
CURLIN		equ	$F41C
KBUF    	equ	$F41F
BUF		equ	$F55E
TTYPOS		equ	$F661
VALTYP		equ	$F663
MEMSIZ		equ	$F672
STKTOP		equ	$F674
TXTTAB		equ	$F676
TEMPPT		equ	$F678
DSCTMP		equ	$F698
FRETOP		equ	$F69B
AUTLIN		equ	$F6AB
SAVSTK		equ	$F6B1
VARTAB		equ	$F6C2
STREND		equ	$F6C6
DAC		equ	$F7F6
ARG		equ	$F847
MAXFIL		equ	$F85F
FILTAB		equ	$F860
NULBUF		equ	$F862
PTRFIL		equ	$F864
FILNAM		equ	$F866
NLONLY		equ	$F87C
SAVEND		equ	$F87D
HOKVLD		equ	$FB20
DRVTBL		equ	$FB21
HOOKSA		equ	$FB29
BASROM		equ	$FBB1
BOTTOM		equ	$FC48
HIMEM		equ	$FC4A
INTFLG  	equ	$FC9B
FLBMEM		equ	$FCAE
RUNBNF		equ	$FCBE
SAVENT		equ	$FCBF
EXPTBL		equ	$FCC1
SLTTBL		equ	$FCC5
SLTWRK		equ	$FD09
PROCNM		equ	$FD89
DISKID		equ	$FD99
H_TIMI		equ	$FD9F
H_DSKO		equ	$FDEF
H_NAME		equ	$FDF9
H_KILL		equ	$FDFE
H_COPY		equ	$FE08
H_DSKF		equ	$FE12
H_DSKI		equ	$FE17
H_LSET		equ	$FE21
H_RSET		equ	$FE26
H_FIEL		equ	$FE2B
H_MKIS		equ	$FE30
H_MKSS		equ	$FE35
H_MKDS		equ	$FE3A
H_CVI		equ	$FE3F
H_CVS		equ	$FE44
H_CVD		equ	$FE49
H_GETP		equ	$FE4E
H_NOFO		equ	$FE58
H_NULO		equ	$FE5D
H_NTFL		equ	$FE62
H_BINS		equ	$FE71
H_BINL		equ	$FE76
H_FILE		equ	$FE7B
H_DGET		equ	$FE80
H_FILO		equ	$FE85
H_INDS		equ	$FE8A
H_LOC		equ	$FE99
H_LOF		equ	$FE9E
H_EOF		equ	$FEA3
H_BAKU		equ	$FEAD
H_PARD		equ	$FEB2
H_NODE		equ	$FEB7
H_POSD		equ	$FEBC
H_RUNC		equ	$FECB
H_CLEA		equ	$FED0
H_LOPD		equ	$FED5
H_ERRP		equ	$FEFD
H_STKE		equ	$FEDA
H_PHYD		equ	$FFA7
EXTBIO		equ	$FFCA

; ------------------------------------------------------------------------------
; Byte constants

_NCOMP	equ	$FF	; incompatible disk
_WRERR	equ	$FE	; write error
_DISK	equ	$FD	; disk error
_NRDY	equ	$FC	; not ready
_VERFY	equ	$FB	; verify error
_DATA	equ	$FA	; data error
_RNF	equ	$F9	; sector not found
_WPROT	equ	$F8	; write protected disk
_UFORM	equ	$F7	; unformatted disk
_NDOS	equ	$F6	; not a DOS disk
_SEEK	equ	$F3	; seek error
_IFAT	equ	$F2	; bad file allocation table
_NOUPB	equ	$F1	;

_INTER	equ	$DF	; internal error
_NORAM	equ	$DE	; not enough memory
_IBDOS	equ	$DC	; invalid BDOS call
_IDRV	equ	$DB	; invalid drive
_IFNM	equ	$DA	; invalid filename
_IPATH	equ	$D9	; invalid pathname
_PLONG	equ	$D8	; pathname too long
_NOFIL	equ	$D7	; file not found
_NODIR	equ	$D6	; directory not found
_DRFUL	equ	$D5	; boot directory full
_DKFUL	equ	$D4	; disk full
_DUPF	equ	$D3	; duplicate filename
_DIRE	equ	$D2	; invalid directory move
_FILRO	equ	$D1	; read only file
_DIRNE	equ	$D0	; directory not empty
_IATTR	equ	$CF	; invalid attributes
_DOT	equ	$CE	; invalid . or .. operation
_SYSX	equ	$CD	; system file exists
_DIRX	equ	$CC	; directory exists
_FILEX	equ	$CB	; file exists
_FOPEN	equ	$CA	; file is already in use
_OV64K	equ	$C9	; cannot transfer above 64K
_FILE	equ	$C8	; file allocation error
_EOF	equ	$C7	; end of file
_ACCV	equ	$C6	; file access violation
_IPROC	equ	$C5	; invalid process id
_NHAND	equ	$C4	; no spare file handles
_IHAND	equ	$C3	; invalid file handle
_NOPEN	equ	$C2	; file handle not open
_IDEV	equ	$C1	; invalid device operation
_IENV	equ	$C0	; invalid environment string
_ELONG	equ	$BF	; environment string too long
_IDATE	equ	$BE	; invalid date
_ITIME	equ	$BD	; invalid time
_RAMDX	equ	$BC	; ram disk already exists
_NRAMD	equ	$BB	; ram disk does not exist
_HDEAD	equ	$BA	; file handle has been deleted
_EOL	equ	$B9	; end of line
_ISBFN	equ	$B8	; invalid sub-function number

_STOP	equ	$9F
_CTRLC	equ	$9E
_ABORT	equ	$9D
_OUTERR equ	$9C
_INERR	equ	$9B

_BADCM	equ	$8E
_OKCMD	equ	$8C
_IPARM	equ	$8B
_BATEND	equ	$80

; DOS Functions
FTERM0		equ	$00
FCONIN		equ	$01
FCONOUT		equ	$02
FAUXIN		equ	$03
FAUXOUT		equ	$04
FLSTOUT		equ	$05
FDIRIO		equ	$06
FDIRIN		equ	$07
FINNOE		equ	$08
FSTROUT		equ	$09
FBUFIN		equ	$0A
FCONST		equ	$0B
FCPMVER		equ	$0C
FDSKRST		equ	$0D
FSELDSK		equ	$0E
FFOPEN		equ	$0F
FFCLOSE		equ	$10
FSFIRST		equ	$11
FSNEXT		equ	$12
FFDEL		equ	$13
FRDSEQ		equ	$14
FWRSEQ		equ	$15
FFMAKE		equ	$16
FFREN		equ	$17
FLOGIN		equ	$18
FCURDRV		equ	$19
FSETDTA		equ	$1A
FALLOC		equ	$1B
FRDRND		equ	$21
FWRRND		equ	$22
FFSIZE		equ	$23
FSETRND		equ	$24
FWRBLK		equ	$26
FRDBLK		equ	$27
FWRZER		equ	$28
FGDATE		equ	$2A
FSDATE		equ	$2B
FGTIME		equ	$2C
FSTIME		equ	$2D
FVERIFY		equ	$2E
FRDABS 		equ	$2F
FWRABS		equ	$30
FDPARM		equ	$31
FFFIRST		equ	$40
FFNEXT		equ	$41
FFNEW		equ	$42
FOPEN		equ	$43
FCREATE		equ	$44
FCLOSE		equ	$45
FENSURE		equ	$46
FDUP		equ	$47
FREAD		equ	$48
FWRITE		equ	$49
FSEEK		equ	$4A
FIOCTL		equ	$4B
FHTEST		equ	$4C
FDELETE		equ	$4D
FRENAME		equ	$4E
FMOVE		equ	$4F
FATTR		equ	$50
FFTIME		equ	$51
FHDELETE	equ	$52
FHRENAME	equ	$53
FHMOVE		equ	$54
FHATTR		equ	$55
FHFTIME		equ	$56
FGETDTA		equ	$57
FGETVFY		equ	$58
FGETCD		equ	$59
FCHDIR		equ	$5A
FPARSE		equ	$5B
FPFILE		equ	$5C
FCHKCHR		equ	$5D
FWPATH		equ	$5E
FFLUSH		equ	$5F
FFORK		equ	$60
FJOIN		equ	$61
FTERM		equ	$62
FDEFAB		equ	$63
FDEFER		equ	$64
FERROR		equ	$65
FEXPLAIN	equ	$66
FFORMAT		equ	$67
FRAMD		equ	$68
FBUFFER		equ	$69
FASSIGN		equ	$6A
FGENV		equ	$6B
FSENV		equ	$6C
FFENV		equ	$6D
FDSKCHK		equ	$6E
FDOSVER		equ	$6F
FREDIR		equ	$70

; ------------------------------------------------------------------------------
; Structures

; File Info Block  (64 bytes)
fib_id		equ	$00	; FIB indicator (always $ff)
fib_fname	equ	$01	; filename (in ASCIIZ format)
fib_attrib	equ	$0E	; file attributes
fib_modtime	equ	$0F	; last modified time
fib_moddate	equ	$11	; last modified date
fib_clstart	equ	$13	; start cluster
fib_fsize	equ	$15	; file size
fib_drive	equ	$19	; logical drive number
;fib_volume	equ	$1A	; volume id (if file on disk) not used in cxdos2
fib_device	equ	$1A	; device descriptor (if device)
fib_devjp	equ	$1C	; pointer to device (if device)
fib_mode	equ	$1E	; device flag (bit 7)
fib_search	equ	$1F	; search attributes
; 
fib_exdir	equ	$32	; FAT16 extended sector number (?)
fib_sdir	equ	$38	; FAT16 sdir (?)

; File Access Block internal variables
fab_dub		equ	$20	; pointer to drive unit block
fab_dirsec	equ	$22	; current directory sector
fab_dirent	equ	$24	; current directory entry in sector
fab_parentcl	equ	$25	; cluster number of parent directory
fab_startcl	equ	$27	; file start cluster
fab_abscl	equ	$29	; current file absolute cluster
fab_logcl	equ	$2B	; current file logical cluster
fab_file	equ	$2D	; current file pointer
fab_access	equ	$31	; access mode
fab_segtype	equ	$32	; segment type for temporary paging (obsolete)

; File Control Block (36 bytes)
fcb_dr		equ	$00	; drive number (0=default)
fcb_fname	equ	$01	; filename
fcb_fext	equ	$09	; filetype (filename extension)
fcb_ex		equ	$0C	; extent (low byte)
fcb_s1		equ	$0D	; extent (high byte)
fcb_s2		equ	$0E	;
fcb_rc		equ	$0F	; record count
fcb_filsiz	equ	$10	; file size
fcb_volume	equ	$14	; volume
fcb_dircl	equ	$18	; first directory cluster
fcb_clstart	equ	$1A	; first physical cluster
fcb_abscl	equ	$1C	; current absolute cluster
fcb_logcl	equ	$1E	; current logical cluster
fcb_cr		equ	$20	; record in extent
fcb_random	equ	$21	; random access record

; Drive Unit Block (96 bytes)
dub_slot	equ	$00	; slot address of the disk driver
dub_offset	equ	$01	; offset to entry table of disk driver
dub_dpb		equ	$02	; pointer to DPB of drive
dub_flags1	equ	$04	; flags
dub_flags2	equ	$05	; flags
dub_drive	equ	$06	; drive id used by the disk driver
dub_ram		equ	$07	; ram segment for ram based driver (not used)
dub_unit	equ	$08	; physical unit number
dub_time	equ	$09	; 1/10th seconds+2 that disk cannot be changed
dub_clmask	equ	$0A	; cluster mask
dub_clshift	equ	$0B	; cluster shift
dub_reserved	equ	$0C	; reserved sectors
dub_nfat	equ	$0E	; number of FAT's
dub_dirodd	equ	$0F	; remainder of directory entries (not whole sector)
dub_dirsec	equ	$10	; number of directory sectors (whole sectors)
dub_fatsec	equ	$11	; number of sectors per FAT
dub_rootsec	equ	$12	; first root directory sector
dub_datasec	equ	$14	; first data sector
dub_ncluster	equ	$16	; total number of clusters + 1 on disk
;dub_dirty	equ	$18	; dirty disk flag not used in cxdos2
;dub_volume	equ	$19	; volume id not used in cxdos2
dub_media	equ	$1D	; media descriptor
dub_dircl	equ	$1E	; first cluster of current directory
dub_dirpath	equ	$20	; current directory path (without "drive:\" prefix, 64 bytes)


; Device

; Disk Buffer

; Directory Entry





