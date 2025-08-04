; ------------------------------------------------------------------------------
; main.asm
; CXDOS 1 main entry point
;
; MSX RAM loadable compact extended DOS created by H.J. Berends.
; Substantial parts of the kernel are based on:
; + MSX-DOS 1.03 kernel functions by ASCII
; + FAT swapper and other MSX-DOS 1 enhancements by SOLiD
; + CP/M 2.2 design principles by Digital Research
; + Commented source code in msxsysrc repository by Arjen Zeilemaker
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------
; CXDOS is compatible with all kernel functions of MSX-DOS 1, excluding:
; - Disk BASIC (CXDOS works without BASIC ROM)
; - Format command
; - Secret message (saves 97 bytes)
; The paging helper routines are DOS2-ish.
; There is only one active disk system included with the kernel, optional
; other disk interfaces in the machine will be de-activated at launch.
; msxdos.sys is replaced by kernel code, loaded at a fixed address in page 3.
; ------------------------------------------------------------------------------

		INCLUDE	"cxdos1.inc"

		SECTION	MAIN			; Main entry point / initial program load
	IFDEF BASBIN
		ORG	BOT32K-7		; 0x8000 - 7 bytes for BIN header
	ELSE
		ORG	TBASE			; 0x0100: Start of TPA
	ENDIF
		SECTION	DISK			; Kernel and driver functions
		SECTION	SYS			; DOS handler (msxdos.sys replacement)

; ------------------------------------------------------------------------------

		SECTION MAIN

		; Used by kernel
		PUBLIC	X0046

		; Defined by kernel
		EXTERN	DosBegin
		EXTERN	DosSize
		EXTERN	DosMultiply
		EXTERN	DosConout
		EXTERN	DosValFCB
		EXTERN	DosGetSlotP3
		EXTERN	DosGetSlotP2
		EXTERN	DosDiskIO
		EXTERN	DosInvalidFbuf
		EXTERN	ClockRunMode
		EXTERN	PatchTPA
		EXTERN	PH_SIRQ
		EXTERN	PH_XFER
		EXTERN	PH_DOSON
		EXTERN	PH_DOSOF
		EXTERN	PH_RDSLT
		EXTERN	PH_WRSLT
		EXTERN	PH_CALLF
		EXTERN	PH_CALSLT
		EXTERN	PH_ENASLT
	IFDEF MAXENT
		EXTERN	PH_STROUT
	ENDIF

		; Defined by driver
		EXTERN	INIHRD			; Initialize hardware
		EXTERN	DRIVES			; Return number of drives in system
		EXTERN	INIENV			; Initialize work area
		EXTERN	OEMSTA			; Used for system expansion (OEMSTATEMENT)
		EXTERN	MYSIZE			; Size of the page-3 RAM work area required by the driver in bytes.
		EXTERN	SECLEN			; Maximum sector size for media supported by this driver (512).
		EXTERN	DEFDPB			; Base address of a 21 byte "default" DPB for this driver.

		; Defined by sys
		EXTERN	SysBoot			; Boot msxdos.sys
		EXTERN	SysEnd
		EXTERN	SysSize

; ------------------------------------------------------------------------------
; *** Header ***
; ------------------------------------------------------------------------------

	IFDEF BASBIN
		db	$fe			; ID
		dw	BOT32K			; Start address
		dw	SysEnd			; End address
		dw	StartIpl		; Execution address
	ELSE
		jp	BootMain

		; identification
		db	CR,LF,"MSX CXDOS V1.0",CR,LF
	IFDEF PPIDE
		db	"for BEER interface",CR,LF
	ELIFDEF CFIDE
		db	"for SODA interface",CR,LF
	ELIFDEF	JIO
		db	"for JIO interface",CR,LF
	ENDIF
	IFDEF DEBUG
		db	"DEBUG version",CR,LF
	ENDIF
		db	CTRL_Z

BootMain:	di
		; disable MSX-DOS IRQ routine
		; some BIOS routines enable interrupts, so 'di' instruction is not enough
		ld	a,$c9
		ld	(KEYINT),a

		; Copy application code to page 2 and start ipl
		ld	hl,IplBegin
		ld	bc,IplSize
		ld	de,BOT32K
		ldir
		jp	StartIpl
	ENDIF

; ------------------------------------------------------------------------------
; *** Initialization ***
; ------------------------------------------------------------------------------

; Some IPL checks in MSX-DOS 1 and call to driver INIHRD are intentionally left out.
; There's no point of return to DOS or BASIC. The system will halt on kernel panic.

IplBegin:
		PHASE	BOT32K

; Initial program loader, CXDOS is the one (master) disk system

StartIpl:
		; enable RAM on page 1
		ld	a,(RAMAD1)
		ld	h,$40
		call	ENASLT

		; disable IRQ routine in MSX-DOS 2 code segment
		ld	a,(DRVTBL)
		or	a
		jr	z,r001			; z=no disk system active
		ld	a,(DOSVER)
		cp	$22			; MSX-DOS 2.2 or higher ?
		jr	c,r001			; c=no
		call	GET_P1
		push	af
		ld	a,(CODE_S)
		call	PUT_P1
		ld	a,$c9
		ld	(KEYINT+$4000),a
		pop	af
		call	PUT_P1

		; enable BIOS on page 0
r001:		ld	a,(EXPTBL+0)
		ld	h,$00
		call	ENASLT

		; initialize screen settings and display loader message
		xor	a
		ld	(CSRSW),a		; cursor off
		call	ERAFNK			; erase function key display
		call	DspMsg
		db	CR,LF,"Loading CXDOS 1 ...",CR,LF,0

		; disable all H.TIMI handlers to prevent disruptions
		; First call H_TIMI 256 times to motor off floppy drives for interfaces that have a counter
		; for it in their H.TIMI handler. Alternative (disk rom function 0x4029) to send MTOFF
		; to ALL active disk interfaces may not work as expected.
		di
		xor	a
mtcount:	call	H_TIMI			; H.TIMI handler saves register AF
		dec	a
		jr	nz,mtcount
		ld	hl,H_TIMI
		ld	bc,$05c9		; b=5 bytes, c=ret instruction
		call	FillTable
		ldir

	IFDEF DEBUG
		; validate size of msxdos.sys code
		ld	hl,SYSMAX
		ld	de,SysSize
		call	Compare
		ld	b,'1'
		jp	c,HaltSystem		; c=actual code size is larger than reserved size

		; validate size of kernel routines
		ld	hl,DOSMAX
		ld	de,DosSize
		call	Compare
		ld	b,'2'
		jp	c,HaltSystem		; c=actual code size is larger than reserved size
	ENDIF

		ld	sp,TMPSTK		; set stack below msxdos area in page 3
		ld	hl,VARWRK
		ld	(HIMEM),hl		; set DOS himem to start of BIOS/BASIC workarea

		ld	hl,VARWRK+MYSIZE
		ld	de,DATABA
		and	a
		sbc	hl,de			; bytes needed for static workarea+workarea driver
	IFDEF DEBUG
		ld	b,'3'
		jp	c,HaltSystem		; c=driver workarea way too big, panic
	ENDIF
		call	AllocMemHL		; allocate memory
		push	hl			; save base for disk driver workarea data

		; clear page 3 cxdos workarea (sys to himem)
		ld	hl,SYSBASE
		ld	bc,VARWRK-SYSBASE
		call	ClearArea

		; copy helper routines
		ld	hl,DosData
		ld	de,DATABA
		ld	bc,DataSize
		ldir

		; set driver workarea
		pop	hl
		ld	(MYWORK),hl

		; copy kernel code to ram page 3
		ld	hl,DosCode
		ld	de,DOSBASE
		ld	bc,DosSize
		ldir

		; clear DRVTBL, HOOKSA
		ld	hl,DRVTBL
		ld	bc,$1400		; b=4*2+4*3 c=clear
		call	FillTable

		; initialize HOOKBE
		ld	hl,HOOKBE
		ld	bc,$69c9		; b=$69 c=ret
		call	FillTable

		; initialize RSLREG subroutine
		ld	hl,RSLREG
		ld	(hl),$db		; $db $a8 = in a,($a8)
		inc	hl
		ld	(hl),$a8
		inc	hl
		ld	(hl),$c9		; $c9 = ret

		; Initialize interface / determine available drives.
		; Leave DRVTBL empty in case an application wants to call driver routines in a disk rom
		; at address 0x401X then it won't detect this interface with the driver routines in RAM.
		xor	a			; default no phantom drives
		call	DRIVES			; query no. of drives (this routine will enable interrupts)
		add	a,l
		cp	8+1			; more than 8 drives?
		jr	c,r002			; c=no
		ld	a,8			; limit to 8
r002:		ld	(SNUMDR),a		; drives in system (only 1 interface supported)
		ld	hl,SDPBLI
		push	hl
		push	af
		ld	b,0
		ld	c,a
	IFDEF MAXENT
		ld	de,23
	ELSE
		ld	de,21
	ENDIF
		call	DosMultiply		; bc*de = drives * size of DPB
		call	AllocMemBC		; reserve number of bytes for the DPBs
		ex	de,hl
		pop	af
		pop	hl
r003:	  	ld	(hl),e
		inc	hl
		ld	(hl),d			; save in DPBTBL
		inc	hl
		push	hl
		ld	hl,DEFDPB
	IFDEF MAXENT
		ld	bc,23
	ELSE
		ld	bc,21
	ENDIF
		ldir				; initialize DPB
		pop	hl
		dec	a
		jr	nz,r003			; next drive
		call	INIENV			; initialize hardware driver workarea

		; initialize double byte header char table
		ld	hl,CHAR_16
		ld	de,KANJTA
		ld	bc,4
		ldir

		; localization and console initialization
		ld	a,(IDBYT0)
		rrca
		rrca
		rrca
		rrca
		and	$07
		ld	(COUNTR),a		; date format
		ld	a,$ff
		ld	(BUFDRN),a		; invalid datasector buffer
		ld	(DIRBFD),a		; invalid directorysector buffer
		ld      (DAYCNT+1),a		; invalid days since 1-1-1980
		ld	a,$0d
		ld	(YCONBF+130),a		; ?? end marker con buffer
		ld	a,7
		ld	(FILMAX),a		; max number of FCBs is 7
		ld	hl,$4035		; number of days between 1-1-1980 and 1-1-2025
		ld	(CURDAT),hl		; set default date when no clockchip

		; initialize character i/o
		ld	hl,AUXBOD
		ld	(hl),$3e		; ld a,"EOF" for AUX input
		inc	hl
		ld	(hl),$1a
		ld	b,2*5-2
r006:	  	inc	hl
		ld	(hl),$c9
		djnz	r006

		; initialize DOS jump table
		ld	hl,JumpTable
		ld	de,SDOSON
		ld	bc,8*3
		ldir

		; initialize buffers
		ld	bc,SECLEN		; fixed sector size (512 bytes)
		call	AllocMemBC
		ld	(SSECBUF),hl		; allocate sectorbuffer
		call	AllocMemBC
		ld	(SBUFFE),hl		; allocate datasector buffer

		; Allocate only 1.5K memory space for a 3 sector FAT buffer, for all drives.
		; If the FAT is bigger than 3 sectors, the needed sector within the FAT will be swapped in from disk.
		call	AllocMemBC
		ld	(SDIRBU),hl		; allocate dirsector buffer
		ld	h,b			; hl,bc = size of biggest sector
		ld	l,c
		add	hl,hl
		add	hl,bc
		inc	hl			; (size of biggest sector: 512) * 3 + 1
		call	AllocMemHL
		inc 	hl
		ld	(FATSWAP4),hl		; FAT common buffer pointer

		; initialize DPB for each drive
		ld	a,(SNUMDR)		; drives in system
		ld	b,a			; b=number of drives
		ld	c,0			; c=drive 0 (A:)
		ld	hl,SDPBLI
r005:	  	ld	e,(hl)
		inc	hl
		ld	d,(hl)			; DPB of drive
		inc	hl
		push	hl
		push	de
		pop	ix
		ld	(ix+0),c		; set drivenumber in DPB
		ld	hl,(FATSWAP4)		; FAT common buffer
		ld	(ix+19),l
		ld	(ix+20),h
		inc	c
		pop	hl
		djnz	r005

		; check for and initialize clockchip
		call	InitRTC

		; if quit anywhere after this point then panic / halt system
		ld	hl,HaltSystem
		push	hl

		; initialize RAM pages
		ld	a,(EXPTBL+0)
		ld	(RAMAD0),a
		ld	(RAMAD1),a		; assume no ram available for page 0 and 1
		call	DosGetSlotP2
		ld	(RAMAD2),a		; slotid of current page 2
		call	DosGetSlotP3
		ld	(RAMAD3),a		; slotid of current page 3
		ld	c,$00
		call	SearchRAM		; search ram in page 0
		ret	c			; c=not found, halt system
		ld	(RAMAD0),a
	  	ld	c,$40
		call	SearchRAM		; search ram in page 1
		ret	c			; c=not found, halt system
		ld	(RAMAD1),a

		ld	hl,(BOTTOM)
		ld	de,BOT32K
		call	Compare			; check if ram on both page 3 and 2
		ret	nz			; nz=no, halt system
		ld	hl,RAMAD0
		ld	a,(EXPTBL+0)
		cp	(hl)
		ret	z			; no ram available on page 0, halt system
		inc	hl
		cp	(hl)
		ret	z			; no ram available on page 1, halt system

		; DOS memory requirements are met, try starting it
		xor	a			; drive 0
		call	DosInvalidFbuf		; invalidate FAT buffer of the drive
		ld	(DOSFLG),a		; flag bootable disk

		; Set page 0 to RAM and initialize scratch area
		ld	a,(RAMAD0)
		ld	h,$00
		call	PH_ENASLT		; (this routine will disable interrupts)

		; clear scratch area
		xor	a
		ld	l,a
		ld	h,a
r007:	  	ld	(hl),a
		inc	l
		jr	nz,r007			; clear 0000-00FF

		; set jump routines in scratch area
		ld	a,$c3
		;
		ld	hl,PH_RDSLT
		ld	(RDSLT),a
		ld	(RDSLT+1),hl
		;
		ld	hl,PH_WRSLT
		ld	(WRSLT),a
		ld	(WRSLT+1),hl
		;
		ld	hl,PH_CALLF
		ld	(CALLF),a
		ld	(CALLF+1),hl
		;
		ld	hl,PH_CALSLT
		ld	(CALSLT),a
		ld	(CALSLT+1),hl
		;
		ld	hl,PH_ENASLT
		ld	(ENASLT),a
		ld	(ENASLT+1),hl

		; install slotswitching helper routines
		ld	hl,X003B
		ld	de,SSLOT
		ld	bc,$001a
		LDIR

		; patch slotswitching routine
		ld	hl,SSLOTL
		ld	(PatchTPA+1),hl

		; enable DOS interrupt routine
		ld	hl,PH_SIRQ
		ld	(KEYINT+0),a		; $c3
		ld	(KEYINT+1),hl		; KEYINT
		ld	hl,ENDBUF
		ld	(IRQ_ST),hl		; re-use the BASIC screen character buffer for the DOS interrupt stack

		; update memory pointers
		ld	hl,(HIMEM)
		ld	(HIMSAV),hl
		ld	(DOSHIM),hl

; ------------------------------------------------------------------------------
; *** Boot msxdos.sys ***
; ------------------------------------------------------------------------------

StartSys:	; load msxdos.sys code
		ld	hl,SysCode
		ld	de,SYSBASE
		ld	bc,SysSize
		ldir

		; display signon message
		ld	de,Signon
		call	SPRTBUF

		; set error vectors
		ld	hl,SYSBASE+$09
		ld	(DISKVE),hl
		ld	hl,SYSBASE+$0B
		ld	(BREAKV),hl

		; set cold boot flag
		ld	hl,NOTFIR
		ld	a,(hl)
		ld	(hl),h			; next time not a cold boot
		ld	(SYSBASE+$1a),a		; initialize cold boot flag

		; set page 1 to ram (DOSOF)
		ld	a,(RAMAD1)
		ld	h,$40
		call	ENASLT

		; cold boot CXDOS
		jp	SysBoot

; CXDOS information
Signon:		db	CR,LF,"CXDOS version 1.0",CR,LF,"$"

; ------------------------------------------------------------------------------
; *** Subroutines ***
; ------------------------------------------------------------------------------

; Subroutine check for and initialize clockchip
InitRTC:  	ld	a,13
		out	($b4),a
		ld	a,$0a
		out	($b5),a			; alarm off, clock running, bank 2
		xor	a
		out	($b4),a			; pos 0
		ld	b,$0f
r011:  		in	a,($b5)			; read data
		and	$0f
		xor	b
		out	($b5),a			; change it and write back
		ld	c,a
		nop				; wait (?)
		in	a,($b5)
		and	$0f
		cp	c			; correctly read back ?
		ret	nz			; nz=no, no clockchip!
		xor	b
		out	($b5),a			; restore orginal data
		djnz	r011			; try all values
		ld	a,$ff
		ld	(TIMFLG),a		; flag use clockchip
		ld	a,13
		out	($b4),a
		ld	a,$09
		out	($b5),a			; alarm off, clock running, bank 1
		ld	a,10
		out	($b4),a			; pos 10
		ld	a,1
		out	($b5),a			; 24 hour system
		ld	a,13
		out	($b4),a
		xor	a
		out	($b5),a			; alarm off, clock paused, bank 0
		ld	bc,$0D00
r012:  		ld	a,c
		out	($b4),a
		in	a,($b5)
		push	af
		inc	c
		djnz	r012			; save time registers
		ld	a,14
		out	($b4),a
		xor	a
		out	($b5),a			; clear testbits
		ld	b,00DH
r013:  		dec	c
		ld	a,c
		out	($b4),a
		pop	af
		out	($b5),a
		djnz	r013			; restore time registers
		jp	ClockRunMode		; put clock in running mode

; ------------------------------------------------------------------------------

; Subroutine find RAM
SearchRAM:  	ld	hl,EXPTBL
		ld	b,4
		xor	a
r021:  		and	$03
		or	(hl)
r022:  		push	bc
		push	hl
		ld	h,c
r023:  		ld	l,$10
r024:  		push	af
		call	RDSLT
		cpl
		ld	e,a
		pop	af
		push	de
		push	af
		call	WRSLT
		pop	af
		pop	de
		push	af
		push	de
		call	RDSLT
		pop	bc
		ld	b,a
		ld	a,c
		cpl
		ld	e,a
		pop	af
		push	af
		push	bc
		call	WRSLT
		pop	bc
		ld	a,c
		cp	b
		jr	nz,r026
		pop	af
		dec	l
		jr	nz,r024
		inc	h
		inc	h
		inc	h
		inc	h
		ld	c,a
		ld	a,h
		cp	$40
		jr	z,r025
		cp	$80
		ld	a,c
		jr	nz,r023
r025:  		ld	a,c
		pop	hl
		pop	hl
		ret

r026:		pop	af
		pop	hl
		pop	bc
		and	a
		jp	p,r027
		add	a,4
		cp	$90
		jr	c,r022
r027:		inc	hl
		inc	a
		djnz	r021
		scf
		ret

; ------------------------------------------------------------------------------

; Subroutine allocate RAM memory (halt when error)
; Input: BC,HL = number of bytes to allocate
AllocMemHL:	ld	b,h
		ld	c,l
AllocMemBC:	ld	hl,(HIMEM)
		and	a
		sbc	hl,bc
		ld	(HIMEM),hl		; new top of MSXDOS
		jr	c,HaltSystem		; c=below zero, halt system
		ld	a,h
		cp	DOSFREE / 256		; below bottom of free variable memory for DOS?
		ret	nc

HaltSystem:  	call	DspMsg
		db	CR,LF
		db	"Kernel panic, system halted",0
		di
		halt

; ------------------------------------------------------------------------------
; Display message (duplicated as PrintMsg in driver)

DspMsg:		ex	(sp),hl
		call	DspString
		ex	(sp),hl
		ret

DspString:  	ld	a,(hl)
		inc	hl
		and	a
		ret	z
		rst	$18			; print character
		jr	DspString

; ------------------------------------------------------------------------------

; Subroutine: clear area / fill area
; Input:  hl = start of area
;	  bc = number of bytes
;	  a  = filler (fill area)
ClearArea:	xor	a
FillArea:	ld	(hl),a
		ld	d,h
		ld	e,l
		inc	de
		dec	bc
		ldir
		ret

; Subroutine: fill table
; Input: hl = start of area
;	  b  = number of bytes
;	  c  = filler
FillTable:	ld	(hl),c
		inc	hl
		djnz	FillTable
		ret

; Subroutine compare hl with de (DCOMPR)
; Input:  hl,de = values to compare
; Output: zx set if hl=de, cx set if hl<de
Compare:	ld	a,h
		sub	d
		ret	nz
		ld	a,l
		sub	e
		ret

; ------------------------------------------------------------------------------
; *** DOS Helper routines ***
; ------------------------------------------------------------------------------

; Subroutines store and change secondary slotregister (relocatable)

X003B:  	out	($a8),a
		ld	a,(AFFFF)
		cpl
		ld	l,a
		and	h
		or	d
		jr	X004E

X0046:  	out	($a8),a
		ld	a,l
		jr	X004E

X004B:  	out	($a8),a
		ld	a,e
X004E:  	ld	(AFFFF),a
		ld	a,b
		out	($a8),a
		ret

; ------------------------------------------------------------------------------
; *** Include initialization part of disk interface driver ***
; ------------------------------------------------------------------------------

		DEFINE	DRV_IPL
	IF PPIDE || CFIDE
		INCLUDE	"../driver/driver.asm"
	ELIF JIO
		INCLUDE	"../driver/drv_jio.asm"
	ENDIF

; ------------------------------------------------------------------------------
; *** DOS page 3 jump table ***
; ------------------------------------------------------------------------------

JumpTable:	jp	PH_DOSON
		jp	PH_DOSOF
		jp	PH_XFER
		jp	AUXBOD+0
		jp	AUXBOD+5
		db	$c9,$00,$00		; BLOAD not used
		db	$c9,$00,$00		; BSAVE not used
		jp	BDOSBO

; ------------------------------------------------------------------------------
; *** CXDOS static data ***
; ------------------------------------------------------------------------------
; CXDOS data, DOS 1 routines/variables are retained for compatibility.
; No DOSOF/DOSON required, the free space is re-used for variables.

DosData:
		DEPHASE

DataBegin:
		PHASE	DATABA

; Function $09 STROUT
; String output
; Input:  de = address of string
AF1C9:	 	
	IFDEF MAXENT
		jp	PH_STROUT
AF1CC:		dw	0			; LASTEN16
AF1CE:		dw	0			; ENTFRE16
AF1D0:		dw	0			; SRCHLO16
AF1D2:		db	0			; reserved for variable
	ELSE
		ld	a,(de)			; SPRTBUF
		inc	de
		cp	'$'
		ret	z
		call	DosConout
		jr	AF1C9
	ENDIF

AF1D3:		dw	0			; MYWORK
AF1D5:		dw	0			; SP_IRQ
AF1D7:		dw	0			; IRQ_ST

; Subroutine move block, same as XFER for driver (obsolete).
AF1D9:		ldir				; BLKMOV
		ret

AF1DC:		db	0			; SS_TEMP
AF1DD:		dw	0			; F16HISEC
AF1DF:		dw	0			; F16LOSEC
AF1E1:		db	0			; reserved for variables

; Subroutine Warm Boot (obsolete)
AF1E2:	 	jp	WBOOT			; ENDJMP

AF1E5:		defs	3,0			; reserved for variables

; Subroutine start handler in DOS memory
; Input:  HL = address of pointer
AF1E8:   	ld	e,(hl)			; JPHL
		inc	hl
		ld	d,(hl)
		ex	de,hl
		jp	(hl)

AF1ED:		dw	0			; FATSWAP1
AF1EF:		db	$ff			; FATSWAP2 (initial value $ff)
AF1F0:		dw	0			; FATSWAP3
AF1F2:		dw	0			; FATSWAP4

; Subroutine validate FCB filename (obsolete)
; Input:  HL = address of pointer
AF1F4:		jp	DosValFCB		; LODNAM

; Data table with reserved filenames (devicenames)
; Remark: is copied to 0F1F7H
AF1F7:	 	db	"PRN "			; IONAME
		db	"LST "
		db	"NUL "
		db	"AUX "
		db	"CON "

; Data: fake direntry for devices
AF20B: 		defs	11,SPACE		; DEVDIR
		db	10000000b
		defs	10
		dw	0
		dw	0
		dw	0
		dw	0,0

; Month table (MONTAB)
AF22B:		db	31,28,31,30,31,30,31,31,30,31,30,31

		DEPHASE

DataSize	EQU	$-DataBegin

; ------------------------------------------------------------------------------
; Assemble modules in following order:
; main		initialization code
; kernel	kernel code
; sys		msxdos.sys

DosCode		EQU	DosData+DataSize
SysCode		EQU	DosCode+DosSize
IplSize		EQU	($-IplBegin)+DosSize+SysSize
