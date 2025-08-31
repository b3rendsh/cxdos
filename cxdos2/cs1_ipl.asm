; ------------------------------------------------------------------------------
; cs1_ipl.asm
; Initialization routines.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION CS1_IPL

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	iplBootMain		; init handler
		PUBLIC	iplDiskBasic		; start Disk BASIC
		PUBLIC	iplBootCxdos		; start CXDOS from boot drive

		; sys
		EXTERN	K_INIT
		EXTERN	GETWRK
		EXTERN	sysWrkSlot
		EXTERN	sysGetSlotP1
		EXTERN	sysGetSlotP2
		EXTERN	sysGetSlotP3
		EXTERN	msgGetMessage
		EXTERN	PROMPT
		EXTERN	sysPrintChar
		EXTERN	sysCRLF
		EXTERN	sysPrintStrZ
		EXTERN	C4DBB
		EXTERN	sysBdos
		EXTERN	sysBdos1

		; bas
		EXTERN	basTxtBasic
		EXTERN	basDiskBasic
		EXTERN	basVecDisk
		EXTERN	basVecIgnore
		EXTERN	basVecBreak
		EXTERN	basBload
		EXTERN	basBsave
		EXTERN	I6568

		; pag
		EXTERN	pagPatch
		EXTERN	pagBegin
		EXTERN	pagTable
		EXTERN	pagVectors

		; ram
		EXTERN	ramInitMapDos
		EXTERN	ramInitMapTab
		EXTERN	InitRamMapper
		EXTERN	ramAllocMem

		; xio
		EXTERN	xioBegin
		EXTERN	xioEnd

		; driver
		EXTERN	INIHRD
		EXTERN	DRIVES
		EXTERN	INIENV
		EXTERN	MYSIZE
	IF PPIDE || CFIDE
		EXTERN	BOOTMENU
	ENDIF


; ------------------------------------------------------------------------------
; *** CXDOS initial program load ***
; ------------------------------------------------------------------------------
iplBootMain:
	IFDEF DEBUG
		; validate size of cxdos i/o code
		ld	hl,XCODE
		ld	de,xioEnd-xioBegin
		rst	R_DCOMPR
		ld	b,'1'
		jp	c,HaltSystem		; c=actual code size is larger than reserved size

		; validate size of paging helper routines
		ld	hl,VCODE
		ld	de,pagTable-pagBegin
		rst	R_DCOMPR
		ld	b,'2'
		jp	c,HaltSystem		; c=actual code size is larger than reserved size
	ENDIF

		call	INIHRD
		di
		ld	a,(DISKID)
		or	a			; disk system initialization canceled?
		ret	m			; m=yes
		jr	nz,_iplNotFirst		; this is not the first disk interface

		; ** first disk interface **

		ld	hl,HOKVLD
		bit	0,(hl)			; EXTBIO initialized?
		jr	nz,r001			; nz=yes
		set	0,(hl)			; set to initialized

		; initialize hooks: EXTBIO, DISINT and ENAINT
		ld	hl,EXTBIO
		ld	bc,$0fc9		; b=3x5 c=ret
		call	FillTable

		; is there at least 16KB RAM in the machine?
r001:		ld	hl,(BOTTOM)
		ld	de,$c001
		rst	R_DCOMPR
		jr	nc,_iplCancel

		; is HIMEM lowered by the extension?
		ld	hl,(HIMEM)
		ld	de,VARWRK
		rst	R_DCOMPR
		jr	nz,_iplCancel

		; is the shift key pressed?
		ld	a,$06
		call	SNSMAT
		di
		rrca				; bit 0 is shift
		jr	c,r002			; c=no, continue

		; beep to indicate that shift is pressed and disk init is canceled
		ld	a,BELL
		rst	R_OUTDO

		; cancel disk initialization
_iplCancel:	ld	a,$ff
		ld	(DISKID),a
		ret

		; continue disk initialization

r002:		call	SetSystemRam		; set system RAM to slot with memory mapper (if available)

		; allocate and clear CXDOS static workarea in page 3
		ld	hl,VARWRK-DATABA
		call	AllocMem
		ret	c			; c=memory allocation failed
		ld	bc,VARWRK-DATABA
		call	ClearArea

		; set biggest sector to 0
		ld	(AUTLIN),bc		

		; clear DRVTBL and HOOKSA
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

		; is the CTRL key pressed?
		ld	a,$06
		call	SNSMAT
		di
		and	$02
		ld	(TARGET),a		; save CTRL key status
		
		; beep
		ld	a,$07
		rst	R_OUTDO
		jr	r003

		; ** not the first disk interface **

_iplNotFirst:	ld	a,(DOSVER)
		cp	CXDOSVER		; current master disk interface is version 2.00 or higher?
		jr	nc,r004

		; ** set disk interface as master disk **

		; set system RAM to slot with memory mapper (if available)
		call	SetSystemRam

r003:		; set master disk DOS version
		ld	a,CXDOSVER
		ld	(DOSVER),a	

		; set H.RUNC to regain control after BASIC is started
		call	sysGetSlotP2
		ld	hl,H_RUNC
		ld	de,iplResume
		call	SetHook

		; ** common disk initialization code ***

r004:		call	GetDrives		; get currently detected drives
		ret	z			; z=no more room for new drives

		; allocate workarea for driver
		ld	hl,MYSIZE
		call	AllocMem
		jr	c,_iplExit		; c=out of memory, increase disk interface count if first disk interface and quit
		ex	de,hl
		call	sysWrkSlot		; get pointer to SLTWRK entry
		ld	(hl),e
		inc	hl
		ld	(hl),d

		; set biggest sector to 512 i.e fixed value in CXDOS
		ld	bc,$0200
		ld	(AUTLIN),bc

		; update DRVTBL with drives/slotid of disk interface
		call	GetDrives		; get currently detected drives (again)
		ld	hl,DRVTBL
		ld	d,$00
		add	hl,de
		add	hl,de
		ex	de,hl			; pointer to free DRVTBL entry
		ld	a,(TARGET)
		or	a			; nz=CTRL key pressed
		ld	a,c			; save current number of drives
		call	DRIVES 			; get number of partitions / drives on disk (DRIVES)
		add	a,l			; update number of drives
		cp	$09			; more than 8 drives?
		jr	c,r005
		ld	a,$08
r005:		sub	c			; set maximum drives for disk interface
		jr	z,_iplExit		; z= no drives for disk interface, increase disk interface if first disk interface and quit
		ld	(de),a			; set drives in DRVTBL entry
		inc	de
		call	sysGetSlotP1
		ld	(de),a			; set slot id in DRVTBL entry

		; Initialize DPB's
		; todo: analyse data structure
		ld      b,0
		ld      hl,SDPBLI
		add     hl,bc
		add     hl,bc
		push    hl			; store pointer to drive parameter block entry of first drive of disk interface
		dec     de
		ld      a,(de)			; number of drives interface
		push    af			; store number of drives interface
		ld      c,a
		add     a,a
		add     a,a
		add     a,c
		add     a,a
		add     a,a
		add     a,c
		ld      l,a
		ld      h,b			; *21 (size of drive parameter block)
		call    AllocHalt		; allocate memory (adjust BASIC areapointers, halt when error)
		ex      de,hl
		pop     af			; restore number of drives interface
		pop     hl			; restore pointer to drive parameter block entry
next_drive:	ld      (hl),e
		inc     hl
		ld      (hl),d
		inc     hl			; update pointer to drive parameter block, next drive parameter block entry
		push    hl			; store pointer to drive parameter block entry
		ld      hl,0			; default drive parameter block disk driver (DEFDPB)
		ld      bc,21
		ldir    			; initialize drive parameter block
		pop     hl			; restore pointer to drive parameter block entry
		dec     a
		jr      nz,next_drive		; next drive

		; initialize driver workarea environment
		call	INIENV

		; increase disk interface count
		ld	hl,DISKID
		inc	(hl)

		; We're done with the first phase of the IPL before BASIC initialization,
		; if this is the master disk interface then IPL will be resumed with the iplResume routine.
		ret

		; Increase disk interface count if first disk interface and quit
_iplExit:	ld	hl,DISKID
		ld	a,(hl)
		or	a			; first disk interface?
		ret	nz			; nz=no
		inc	(hl)			; increase disk interface count
		ret

; ------------------------------------------------------------------------------
; *** Resume IPL if this is the master disk interface ***
; Note: only disk ROM page 2 is set at this point
; ------------------------------------------------------------------------------
iplResume:	; clear H.RUNC hook
		ld	hl,H_RUNC
		ld	bc,$05c9
		call	FillTable

		; check and clear DISKID, if disk system initialization is canceled then exit
		ld	hl,DISKID
		ld	a,(hl)
		ld	(hl),$00
		or	a			; initizalize disk system canceled?
		ret	m			; m=yes
		ld	d,a			; save disk interface count

		; update number of drives
		call	GetDrives		; get all detected drives
		ld	(SNUMDR),a

		; validate integrity of drive table
		ld	a,d
		sub	e			; disk interface count same as interface count of DRVTBL?
		jr	z,r006			; z=yes
		dec	a			; 1 extra ?
		ld	b,'3'
		jp	nz,HaltSystem		; nz=no, halt system

		; remove disk driver interrupt handler of MASTER disk interface
		ld	de,HOOKSA
		ld	hl,HOOKSA+3
		ld	bc,4*3
		ldir

r006:		; update master disk system slot id and activate disk ROM page 1
		call	sysGetSlotP2
		ld	(MASTER),A
		ld	h,$40
		call	ENASLT

		; update data format
		ld	a,(IDBYT0)
		rrca
		rrca
		rrca
		rrca
		and	$07
		ld	(COUNTR),A

		; initialize AUX variables
		ld	hl,AUXBOD
		ld	(hl),$3e
		inc	hl
		ld	(hl),$1a
		inc	hl
		ld	bc,$08c9
		call	FillTable

		; initialize DOS vectors
		ld	hl,PROMPT
		ld	(SPROMPT),hl		; address of prompt subroutine (not used)
		ld	hl,sysBdos
		ld	(SBDOS),hl		; address of BDOS subroutine
		ld	hl,sysBdos1
		ld	(SDOS1),hl		; address of DOS1 BDOS subroutine

		; todo: detect clockchip
		ld	a,$FF
		ld	(TIMFLG),a

		; set max sector size and allocate memory for sector buffer
		ld	hl,$0200
		ld	(SMAXSEC),hl
		inc	hl			; add 1 byte for FAT buffer dirty flag
		call	AllocHalt
		ld	(hl),$00		; FAT buffer is clean
		inc	hl
		ld	(SSECBUF),hl		; update pointer to sector buffer

		; todo: investigate usage of DPB's
		ld	hl,SDPBLI		; drive parameter block entries
		ld	bc,8*256 + 0		; 8 drives, drive id = 0
_dpb1:		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; get pointer to drive parameter block
		inc	hl
		ld	a,e
		or	d			; drive has pointer to drive parameter block (valid drive) ?
		jr	z,_dpb2			; nope, skip
		ex	de,hl
		ld	(hl),c			; update drive id drive parameter block
		push	bc			; store entry counter, drive id
		ld	bc,19
		add	hl,bc			; offset 19, pointer to fat buffer
		ld	bc,(SSECBUF)
		ld	(hl),c
		inc	hl
		ld	(hl),b			; update pointer to fat buffer with pointer to sector buffer
		pop	bc			; restore entry counter, drive id
		ex	de,hl
_dpb2:		inc	c			; next drive id
		djnz	_dpb1			; next drive

		; install disk system subroutines
		call	InitDiskSystem
		jp	c,HaltSystem		; c=error, halt system

		; allocate memory for buffers
		ld	hl,100+64+100+64
		call	AllocHalt
		ld	(BUF_1),hl
		ld	b,0			; b=0 used 3 times in additions below
		ld	c,100
		add	hl,bc			; (1)
		ld	(BUF_2),hl
		ld	c,64
		add	hl,bc			; (2)
		ld	(BUF_3),hl
		ld	c,100
		add	hl,bc			; (3)
		ld	(ERR_BUF),hl

		; register HIMEM for disk system environment
		ld	hl,(HIMEM)
		ld	(HIMSAV),hl

		; install interrupt handler
		ld	hl,H_TIMI
		ld	de,TIMI_S
		call	SaveHook
		ld	de,A4049		; interrupt handler DOS
		call	PatchHook

		; install EXTBIO handler
		ld	hl,EXTBIO
		ld	de,FCALSA
		call	SaveHook
		ld	de,A4043		; EXTBIO handler DOS
		call	PatchHook

		; get address of BASIC screen initialization
		; and run BASIC screen initialization
		; optimize: investigate the BASIC code how this works
		ld	hl,M7D2F+1
		ld	a,(EXPTBL)
		call	RDSLT
		push	af
		inc	sp
		dec	hl
		ld	a,(EXPTBL)
		call	RDSLT
		push	af
		inc	sp
		pop	ix
		ld	iy,(EXPTBL-1)
		call	CALSLT

		; patch H.LOPD if H.CLEAR is patched
		; to regain control of BASIC initialization from extension
		call	PatchBas

		; start BASIC?
		ld	sp,TMPSTK		; switch to temporary stack
		ld	a,(H_STKE)
		cp	$c9			; extension has patched H_STKE to take control of BASIC initialization ?
		ld	ix,M7D17
		jr	nz,r007			; nz=yes
		ld	a,(BASROM)
		or	a			; about to start BASIC program in ROM?
		ld	ix,M7DE9
		jr	z,r008			; z=no

		; start DiskBASIC
r007:		call	InitDiskBasic		; initialize DiskBASIC
		ld	hl,CALBAS
		push	hl
		jp	basDiskBasic

r008:		call	DisableDosP1		; disable DOS page 1 support
		call    GetBootLoader		; search for first drive with valid boot loader
		call	nz,StartBootLdr		; nz=found, execute boot loader with Cx=0

	IF CFIDE || PPIDE
		call	BOOTMENU		; boot menu which sets current drive
		jp	c,iplDiskBasic		; if c-flag is set then start DiskBASIC
	ELSE
		ld	a,(CUR_DRV)		; current drive
	ENDIF
		ld	hl,AutoBasNo		; empty command line
		jr	StartCxdos		; start CXDOS

StartBootLdr:	ld	hl,DISKVE		; address BDOS diskerror handler pointer
		ld	de,SDOSON		; enable DOS kernel subroutine
		ld	a,(NOTFIR)		; cold boot flag
		call    $0c01e			; start boot loader (if it returns then continue with ipl cxdos )
		call	UpdateDosP1
		scf
		jp	$0c01e


; ------------------------------------------------------------------------------
; *** Start CXDOS from boot drive ***
; ------------------------------------------------------------------------------
iplBootCxdos:	push	hl
		ld	a,(MASTER)		; activate disk ROM page 1,
		ld	h,$40			; SDOSON is disabled at this point,
		call	SENASLT			; use page 3 ENASLT routine!
		pop	hl
		ld	a,(BOOT_D)		; boot drive

StartCxdos:	ld	sp,TMPSTK		; switch to temporary stack
		push	hl
		ld	hl,iplDiskBasic
		ex	(sp),hl			; start DiskBASIC if booting cxdos fails
		push	af			; save drive id
		ld	a,$ff
		ld	(DOSFLG),a		; CXDOS environment = enabled
		pop	af			; restore drive id

		; prepare and start CXDOS
		ld	b,a			; store boot drive id
		ld	a,(CUR_DRV)		; current drive
		push	af			; store default drive
		push	bc			; store boot drive
		push	hl			; store pointer to command line
		ld	hl,(HIMSAV)
		ld	(DOSHIM),hl		; register top of DOS memory
		di

		; select TPA RAM segments
		ld	a,(VARMAPPER)
		bit 	0,a			; Use RAM Mapper?
		jr	z,_end64kseg
		ld	a,(P0_64K)		; page 0 segment disk system
		call	PUT_P0
		ld	a,(P1_64K)		; page 1 segment disk system
		call	PUT_P1
		ld	a,(P2_64K)		; page 2 segment disk system
		call	PUT_P2
_end64kseg:
		; switch page 0 to disk system RAM slot
	        ld      a,(RAMAD0)
	        ld      h,$00
	        call    SENASLT			; use page 3 ENASLT routine!

		ld	hl,WBOOT
_cxdos1:	ld	(hl),h
		inc	l
		jr	nz,_cxdos1		; clear cp/m low storage page
		ld	hl,_cxdosjp		; table with msxdos jump entry points
		ld	bc,_cxdosn*256+$ff	; b=number of jump entries c=decremented by ldi
		ld	a,$c3
_cxdos2:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	(de),a
		inc	de
		ldi
		ldi
		djnz	_cxdos2

_cxdos3:	; copy SSLOT routines to page 0
		ld	hl,SslotCode
		ld	de,SSLOT
		ld	bc,SslotSize
		ldir

		pop	hl			; restore pointer to command line
		ld	de,DBUF+1
		ld	b,-1
_cxdos4:	ld	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		inc	b
		or	a
		jr	nz,_cxdos4		; copy command line to dbuf
		ld	a,b
		ld	(DBUF+0),a		; update command line size
		pop	af			; restore boot drive
		or	a
		call	nz,_cxdos5		; boot drive specified, start cxdos
		pop	af			; restore current drive

_cxdos5:	ld	(CUR_DRV),a		; update current drive
		ld	hl,basVecDisk		; on BDOS disk error warm boot (start DiskBASIC)
		ld	(DISKVE),hl		; install BDOS disk error handler
		ld	hl,basVecIgnore		; ignore BDOS abort
		ld	(BREAKV),hl		; install BDOS abort handler

		ld	b,0			;
		ld	d,b			; d=b=0: cancel all assignments
		ld	c,FASSIGN		; function logical drive assignment
		call    sysBdos1		; direct BDOS call, ignore errors

		ld	a,$c3			; enable DOS page 1 support
		call	UpdateDosP1
		ld	a,(CUR_DRV)		; current drive
		ld	(BOOT_D),a		; update boot drive

		; load cxdos i/o code (replaces msxdos2.sys)
		ld	hl,xioBegin
		ld	de,XIOBASE
		ld	bc,xioEnd-xioBegin
		ldir

		; signon message
		ld	a,(NOTFIR)		; print sign-on message if this is the first boot
		ld	de,Signon
		or	a
		call	z,sysPrintStrZ

		; continue cold boot XIO BIOS
		jp	XIOBASE+$100

_cxdosjp:	dw	RDSLT,SRDSLT
		dw	WRSLT,SWRSLT
		dw	CALSLT,SCALSLT
		dw	ENASLT,SENASLT
		dw	CALLF,SCALLF
		dw	KEYINT,SIRQ
_cxdosn		equ	6			; number of jump entries


; SSLOT code, relocatable to page 0
SslotCode:	out	($a8),a			; SSLOT
		ld	a,(AFFFF)
		cpl
		ld	l,a
		and	h
		or	d
		jr	$+10
		out	($a8),a			; SSLOTL
		ld	a,l
		jr	$+5
		out	($a8),a			; SSLOTE
		ld	a,e
		ld	(AFFFF),a
		ld	a,b
		out	($a8),a
		ret
SslotSize:	equ	$-SslotCode

	IFDEF DEBUG
Signon:		db	"CXDOS version 2.00",MOD1,MOD2," (TEST)",CR,LF,LF,0
	ELSE
Signon:		db	"CXDOS version 2.00",MOD1,MOD2,CR,LF,LF,0
	ENDIF
AutoBasYes:	db	"RUN\"\\AUTOEXEC.BAS"
AutoBasNo:	db	0

; BDOS disk error handler: start DiskBASIC without running BASIC program file
VecDiskError:	dw	iplBasCore		; start DiskBASIC without running BASIC program file

; ------------------------------------------------------------------------------
; *** Start DiskBASIC ***
; ------------------------------------------------------------------------------
iplDiskBasic:	ld	sp,TMPSTK		; switch to temporary stack
		call	DisableDosP1		; disable DOS page 1 support
		ld	hl,AutoBasYes
		ld	de,BUF+12
		ld	bc,18
		ldir				; prepare executing AUTOEXEC.BAS
		ld	hl,NOTFIR
		ld	a,(HL)
		or	a			; cold start ?
		ld	(hl),h			; next start is warm start
		jr	nz,iplBasWarm		; nz=warm start
		ld	(DOSFLG),a		; CXDOS environment = disabled
		ld	hl,VecDiskError		; start DiskBASIC without running BASIC program file
		ld	(DISKVE),hl		; install BDOS disk error handler
		ld	de,BUF+16		; AUTOEXEC.BAS
		ld	a,1			; open mode = no write
		ld	c,FOPEN			; open file handle
		call	BDOS
		jr	nz,iplBasCore		; nz=error, start DiskBASIC without running BASIC program file
		ld	c,FCLOSE		; close file handle
		call	BDOS
		jr	iplBasProg		; start DiskBASIC

; Warm start DiskBASIC
iplBasWarm:	ld	a,(WBOOT)
		cp	$c3			; start DiskBASIC from CXDOS?
		jr      nz,iplBasCore		; nz=no, start DiskBASIC without running BASIC program file

		; get size of command line
		ld      hl,DBUF
		ld	b,(HL)
		inc	b
		dec	b			; command line empty?
		jr	z,iplBasCore		; z=yes, start DiskBASIC without running BASIC program file

		; skip white spaces
r009:		inc	hl
		ld	a,(hl)
		cp	TAB
		jr	z,r010
		cp	SPACE
		jr	nz,r011			; nope, command line has a BASIC program file name
r010:		djnz    r009			; next character
iplBasCore:	xor	a
		ld	(BUF+12+3),a		; simple RUN statement
		jr	iplBasProg

		; copy filename to cmd buffer and start DiskBASIC running the specified BASIC program
r011:		ld	c,b
		xor	a
		ld	b,a
		ld	de,BUF+12+4
		ldir
		ld      (de),a			; add end of line

iplBasProg:	ld	sp,TMPSTK
		ld	a,(EXPTBL+0)
		ld	h,$00
		call	ENASLT			; switch page 0 to MAIN ROM
		call	InitDiskBasic		; initialize DiskBASIC
r012:		ld	bc,$00*256+FJOIN	; rejoin parent process
		call	BDOS
		jr	nz,r012			; nz=error, retry
		ld	hl,(BOTTOM)		; BASIC bottom
		ld	e,$00
		ld	a,(RAMAD2)
		call	WRSLT			; make sure bottom start with BASIC end of line token
		inc	hl
		ld	(TXTTAB),hl		; start of BASIC program
		ld	a,(RAMAD2)
		call	WRSLT
		inc	hl
		ld	a,(RAMAD2)
		call	WRSLT			; pointer to next line = 0 (end of program)
		inc	hl
		ld	(VARTAB),hl		; start of BASIC variables
		ld	hl,-1
		ld	(CURLIN),hl		; BASIC interpreter in direct mode
		ld	sp,(STKTOP)		; switch to BASIC stack
		ld	a,$ff
		ld	(CNSDFG),a		; function keys = enabled
		ld	a,FF
		call	sysPrintChar		; clear screen (for MSX1 BASIC)
		ld	ix,M7D31
		call	CALBAS			; BASIC welcome message
		call    sysCRLF
		ld	de,basTxtBasic		; DiskBASIC version string
		call	sysPrintStrZ
		call	sysCRLF
		ld	hl,NTSTOP
		push	hl			; execute RUN command
		ld	hl,BUF+11
		push	hl			; BASIC pointer
		ld	hl,BUF+10
		push	hl			; restore BASIC pointer routine
		ld	(hl),$E1
		inc	hl
		ld	(hl),$C9
		jp	basDiskBasic

; ------------------------------------------------------------------------------

; Subroutine: initialize DiskBASIC
InitDiskBasic:	ld	hl,(HIMSAV)
		ld	(HIMEM),HL		; restore HIMEM from registered disk system HIMEM
		
		; select system RAM segments
		ld	a,(VARMAPPER)
		bit 	0,a			; Use RAM Mapper?
		jr	z,_endsysseg
		ld	a,(P0_64K)
		call	PUT_P0
		ld	a,(P1_64K)
		call	PUT_P1
		ld	a,(P2_64K)
		call	PUT_P2
_endsysseg:
		ld	hl,basVecDisk
		ld	(DISKVE),hl		; install BDOS disk error handler
		ld	hl,basVecBreak
		ld	(BREAKV),hl		; install BDOS abort handler
		ld	bc,(SMAXSEC)		; biggest sector size
		call	AllocHimem		; buffer must be in page 3
		ld	(SDIRBUF),hl		; pointer to directory buffer
		ld	(PATHNAM),hl		; pointer to path name buffer

		; initialize BLOAD/BSAVE code
		ld	bc,13
		call	AllocHimem
		ld	(BLDCHK),hl
		ex	de,hl
		ld	hl,CodeBloadBsave
		ldir
		ld	hl,-5
		add	hl,de
		ld	(BSVCHK),hl
		ld	a,(RAMAD1)		; old:MASTER
		ld	hl,-12
		add	hl,de
		ld	(hl),a
		ld	hl,-4
		add	hl,de
		ld	(hl),a

		; allocate i/o channels (system + user)
		call	AllocIO

; Subroutine: disable DOS page 1 support
DisableDosP1:	ld	a,$c9

; Subroutine: update DOS page 1 support
UpdateDosP1:	ld	(SDOSON),a
		ld	(SDOSOF),a
		; do not disable xfer, it is used by disk basic and bdos
		; ld	(XFER),a
		ret

; Subroutine: allocate memory (adjust HIMEM, halt when error)
AllocHimem:	ld	hl,(HIMEM)
		or	a
		sbc	hl,bc
		ld	(HIMEM),hl
		jr	c,HaltSystem
		ld	a,h
		cp	$C2
		jr	_alloc1

; Subroutine: allocate memory, adjust BASIC areapointers and halt if error
AllocHalt:	call	AllocMem		; allocate memory (adjust BASIC areapointers)
_alloc1:	ret	nc

; Subroutine: halt system
HaltSystem:	ld	a,FF
		call	sysPrintChar		; character to screen
		ld	a,1			; message = 1
		ld	de,BUF
		call	msgGetMessage		; copy message to buffer
		call	sysPrintStrZ
		ld	a,b
		call	sysPrintChar		; halt error number
		di
		halt				; guru meditation

; Bload/Bsave code
CodeBloadBsave:	rst	R_CALLF
		db	$00
		dw	basBload
		push	hl
		jp	BLDFIN
		rst	R_CALLF
		db	$00
		dw	basBsave
		ret

; ------------------------------------------------------------------------------
; Subroutine get valid boot loader

GetBootLoader:	LD      HL,basVecDisk		; on BDOS disk error warm boot (start DiskBASIC)
		LD      (DISKVE),HL		; install BDOS disk error handler
		LD      HL,basVecIgnore		; ignore BDOS abort
		LD      (BREAKV),HL		; install BDOS abort handler
		LD      DE,(SSECBUF)
		LD      C,FSETDTA		; function set disk transfer address
		CALL    sysBdos1
		LD      C,1			; drive id = 1
		LD      DE,DRVTBL
J6964:		PUSH    BC			; store drive id
		PUSH    DE			; store pointer in DRVTBL
		LD      L,C
		DEC     L			; to drive id
		LD      H,1			; number of sectors = 1
		LD      DE,0			; sector number = 0
		LD      C,FRDABS		; function absolute sector read
		CALL    sysBdos1
		POP     DE			; restore pointer in DRVTBL
		POP     BC			; restore drive id
		JR      NZ,J6980		; error, no valid boot loader
		LD      HL,(SSECBUF)
		LD      A,(HL)
		OR      $02
		CP      $eb			; x86 JMP instruction ?
		JR      Z,J698A			; yep, update default drive and copy boot loader
J6980:		LD      A,(DE)
		ADD     A,C
		LD      C,A			; update drive id
		INC     DE
		INC     DE
		LD      A,(DE)
		AND     A			; more disk interfaces ?
		JR      NZ,J6964		; yep, next disk interface
		RET

J698A:		; add additional test for extended boot signature: FAT16 / MS-DOS boot sector
		; if it exists then there is no valid MSX bootloader
		LD	A,C			; save drive id
		LD	BC,$0026
		ADD	HL,BC
		LD	C,A			; restore drive id
		LD	A,(HL)
		AND	$fe			; EBS can be 28H or 29H
		CP	$28			; EBS?
		JR	Z,J6980			; Z=yes

		LD      A,C
		LD      (CUR_DRV),A		; update current drive
		LD      HL,(SSECBUF)
		LD      DE,BOT16K
		LD      BC,256
		LDIR
		OR      A
		RET

; ------------------------------------------------------------------------------

; Subroutine: get detected disk interfaces and drives
GetDrives:	ld	hl,DRVTBL
		ld	b,4			; number of interfaces = 4
		xor	a			; number of detected drives = 0
		ld	e,a			; number of detected interfaces = 0
_getdrives1:	ld	c,a			; store drives
		add	a,(hl)			; update drives
		jr	c,_getdrives3		; invalid DRVTBL, halt system
		cp	c			; entry used ?
		jr	z,_getdrives2		; nope, check if remaining entries are unused
		inc	e			; update detected interfaces
		inc	hl
		inc	hl
		djnz	_getdrives1		; next interface entry
		cp	a			; clear Cx, set Zx
		ret

_getdrives2:	add	a,(hl)
		cp	c			; entry used ?
		jr	nz,_getdrives3		; yep, halt system
		inc	hl
		inc	hl
		djnz	_getdrives2		; next interface entry
		cp	8			; valid number of detected drives ?
		ret	z			; z=yes, flag is also used by calling routine
		ret	c			; c=yes, "
_getdrives3:	jp	HaltSystem		; halt system

; Subroutine: switch system RAM to slot with memory mapper
SetSystemRam:	call	sysGetSlotP1		; get slot id of page 1
		call	InitRamMapper		; check and invoke memorymapper of 4 or more segments
		ret	c			; c=no mapper available

		; Update saved primary slot of page 2 and 3 on stack, otherwise memorymapper change will be rolled back
		; after CALLF is finished. Since page 2 is currently this disk rom the current primary page 3 slot is
		; copied to the page 2 slot setting on the stack.
		ld	hl,5
		add	hl,sp			; offset 5
		ld	a,(hl)
		and	$0f			; preserve page 0 and 1 slot setting
		ld	c,a
		in	a,($a8)
		and	$c0			; preserve current page 3 slot
		ld	b,a
		rrca				; move page 3 slot bits to page 2 slot bits
		rrca
		or	b			; add page 3 slot bits
		or	c			; add page 0 and page 1 slot bits
		ld	(hl),a
		call	sysGetSlotP1		; get slot id of page 1
		bit	7,a			; slot expanded ?
		ret	z			; nope, quit
		ld	hl,12
		add	hl,sp			; offset 12
		ld	c,a
		call	sysGetSlotP3		; get slot id of page 3
		xor	c
		and	$03			; same primary slot ?
		jr	nz,_systemram1		; nope, skip secondary slot register
		ld	a,(AFFFF)
		cpl	    			; current secondary slot register
		rrd
		; update saved secondary slot on stack, otherwise memorymapper change
		; will be rolled back after CALLF is finished
		ld	(hl),a

_systemram1:	DEC     HL			; offset 11
		in	a,($a8)
		RRD
		; update saved primary slot on stack, otherwise memorymapper change
		; will be rolled back after CALLF is finished)
		ld	(hl),a
		ret

; Subroutine allocate memory (adjust BASIC areapointers)
AllocMem:	ld	a,l
		or	h
		ret	z
		ex	de,hl
		ld	hl,0
		sbc	hl,de
		ld	c,l
		ld	b,h
		add	hl,sp
		ccf
		ret	c
		ld	a,h
		cp	TMPSTK / 256
		ret	c
		ld	de,(BOTTOM)
		sbc	hl,de
		ret	c
		ld	a,h
		cp	$02
		ret	c
		push	bc
		ld	hl,$0000
		add	hl,sp
		ld	e,l
		ld	d,h
		add	hl,bc
		push	hl
		ld	hl,(STKTOP)
		or	a
		sbc	hl,de
		ld	c,l
		ld	b,h
		inc	bc
		pop	hl
		ld	sp,hl
		ex	de,hl
		ldir
		pop	bc
		ld	hl,(HIMEM)
		add	hl,bc
		ld	(HIMEM),hl
		ld      de,-(2*256+2*9+2*2)	; 2 i/o channels, 2 filtab entries
		add     hl,de
		ld	(FILTAB),hl
		ex	de,hl
		ld	hl,(MEMSIZ)
		add	hl,bc
		ld	(MEMSIZ),hl
		ld	hl,(NULBUF)
		add	hl,bc
		ld	(NULBUF),hl
		ld	hl,(STKTOP)
		add	hl,bc
		jr	_allocio1		; update BASIC stack and initialize i/o channels

; Subroutine allocate i/o channels (system + user)
AllocIO:	ld	a,1
		ld	(MAXFIL),a		; number of user i/o channels = 1
		ld	hl,(HIMEM)
		ld	de,-(2*256+2*9+2*2)	; 2 i/o channels, 2 FILTAB entries
		add	hl,de
		ld	(FILTAB),hl		; pointer to i/o channel pointers
		ld	e,l
		ld	d,h			; store pointer to system i/o channel pointer
		dec	hl
		dec	hl
		ld	(MEMSIZ),hl		; top of BASIC memory
		ld	bc,200
		or	a
		sbc	hl,bc			; 200 bytes string space
		push	hl			; store top of BASIC stack
		ld	hl,2*2+9
		add	hl,de
		ld	(NULBUF),hl		; pointer to system i/o channel buffer
		pop	hl			; restore top of BASIC stack

_allocio1:	ld	(STKTOP),hl		; update top of BASIC stack
		dec	hl
		dec	hl
		ld	(SAVSTK),hl
		ld	l,e
		ld	h,d
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a,2			; number of i/o channels
_allocio2:	ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d			; update pointer to i/o channel
		inc	hl
		ex	de,hl
		ld	bc,7
		ld	(hl),b			; i/o channel mode = closed
		add	hl,bc			; +7
		ld	(hl),b			; clear i/o channel flags
		ld	bc,256+9-7
		add	hl,bc			; pointer to next i/o channel
		dec	a
		jr	nz,_allocio2		; next i/o channel
		ret

; ------------------------------------------------------------------------------

; Install disksystem routines
InitDiskSystem:	di
		call	sysGetSlotP1		; slot 1 is used to init the mapper (not slot 2)
		call	ramInitMapDos		; initialize memory mapper segments for dos
		call	InitPagHelper		; initialize paging helper routines
		ret	c			; c=fatal memory error
		ld	a,(VARMAPPER)
		bit 	0,a			; Use RAM Mapper?
		call	nz,ramInitMapTab	; nz=yes,initialize memory mapper segment table
		ld	a,1
		ld	(CUR_DRV),a              ; default drive = A:
		ld	a,(IDBYT0)
		rlca
		sbc	a,a
		add	a,6
		ld	(TIM_RA),a
		ld	(RANDOM),a
		ld	hl,P0_64K
		ld	de,P0_TPA
		ld	bc,4
		ldir
		ld	a,1
		ld	(KBUF+0),a
		jp	K_INIT

; ------------------------------------------------------------------------------
; Load and patch BDOS paging helper routines in ram p3
; In cxdos the paging helper routines are at fixed P3 RAM memory locations
; so no relocated code patching is required
; ------------------------------------------------------------------------------
InitPagHelper:	; allocate and clear memory for CXDOS
		ld	hl,(HIMEM)
		ld	(VARHIMEM),hl		; save himem for cxdos variables
		ld	de,VARBASE+VARSIZE
		or	a
		sbc	hl,de			; himem < cxdos minimum ?
		add	hl,de
		ret	c			; c=yes
		ld	de,VARDOS
		sbc	hl,de
		push	hl
		call	ramAllocMem		; allocate memory for cxdos
		pop	bc
		ret	c			; c=insufficient memory
		call	ClearArea		; hl=VARDOS bc=allocated memory

		; initialize dynamic memory top and bottom pointer
		dec	hl			; (VARHIMEM)-2
		ld	a,l
		and	$fe			; set to even address
		ld	l,a
		ld      (VARMEMPTR),hl
		ld	hl,varBottom
		ld	(VARLOMEM),hl

		; initialize stack pointers
		ld	hl,VARDOS+VSTI
		ld	(IRQ_ST),hl
		ld	hl,PAGCODE
		ld	(ST_BDOS),hl

		; copy paging helper routines from ROM to RAM P3
		ld	hl,pagBegin
		ld	de,PAGCODE
		ld	bc,pagTable-pagBegin	; VCODE
		ldir

		; copy jump table
		ld	hl,pagTable
		ld	de,JUMPB
		ld	bc,pagVectors-pagTable
		ldir

		; patch jump entries
		ld      hl,pagVectors
		ld	bc,$08ff		; b=8 vectors c=countdown ldi (16x)
		ld	a,$c3
pag01:		ld      e,(hl)
		inc     hl
		ld      d,(hl)
		inc     hl
		ld      (de),a			; jump instruction
		inc	de
		ldi				; copy vector
		ldi
		djnz	pag01
		xor	a			; no error
		ret

; ------------------------------------------------------------------------------
; *** IPL subroutines ***
; ------------------------------------------------------------------------------

; Subroutine: clear area / fill area
; Input: hl = start of area
;        bc = number of bytes
;        a  = filler (fill area)
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
;        b  = number of bytes
;        c  = filler
FillTable:	ld	(hl),c
		inc	hl
		djnz	FillTable
		ret

; Subroutine: patch h.lopd if h.clear is patched
; to regain control of BASIC initialization from extension
PatchBas:	ld	hl,H_CLEA
		ld	a,(hl)
		cp	$c9
		ret	z
		ld	hl,H_LOPD
		ld	de,_patchbas1
		jr	PatchHook

_patchbas1:	ld	a,$c9
		ld	(H_LOPD),a
		ld	de,(HIMEM)
		ld	(HIMSAV),de		; reregister HIMEM for disk system environment
		ret

; Subroutine: patch / set hook
; Input: hl = hook address
;        a  = slot (or driver slot)
;        de = routine address
PatchHook:	ld	a,(MASTER)
SetHook:	ld	(hl),$f7	; $f7 = rst $30
		inc	hl
		ld	(hl),a		; parameter: slot
		inc	hl
		ld	(hl),e		; parameter: address
		inc	hl
		ld	(hl),d
		inc	hl
		ld	(hl),$c9	; $c9 = ret
		ret

; Subroutine: save hook
; Input: hl = hook address
;        de = save to address
SaveHook:	push	hl
		ld	bc,5		; 5 bytes
		ldir			; save hook in hl to de
		pop	hl
		ret
