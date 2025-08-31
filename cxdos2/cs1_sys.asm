; ------------------------------------------------------------------------------
; cs1_sys.asm
; System routines code segment 1.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS1_SYS

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	K_INIT			; ipl
		PUBLIC	PROMPT			; ipl
		PUBLIC	sysPrintChar		; ipl
		PUBLIC	sysCRLF			; ipl
		PUBLIC	sysPrintStrZ		; ipl
		PUBLIC	sysGetSlotP1		; sys,ipl
		PUBLIC	sysGetSlotP2		; ipl
		PUBLIC	sysGetSlotP3		; ipl
		PUBLIC	sysPrintMsg		; drv
		PUBLIC	sysPrintString		; drv
		PUBLIC	sysPrintCRLF		; drv

		EXTERN	iplBootMain
		EXTERN	iplDiskBasic
		EXTERN	msgGetMessage
		EXTERN	ramMapper

		; Symbols in code segment 0 used by K_INIT
		EXTERN	F_ASSIGN		; fhs
		EXTERN	F_BUFFER		; ram
		EXTERN	F_JOIN			; sys
		EXTERN	ramVarAlloc		; ram
		EXTERN	timInitRTC		; tim
		EXTERN	K_CON_INIT		; con
		EXTERN	DEV_CON			; con
		EXTERN	DEV_AUX			; con
		EXTERN	DEV_LST			; con
		EXTERN	DEV_NUL			; con

; ------------------------------------------------------------------------------
; *** Routines in code segment 1 ***
; ------------------------------------------------------------------------------

C8000:		jp	iplBootMain
C8003:		jp	iplDiskBasic
C8006:		jp	ramMapper

; ---------------------------------------------------------
; *** Initialize kernel / BDOS ***
; ---------------------------------------------------------
K_INIT:		ld	iy,varIY		; base address for IY relative kernel variables
		ld	hl,DRVTBL
		ld	de,SDPBLI
		ld	b,$04			; 4 disk interfaces
_init1:		ld	a,(hl)
		inc	hl
		ld	c,(hl)			; slot id
		inc	hl
		or	a
		push	hl
		push	bc
		ld	b,a			; number of drives
		ld	l,$10			; driver jump table offset
		call	nz,K_INIT_DRVTAB	; allocate and initialize drive tables
		pop	bc
		pop	hl
		jr	nz,_init3
		djnz	_init1			; next disk interface
		ld	d,b
		call	F_ASSIGN
		ld	hl,DeviceTable
_init2:		ld	a,(hl)
		or	a
		jr	z,_init5
		inc	hl
		push	hl
		ld	hl,43
		call	ramVarAlloc
		pop	de
_init3:		jr	nz,_init6
		ld	bc,(varBBF4)
		ld	(varBBF4),hl		; Update start of device chain
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ex	de,hl
		ldi
		ldi
		ex	de,hl
		ld	bc,6
		add	hl,bc
		ex	de,hl
		ld	bc,12
		ldir
		ld	a,$80
		ld	(de),a
		ld	b,$14
		xor	a
_init4:		inc	de
		ld	(de),a
		djnz	_init4
		jr	_init2

_init5:		ld	b,1			; number of buffers (old:5)
		call	F_BUFFER		; allocate buffers
		ld	b,$00
		call	F_JOIN
		call	K_CON_INIT
		ld	a,1
		ld	(ST_COU),a
		ld	(iy+16),$ff
		call	timInitRTC		; initialize clockchip
		or	a
		ret

_init6:		scf
		ret

DeviceTable:	db	$ff
		dw	DEV_CON
		db	$a3			; device, ascii mode, console input device, console output device
		db	"CON        "

		db	$ff
		dw	DEV_LST
		db	$a0			; device, ascii mode
		db	"LST        "

		db	$ff
		dw	DEV_LST
		db	$a0			  ; device, ascii mode
		db	"PRN        "

		db	$ff
		dw	DEV_NUL
		db	$a0			  ; device, ascii mode
		db	"NUL        "

		db	$ff
		dw	DEV_AUX
		db	$a0			  ; device, ascii mode
		db	"AUX        "

		db	0

; ---------------------------------------------------------
; Subroutine allocate and initialize drive tables
; Input:  B  = number of drives
;		L  = driver jump table offset
;		C  = slot id
;		DE = pointer to DPB entry
; ---------------------------------------------------------
K_INIT_DRVTAB:	xor	a
_drvtab1:	ex	af,af'
		push	hl
		ld	hl,96
_drvtab2:	call	ramVarAlloc
		jr	nz,_drvtab6
		ex	de,hl
		push	de
		pop	ix
		push	bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
_drvtab3:	inc	hl
		push	hl
_drvtab4:	ld	(ix+2),c
		ld	(ix+3),b
		ld	a,(bc)
		inc	a
		ld	(ix+8),a
		ld	l,a
		ld	h,$00
		inc	bc
		ld	a,(bc)
		ld	(ix+29),a
		ld	bc,varBBFB
		jr	z,_drvtab5
		ld	bc,varBA23
_drvtab5:	add	hl,hl
		add	hl,bc
		ld	(hl),e
		inc	hl
		ld	(hl),d
		pop	de
		pop	bc
		pop	hl
		ld	(ix+0),c
		ld	(ix+1),l
		ex	af,af'
		ld	(ix+6),a
		inc	a
		ld	(ix+30),$ff
		ld	(ix+31),$ff
		djnz	_drvtab1
		xor	a
		ret

_drvtab6:	pop	hl
		ret

; ------------------------------------------------------------------------------
; *** System routines ***
; ------------------------------------------------------------------------------

; Subroutine: prompt for phantom drive (not used)
PROMPT:		ld	a,(TARGET)
		add	a,'a'
		push	af			; store drive letter
		call	sysCRLF
		ld	a,7			; "Insert disk for drive "
		call	sysMsg
		pop	af			; restore drive letter
		call	sysPrintChar
		ld	a,8			; ":"
		call	sysMsg			; message to screen
		call	sysCRLF
		ld	a,9			; "and strike a key when ready "
		call	sysMsg
_prompt1:	call	GetKey			; get fresh key
		cp	$03			; CTRL-STOP?
		jr	z,_prompt1		; abort, again

; Subroutine: CR/LF to screen
sysCRLF:	ld	a,CR
		call	sysPrintChar
		ld	a,LF

; Subroutine character to screen
sysPrintChar:	push	ix
		ld	ix,CHPUT
callbios:	call	FarBios
		pop	ix
		ret

; Subroutine: get fresh key
GetKey:		push	ix
		push	hl
		ld	ix,KILBUF
		call	FarBios
		pop	hl
		ld	ix,CHGET
		jr	callbios

; Subroutine: message to screen
sysMsg:		push	hl
		push	de
		ld	de,(SSECBUF)
		call	msgGetMessage
		call	sysPrintStrZ
		pop	de
		pop	hl
		ret

; Subroutine: string to screen
sysPrintStrZ:	ld	a,(de)
		inc	de
		or	a
		ret	z
		call	sysPrintChar
		jr	sysPrintStrZ

; Subroutine: far call bios
FarBios:	push	iy
		ld	iy,(EXPTBL-1)
		call	CALSLT
		ei
		pop	iy
		ret

; ------------------------------------------------------------------------------
; Get slot id of the currently selected slot for page 1,2 or 3
; Notes: 
; 1. Get slot id for page 0 is not used and excluded from CXDOS
; 2. Get slot id for page 1 is duplicated in code segment 0 (GETSLT)
; ------------------------------------------------------------------------------

sysGetSlotP1:	push	hl
		push	bc
		in	a,($a8)
		rrca
		rrca
		call	_getSlotExp
		jr	z,_getslotr1		; z=slot not expanded
		jr	_getslotr2

sysGetSlotP2:	push	hl
		push	bc
		in	a,($a8)
		rrca
		rrca
		rrca
		rrca
		call	_getSlotExp
		jr	z,_getslotr1		; z=slot not expanded
		jr	_getslotr3

sysGetSlotP3:	push	hl
		push	bc
		in	a,($a8)
		rlca
		rlca
		call	_getSlotExp
		jr	z,_getslotr1		; z=slot not expanded
		rrca
		rrca
_getslotr3:	rrca
		rrca
_getslotr2:	and	$0c			; mask for secondary slot bits
		or	$80			; set expanded slot flag
		or	c			; add primary slot bits
_getslotr1:	pop	bc
		pop	hl
		ret

; Get secondary slot register (if slot is expanded)
_getSlotExp:	and	$03
		ld	c,a
		ld	b,$00
		ld	hl,EXPTBL
		add	hl,bc
		bit	7,(hl)			; is slot expanded?
		ret	z			; z=no
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)			; get secondary slot register
		ret

; ------------------------------------------------------------------------------
; Print subroutines, used by driver initialization routines. 
; System BIOS must be active in page 0!
; ------------------------------------------------------------------------------

sysPrintMsg:	ex	(sp),hl
		call	sysPrintString
	  	ex	(sp),hl
		ret

sysPrintString:	ld	a,(hl)
		inc	hl
	  	and	a
		ret	z
	  	rst	R_OUTDO			; output character
		jr	sysPrintString

sysPrintCRLF:	ld	a,CR
		rst	R_OUTDO
		ld	a,LF
		rst	R_OUTDO
		ret

; ------------------------------------------------------------------------------
; *** Include initialization part of disk interface driver ***
; ------------------------------------------------------------------------------

		SECTION CS1_DRV
		DEFINE	DRV_IPL
	IF PPIDE || CFIDE
		INCLUDE	"../driver/driver.asm"
	ELIF JIO
		INCLUDE	"../driver/drv_jio.asm"
	ENDIF

; ------------------------------------------------------------------------------
; *** Last section in code segment 1 ***
; ------------------------------------------------------------------------------

		SECTION	CS1_END

		PUBLIC	bootWait

		; Reserved
		ds	$2e,$00

bootWait:	dw	$5000		; 7FFE Boot menu wait time-out (default $5000 is appr. 3 sec for MSX/3.58Mhz)

