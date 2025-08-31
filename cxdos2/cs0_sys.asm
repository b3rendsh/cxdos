; ------------------------------------------------------------------------------
; cs0_sys.asm
; Main entry point, BDOS kernel handler and system routines.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		; Code segment 0
		SECTION	CS0_SYS			; System main entry point
		ORG	$4000
		SECTION	CS0_CON			; Console functions
		SECTION	CS0_DSK			; Common disk routines
		SECTION	CS0_FCB			; CP/M disk functions
		SECTION	CS0_FHS			; File handle disk functions
		SECTION	CS0_BAS			; Disk BASIC handler
		SECTION	CS0_RAM			; RAM functions
		SECTION	CS0_DRV			; Driver disk i/o

		; Code segment 1
		SECTION	CS1_SYS			; System
		ORG	$8000
		SECTION	CS1_IPL			; Initial program load
		SECTION	CS1_PAG			; Paging helper routines
		SECTION	CS1_XIO			; BDOS wrapper (msxdos2.sys)
		SECTION	CS1_TIM			; Time/date functions
		SECTION	CS1_ENV			; Environment functions
		SECTION	CS1_BAS			; Disk BASIC loader
		SECTION	CS1_MSG			; Messages
		SECTION	CS1_RAM			; RAM Mapper
		SECTION	CS1_DRV			; Driver initialization
		SECTION	CS1_END			; Patch area
		ORG	$BFD0

; ------------------------------------------------------------------------------

		SECTION	CS0_SYS

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	K_BDOS			; pag
		PUBLIC	GETWRK			; drv
		PUBLIC	GETSLT			; drv
		PUBLIC	sysWrkSlot		; ipl
		PUBLIC	sysTpaAbort		; con,dsk
		PUBLIC	sysTpaCall		; con,dsk
		PUBLIC	sysChainRmv		; fhs,env
		PUBLIC	sysCheckChar		; dsk,env
		PUBLIC	sysFarDos		; dsk,fcb

		; BASIC BDOS wrapper
		PUBLIC	sysBdos			; ipl
		PUBLIC	sysBdos1		; ipl

		; con
		EXTERN	F_CONIN			; 01
		EXTERN	F_CONOUT		; 02
		EXTERN	F_AUXIN			; 03
		EXTERN	F_AUXOUT		; 04
		EXTERN	F_LSTOUT		; 05
		EXTERN	F_DIRIO			; 06
		EXTERN	F_DIRIN			; 07
		EXTERN	F_INNOE			; 08
		EXTERN	F_BUFIN			; 0A
		EXTERN	F_CONST			; 0B

		; fcb
		EXTERN	F_DSKRST		; 0D
		EXTERN	F_SELDSK		; 0E
		EXTERN	F_FOPEN			; 0F
		EXTERN	F_FCLOSE		; 10
		EXTERN	F_SFIRST		; 11
		EXTERN	F_SNEXT			; 12
		EXTERN	F_FDEL			; 13
		EXTERN	F_RDSEQ			; 14
		EXTERN	F_WRSEQ			; 15
		EXTERN	F_FMAKE			; 16
		EXTERN	F_FREN			; 17
		EXTERN	F_LOGIN			; 18
		EXTERN	F_CURDRV		; 19
		EXTERN	F_SETDTA		; 1A
		EXTERN	F_ALLOC			; 1B
		EXTERN	F_RDRND			; 21
		EXTERN	F_WRRND			; 22
		EXTERN	F_FSIZE			; 23
		EXTERN	F_SETRND		; 24
		EXTERN	F_WRBLK			; 26
		EXTERN	F_RDBLK			; 27
		EXTERN	F_WRZER			; 28
		EXTERN	F_VERIFY		; 2E
		EXTERN	F_RDABS 		; 2F
		EXTERN	F_WRABS			; 30

		; tim
		EXTERN	F_GDATE			; 2A
		EXTERN	F_SDATE			; 2B
		EXTERN	F_GTIME			; 2C
		EXTERN	F_STIME			; 2D

		; fhs
		EXTERN	F_DPARM			; 31
		EXTERN	F_FFIRST		; 40
		EXTERN	F_FNEXT			; 41
		EXTERN	F_FNEW			; 42
		EXTERN	F_OPEN			; 43
		EXTERN	F_CREATE		; 44
		EXTERN	F_CLOSE			; 45
		EXTERN	F_ENSURE		; 46
		EXTERN	F_DUP			; 47
		EXTERN	F_READ			; 48
		EXTERN	F_WRITE			; 49
		EXTERN	F_SEEK			; 4A
		EXTERN	F_IOCTL			; 4B
		EXTERN	F_HTEST			; 4C
		EXTERN	F_DELETE		; 4D
		EXTERN	F_RENAME		; 4E
		EXTERN	F_MOVE			; 4F
		EXTERN	F_ATTR			; 50
		EXTERN	F_FTIME			; 51
		EXTERN	F_HDELETE		; 52
		EXTERN	F_HRENAME		; 53
		EXTERN	F_HMOVE			; 54
		EXTERN	F_HATTR			; 55
		EXTERN	F_HFTIME		; 56
		EXTERN	F_GETDTA		; 57
		EXTERN	F_GETVFY		; 58
		EXTERN	F_GETCD			; 59
		EXTERN	F_CHDIR			; 5A
		EXTERN	F_PARSE			; 5B
		EXTERN	F_PFILE			; 5C
		EXTERN	F_CHKCHR		; 5D
		EXTERN	F_WPATH			; 5E
		EXTERN	F_FLUSH			; 5F
		EXTERN	F_FORK			; 60
		EXTERN	F_JOIN			; 61
		EXTERN	F_ASSIGN		; 6A
		EXTERN	F_DSKCHK		; 6E

		; ram
		EXTERN	F_BUFFER		; 69
		EXTERN	K_ALLSEG
		EXTERN	K_FRESEG
		EXTERN	ramVarFree

		; env
		EXTERN	F_GENV			; 6B
		EXTERN	F_SENV			; 6C
		EXTERN	F_FENV			; 6D

		; drv
		EXTERN	DSKIO
		EXTERN	DSKCHG
		EXTERN	GETDPB
		EXTERN	CHOICE
		EXTERN	DSKFMT
		EXTERN	MTOFF
		EXTERN	READSEC

		; other
		EXTERN	basCallHandler
		EXTERN	msgGetError

; ------------------------------------------------------------------------------
; *** Routines in code segment 1 ***
; ------------------------------------------------------------------------------

cs1BootMain	equ	$8000
cs1DiskBasic	equ	$8003
cs1RamMapper	equ	$8006

; ------------------------------------------------------------------------------
; *** Header ***
; ------------------------------------------------------------------------------

		db	"AB"
		dw	BootMain		; init handler
		dw	basCallHandler		; call statement handler
		dw	$0000			; no device handler
		dw	$0000			; no basic program
		defs	$06,$00

; Jump table for disk driver routines
C4010:  	jp	DSKIO			; DSKIO
C4013:		jp	DSKCHG			; DSKCHG
C4016:		jp	GETDPB			; GETDPB
C4019:		jp	CHOICE			; CHOICE
C401C:		jp	DSKFMT			; DSKFMT
C401F:		jp	MTOFF			; MTOFF

C4022:		jp	DiskBasic		; start Disk BASIC
C4025:  	ret				; format disk
		defs	$0029-$,$00
C4029:		jp	sysMotorsOff
C402C:		nop

; DOS entry point (GETSLT)
C402D:		jp	GETSLT			; get slot id of page 1

; DOS entry point
DOS_INIT:	ld	hl,(DOSHIM)		; get top of DOS memory
		ret

; Pointer to kernel version ASCIIZ string
		defs	$0038-$,$00
C4038:		defw	TxtVersion

; EXTBIO handler
		defs	$0043-$,$00
C4043:		call	RamMapper
		jp	FCALSA

; H.TIMI handler
C4049:		push	af
		call	sysHtimi
		pop	af
		ret

; Routines in code segment 1
BootMain:	call	EnaCS1
		jp	cs1BootMain
DiskBasic:	call	EnaCS1
		jp	cs1DiskBasic
RamMapper:	call	EnaCS1
		jp	cs1RamMapper

; Enable code segment 1
EnaCS1:		push	af
		push	bc
		push	de
		push	hl
		call	GETSLT			; get slot of page 1 (this ROM)
		ld	h,$80			; enable page 2
		call	ENASLT
		pop	hl
		pop	de
		pop	bc
		pop	af
		ret

TxtVersion: 	db	"CXDOS kernel version 2.00",MOD1,MOD2,0

; ---------------------------------------------------------
; *** BDOS handler ***
; ---------------------------------------------------------
K_BDOS:		ei
		call	_kbdos1
		ld	(varBBFD),a
		ret

_kbdos1:	push	hl
		ex	af,af'
		ld	a,c
		cp	$71			; Function number valid?
		jr	c,r001			; c=yes
		ld	a,9			; set invalid function
r001:		ld	hl,FunctionTable
		add	a,a
		add	a,l
		ld	l,a
		jr	nc,r002
		inc	h
r002:		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a

		; check for functions in code segment 1
		ld	a,h
		cp	$80
		jr	c,r003
		ex	af,af'
		push	hl
		pop	ix
		pop	hl
		ld	iy,(MASTER-1)
		jp	CALSLT

		; functions in code segment 0
r003:		ex	af,af'
		ex	(sp),hl
		ld	iy,varIY
		ret

; ---------------------------------------------------------
; Subroutine to handle invalid BDOS function calls
; ---------------------------------------------------------
K_INVALID:	ld	a,_IBDOS
		ld	hl,$0000
		ret

; ---------------------------------------------------------
; Function table
; Note: function $09 is implemented by direct call to PRTBUF.
; ---------------------------------------------------------
FunctionTable:	dw	F_TERM0,F_CONIN,F_CONOUT,F_AUXIN	; 0
		dw	F_AUXOUT,F_LSTOUT,F_DIRIO,F_DIRIN	; 4
		dw	F_INNOE,K_INVALID,F_BUFIN,F_CONST	; 8
		dw	F_CPMVER,F_DSKRST,F_SELDSK,F_FOPEN	; 0C
		dw	F_FCLOSE,F_SFIRST,F_SNEXT,F_FDEL	; 10
		dw	F_RDSEQ,F_WRSEQ,F_FMAKE,F_FREN		; 14
		dw	F_LOGIN,F_CURDRV,F_SETDTA,F_ALLOC	; 18
		dw	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 1C
		dw	K_INVALID,F_RDRND,F_WRRND,F_FSIZE	; 20
		dw	F_SETRND,K_INVALID,F_WRBLK,F_RDBLK	; 24
		dw	F_WRZER,K_INVALID,F_GDATE,F_SDATE	; 28
		dw	F_GTIME,F_STIME,F_VERIFY,F_RDABS	; 2C
		dw	F_WRABS,F_DPARM,K_INVALID,K_INVALID	; 30
		dw	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 34
		dw	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 38
		dw	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 3C
		dw	F_FFIRST,F_FNEXT,F_FNEW,F_OPEN		; 40
		dw	F_CREATE,F_CLOSE,F_ENSURE,F_DUP		; 44
		dw	F_READ,F_WRITE,F_SEEK,F_IOCTL		; 48
		dw	F_HTEST,F_DELETE,F_RENAME,F_MOVE	; 4C
		dw	F_ATTR,F_FTIME,F_HDELETE,F_HRENAME	; 50
		dw	F_HMOVE,F_HATTR,F_HFTIME,F_GETDTA	; 54
		dw	F_GETVFY,F_GETCD,F_CHDIR,F_PARSE	; 58
		dw	F_PFILE,F_CHKCHR,F_WPATH,F_FLUSH	; 5C
		dw	F_FORK,F_JOIN,F_TERM,K_INVALID		; 60
		dw	K_INVALID,F_ERROR,F_EXPLAIN,K_INVALID	; 64
		dw	K_INVALID,F_BUFFER,F_ASSIGN,F_GENV	; 68
		dw	F_SENV,F_FENV,F_DSKCHK,F_DOSVER		; 6C
		dw	F_REDIR					; 70

; ---------------------------------------------------------
; Function $0C CPMVER
; ---------------------------------------------------------
F_CPMVER:	ld	hl,$0022	; version 2.2 (hex coded)
		xor	a
		ret

; ---------------------------------------------------------
; Function $00 TERM0
; ---------------------------------------------------------
F_TERM0:	ld	b,$00

; ---------------------------------------------------------
; Function $62 TERM
; ---------------------------------------------------------
F_TERM:		ld	a,b
		ld	b,$00
		call	sysTpaAbort
		jr	$

; ---------------------------------------------------------
; Function $65 ERROR
; ---------------------------------------------------------
F_ERROR:	ld	b,(iy+125)
		xor	a
		ret

; ---------------------------------------------------------
; Function $66 EXPLAIN
; Input:  B = error code
; ---------------------------------------------------------
F_EXPLAIN:	ld	a,b
		push	de
		ld	ix,msgGetError		; get error message in code segment 1
		call	sysFarDos
		ld	b,a
		or	a
		dec	hl
		call	nz,ByteToString
		xor	a
		ld	(hl),a
		pop	de
		ret

; Convert byte number to string
; Input:  a  = number (0-99)
;         hl = pointer to string
ByteToString:	ld	c,$ff
_bytetostring1:	inc	c
		sub	$0a
		jr	nc,_bytetostring1
		add	a,$3a
		push	af
		ld	a,c
		or	a
		call	nz,ByteToString
		pop	af
		ld	(hl),a
		inc	hl
		ret

; ---------------------------------------------------------
; Function $6F DOSVER
; Output: a  = 0 (no error)
;	   bc = cxdos kernel version
;	   de = xio version
; ---------------------------------------------------------
F_DOSVER:	ld	b,$02
		ld	c,$20		; kernel version 2.20 (BCD)
		ld	d,b
		ld	e,c		; xio version
		xor	a
		ld	h,a
		ld	l,a
		ret

; ---------------------------------------------------------
; Function $70 REDIR
; ---------------------------------------------------------
F_REDIR:	ld	c,(iy+9)
		or	a
		jr	z,_redir1
		ld	(iy+9),b
_redir1:	ld	b,c
		xor	a
		ret

; ------------------------------------------------------------------------------
; *** BASIC BDOS wrapper (see also XIO module) ***
; ------------------------------------------------------------------------------

; Subroutine BDOS abort handler
BdosAbort:	exx
		ld	b,a
		exx
		ld	sp,(SP_BDOS)
		ld	hl,(BREAKV)		; abort BDOS handler
		jp	BdosJpVec1

; Subroutine BDOS disk error handler
BdosDiskError:	ex	af,af'
		ld	l,c
		ld	c,a
		ld	a,b
		dec	a
		ld	b,l
		ld	hl,(DISKVE)		; BDOS disk error handler
		call	BdosJpVec
		ld	a,$03
		sub	c
		ei
		ret

; Subroutine 
BdosJpVec:	push	hl
		ld	hl,SDOSON
		ex	(sp),hl
BdosJpVec1:	push	hl
		ld	hl,JP_VEC
		ex	(sp),hl
		jp	SDOSOF

sysBdos:	ld	ix,(IX_BDOS)

sysBdos1:	ld	(VAR_A),A		; old: ex af,af'
		exx
		ld	hl,BdosAbort
		ld	(KAB_VE),hl		; install DOS abort handler
		ld	hl,BdosDiskError
		ld	(KDSK_V),hl		; install DOS disk error handler
		ld	hl,(ST_BDOS)
		or	a
		sbc	hl,sp			; using the BDOS stack ?
		jr	c,_bdos1		; c=no
		ld	bc,300
		sbc	hl,bc			; using the BDOS stack ?
		jr	c,_bdos2		; c=yes
_bdos1:		ld	(SP_BDOS),sp
		ld	sp,(ST_BDOS)
		call	ExecBdos
		ld	sp,(SP_BDOS)
		or	a
		ret

_bdos2:		call	ExecBdos		; execute BDOS function
		or	a
		ret

; Subroutine execute BDOS function (dispatcher)
ExecBdos:	exx
		push	hl
		ld	a,c
		cp	$67			; valid BDOS function ?
		jr	c,_bdos3		; c=yes
		xor	a			; no wrapper / unknown function
_bdos3:		ld	hl,FNTAB
		add	a,a
		add	a,l
		ld	l,a
		jr	nc,_bdos4
		inc	h
_bdos4:		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ex	(sp),hl
		ld	a,(VAR_A)		; old: ex af,af'
		ret

; BDOS function jump table
FNTAB:		dw	K_BDOS,FNCPM,FNCPM,FNCPM,FNCPM,FNCPM,FNCPM,FNCPM		; 00
		dw	FNCPM,SPRTBUF,FNBUFIN,FNCPM,FNCPM,FNCPM,FNCPM,FNFCB33		; 08
		dw	FNFCB33,FNFCB33,FNCPM,FNFCB33,FNFCB33,FNFCB33,FNFCB33,FNFCB33	; 10
		dw	FNCPM,FNCPM,FNCPM,FNALLOC,K_BDOS,K_BDOS,K_BDOS,K_BDOS		; 18
		dw	K_BDOS,FNFCB36,FNFCB36,FNFCB36,FNFCB36,K_BDOS,FNBLK,FNBLK	; 20
		dw	FNFCB36,K_BDOS,FNTIME,FNTIME,FNTIME,FNTIME,FNCPM,K_BDOS		; 28
		dw	K_BDOS,FNPARM,K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS		; 30
		dw	K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS		; 38
		dw	FNFIRST,FNNEXT,FNFIRST,FNFHS,FNFHS,K_BDOS,K_BDOS,K_BDOS		; 40
		dw	K_BDOS,K_BDOS,K_BDOS,K_BDOS,FNFHS,FNFHS,FNMOVE,FNMOVE		; 48
		dw	FNFHS,FNFHS,K_BDOS,FNHMOVE,FNHMOVE,K_BDOS,K_BDOS,K_BDOS		; 50
		dw	K_BDOS,FNWPATH,FNCHDIR,FNPARSE,FNPFILE,K_BDOS,FNWPATH,K_BDOS	; 58
		dw	K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS,K_BDOS,FNEXPLAIN		; 60

; BDOS function CP/M compatible output
FNCPM:		call	K_BDOS
		ld	a,l
		ld	b,h
		ret

; BDOS function buffered console input
FNBUFIN:	push	de
		ld	a,(de)
		ld	de,(BUF_1)		; pointer to temporary buffer
		ld	(de),a
		call	K_BDOS			; simple BDOS function
		pop	de
		ld	a,(de)
		inc	de
		ld	hl,(BUF_1)		; pointer to temporary buffer
		inc	hl
		ld	c,a
		ld	b,$00
		inc	bc
		ldir
		xor	a
		ld	b,a
		ld	l,a
		ld	h,a
		ret

; BDOS function alloc
FNALLOC:	call	K_BDOS
		ld	a,c
		ld	bc,512
		ret

; BDOS function sequential i/o FCB size 33
FNFCB33:	ld	a,33
		db	$21

; BDOS function random i/o FCB size 36
FNFCB36:	ld	a,36
		push	de
		exx
		pop	hl
		ld	c,a
		ld	b,0

; BDOS function with FCB parameter
RW:		push	hl
		push	bc
		ld	de,(BUF_1)		; pointer to temporary buffer
		push	de
		ldir				; copy FCB
		exx
		pop	de
		push	de
		call	K_BDOS			; simple BDOS function
		exx
		pop	hl
		pop	bc
		pop	de
		ldir				; copy fcb
		exx
		ld	a,l
		ld	b,h
		ret

FNBLK:		push	de
		exx
		pop	hl
		push	hl
		ld	bc,15
		add	hl,bc
		ld	c,$24			; CP/M random i/o FCB size
		ld	a,(hl)
		or	a			; record size > 255 ?
		jr	nz,_fnblk1		; yep,
		dec	hl
		ld	a,(hl)
		cp	64			; record size < 64 ?
		jr	nc,_fnblk1		; nope,
		inc	c			; random i/o FCB size
_fnblk1:	pop	hl
		call	RW			; BDOS function with FCB parameter
		ex	de,hl
		ret

; BDOS function date/time
FNTIME:		call	K_BDOS
		ld	a,c
		ret

; BDOS function get disk parameters
FNPARM:		push	de
		ld	de,(BUF_1)
		call	K_BDOS
		ex	de,hl
		pop	de
		push	de
		ld	bc,32
		ldir
		pop	de
		ret

; BDOS function find first / new
FNFIRST:	call	CopyDE
		call	C,CopyHL
		ret	nz

; BDOS function find next
FNNEXT:		push	ix
		pop	hl
		push	hl
		ld	de,(BUF_2)
		push	bc
		ld	bc,64
		ldir
		pop	bc
		call	FNKBDOS3
		pop	de
		ld	hl,(BUF_2)
		ld	bc,64
		ldir
		ret

; BDOS function rename / move
FNMOVE:		call	CopyHL
		ld	hl,(BUF_3)

; BDOS function FHS
FNFHS:		ex	af,af'
		push	de
		call	CopyDE
		push	af			; store parameter type
		ex	af,af'
		call	FNKBDOS1
		ex	af,af'
		exx
		pop	af			; restore parameter type
		pop	de
		jr	nc,FNRET
		ld	hl,(BUF_1)
		ld	bc,64
		ldir
FNRET:		ex	af,af'
		exx
		ret

; BDOS function
FNHMOVE:	ex	af,af'
		call	CopyHL
		ex	af,af'

; Call BDOS function with 3 pointer parameters
FNKBDOS3:	ld	ix,(BUF_2)
		ld	hl,(BUF_3)

; Call BDOS function with pointer parameter
FNKBDOS1:	ld	de,(BUF_1)
		jp	K_BDOS

; BDOS function parse path
FNPARSE:	push	de
		call	Copy64			; copy FIB parameter to temporary buffer
		pop	de

; BDOS function path
FNWPATH:	ex	de,hl
		push	hl
		ld	de,(BUF_1)
		or	a
		sbc	hl,de
		push	hl
		push	de
		call	K_BDOS
		exx
		pop	hl
		pop	bc
		pop	de
		push	bc
		ld	bc,64
		ldir
		exx
		ex	(sp),hl
		ex	de,hl
		add	hl,de
		ex	(sp),hl
		add	hl,de
		pop	de
		ret

; BDOS function
FNPFILE:	push	hl
		push	hl
		ld	l,e
		ld	h,d
		call	Copy64			; copy FIB parameter to temporary buffer
		ld	de,(BUF_1)
		or	a
		sbc	hl,de
		ex	(sp),hl
		push	hl
		call	FNKBDOS3
		exx
		pop	de
		ld	bc,11
		ld	hl,(BUF_3)
		ldir
		exx
		pop	hl
		add	hl,de
		ex	de,hl
		pop	hl
		ret

; BDOS function chdir
FNCHDIR:	call	Copy64			; copy FIB parameter to temporary buffer
		jp	FNKBDOS1

; BDOS function explain
FNEXPLAIN:	push	de
		ld	de,(ERR_BUF)
		call	K_BDOS
		ex	de,hl
		pop	de
		push	de
		push	bc
		ld	bc,64
		ldir
		pop	bc
		pop	de
		ret

; BDOS wrapper subroutines

CopyDE:		ld	a,(de)
		inc	a			; parameter is a FIB?
		jr	nz,Copy100		; nz=no

Copy64:		push	hl
		push	bc
		ex	de,hl
		ld	de,(BUF_1)
		ld	bc,64
		ldir
		pop	bc
		pop	hl
		scf				; parameter is a FIB
		ret

Copy100:	push	hl
		push	bc
		ex	de,hl
		ld	de,(BUF_1)
		ld	b,100
		call	CopyString
		pop	bc
		pop	hl
		ret

CopyHL:		push	de
		push	bc
		ld	de,(BUF_3)
		ld	b,100
		call	CopyString
		pop	bc
		pop	de
		ret

CopyString:	ld	a,(hl)
		inc	hl
		ld	(de),a
		inc	de
		or	a
		ret	z
		djnz	CopyString
		ld	a,_PLONG		; pathname too long
		ret

; ---------------------------------------------------------
; *** TPA program abort / call routines ***
; ---------------------------------------------------------

; Call program abort routine with TPA segments active
sysTpaAbort:	ld	hl,(KAB_VE)

; Call routine with TPA segments active
sysTpaCall:	push	hl
		ld	hl,SDOSON
		ex	(sp),hl
		push	hl
		jp	SDOSOF

; ---------------------------------------------------------
; *** Motors Off ***
; ---------------------------------------------------------
sysMotorsOff:	ld	hl,DRVTBL
		ld	b,4
_motorsoff1:	inc	hl
		ld	a,(hl)
		push	af
		pop	iy
		inc	hl
		push	hl
		push	bc
		ld	hl,A401F
		push	hl
		pop	ix
		or	a
		call	nz,RDSLT
		or	a
		call	nz,CALSLT
		pop	bc
		pop	hl
		djnz	_motorsoff1
		ret

; ---------------------------------------------------------
; *** H.TIMI handler ***
; ---------------------------------------------------------
sysHtimi:	push	af			; store VDP status register
		call	IrqHandler		; CXDOS interrupt handler
		call	DskIrq			; execute disk interface interrupt handlers
		pop	af			; restore VDP status register
		jp	TIMI_S			; next H_TIMI handler

; Subroutine execute disk interface interrupt handlers
DskIrq:		ld	de,DRVTBL
		ld	hl,HOOKSA
		ld	b,4
_dskirq1:	ld	a,(de)
		and	a			; entry used ?
		ret	z			; nope, end of table, quit
		inc	de
		ld	a,(de)			; slot id interface
		inc	de
		cp	(hl)			; does interface have a driver interrupt handler ?
		jr	nz,_dskirq2		; nope, next entry
		ld	a,(MASTER)
		cp	(hl)			; driver interrupt handler in DOS master ROM ?
		ld	a,(hl)			; slot id
		push	bc
		push	de
		push	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; address interrupt handler
		push	af
		pop	iy			; iyh = slot id
		push	de
		pop	ix			; ix = address interrupt handler
		call	_dskirq3		; call driver interrupt handler
		pop	hl
		pop	de
		pop	bc
_dskirq2:	inc	hl
		inc	hl
		inc	hl
		djnz	_dskirq1		; next entry
		ret

; Subroutine call driver interrupt handler
_dskirq3:	jp	nz,CALSLT		; not in MASTER rom, use CALSLT
		jp	(IX)			; in MASTER rom, use fast jump

; Subroutine interrupt handler
IrqHandler:	di
		ld	a,1
		ld	(ST_COU),a
		ld	hl,TIM_CO
		inc	(hl)
		ld	a,(TIM_RA)
		cp	(hl)
		jr	nz,_irqhandler1
		ld	(hl),$00
		ld	a,(CH_COU)
		CP	$02
		adc	a,$ff
		ld	(CH_COU),a
		ld	a,(TIM_TI)
		cp	$07
		adc	a,$00
		ld	(TIM_TI),a
_irqhandler1:	ld	hl,(RANDOM+0)
		ld	a,(RANDOM+2)
		ld	c,a
		rrca
		rrca
		rrca
		xor	c
		rla
		rla
		adc	hl,hl
		ld	a,c
		adc	a,a
		ld	(RANDOM+2),a
		ld	(RANDOM+0),hl
		ret

; ------------------------------------------------------------------------------
; *** Validate character ***
; ------------------------------------------------------------------------------

; ---------------------------------------------------------
; Subroutine check character
; Input:  C = character flags:
;		 b0 = set suppress upcasing
;		 b1 = set 1st double byte character (obsolete)
;		 b2 = set 2nd double byte character (obsolete)
;		 b3 = set volume name
; ---------------------------------------------------------
sysCheckChar:	res	1,c
		res	2,c
		res	4,c
		bit	0,c
		call	z,MakeUpCase
		bit	3,c
		call	ValChar
		jr	nc,_checkchar1
		set	4,c
_checkchar1:	or	a
		ret	nz
		set	4,c
		ret

; ---------------------------------------------------------
; Subroutine make upper case
; Notes:
; 1. The conversion routine is copied here so no table
; is required in the RAM Page 2 workarea
; 2. The H.UP hook call is removed
; ---------------------------------------------------------
MakeUpCase:	cp	'a'
		ret	c
		cp	'z'+1
		jr	c,_upchar	; lower case to upper case
		cp	$80
		ret	c
		cp	$C0
		ret	nc
		push	hl
		ld	hl,_uptable-$80
		push	bc
		ld	b,0
		ld	c,a
		add	hl,bc
		ld	a,(hl)
		pop	bc
		pop	hl
		ret

_upchar:	add	a,-32		; Make upper case
		ret

; international upper case table for accented characters
_uptable:	db	$80,$9a,$45,$41,$8e,$41,$8f,$80
		db	$45,$45,$45,$49,$49,$49,$8e,$8f
		db	$90,$92,$92,$4f,$99,$4f,$55,$55
		db	$59,$99,$9a,$9b,$9c,$9d,$9e,$9f
		db	$41,$49,$4f,$55,$a5,$a5,$a6,$a7
		db	$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
		db	$b0,$b0,$b2,$b2,$b4,$b4,$b6,$b6
		db	$b8,$b8,$ba,$bb,$bc,$bd,$be,$bf

; ---------------------------------------------------------
; Subroutine validate character
; Input:  a  = character
;	   zx = set: file name, reset: volume name
; Output: cx set if illegal character
; ---------------------------------------------------------
ValChar:	push	hl
		push	bc
		ld	bc,17
		ld	hl,_illegal
		jr	z,_valchar1
		ld	bc,6
_valchar1:	cp	SPACE
		jr	c,_valchar2
		cpir
		jr	nz,_valchar3
_valchar2:	scf
_valchar3:	pop	bc
		pop	hl
		ret

_illegal:	db	$7f,"|<>/",$ff," :;.,=+\\\"[]"

; ------------------------------------------------------------------------------
; *** System routines ***
; ------------------------------------------------------------------------------

; Get slot id of disk rom
GETSLT:		push	hl
		push	bc
		in	a,($a8)
		rrca
		rrca
		call	_getSlotExp
		jr	z,_getslotr1		; z=slot not expanded
		jr	_getslotr2

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

; Get pointer to driver work area
GETWRK:		call	sysWrkSlot		; get pointer to SLTWRK entry
		ld	a,(hl)			; hl = (hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		push	hl
		pop	ix
		ret

; Get pointer to SLTWRK entry
sysWrkSlot:	in	a,($a8)
		and	$0c
		rrca
		rrca
		ld	hl,EXPTBL
		ld	c,a
		ld	b,0			; b=0 used 2 times in hl=hl+a below
		add	hl,bc			; hl=hl+a (1)
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		inc	a
		ld	c,a			; c=16*a+1
		ld	a,(hl)
		add	a,a
		sbc	a,a
		and	$0c
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		and	(hl)
		or	c
		add	a,a
		ld	hl,SLTWRK
		ld	c,a
		add	hl,bc			; hl=hl+a (2)
		ret

; Subroutine: remove element from chain
; Input:  hl = start of chain
;	   de = address of element
sysChainRmv:	ex	de,hl
		ld	b,h
		ld	c,l
		call	ramVarFree
		ex	de,hl
_chainrmv1:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ld	a,d
		or	e
		ret	z
		ex	de,hl
		sbc	hl,bc
		add	hl,bc
		jr	nz,_chainrmv1
		dec	de
		ld	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(hl)
		ld	(de),a
		ret

; Subroutine: far call to cxdos routine in other code segment
sysFarDos:	push	iy
		ld	iy,(MASTER-1)
		call	CALSLT
		ei
		pop	iy
		ret

; ---------------------------------------------------------------------------------------
; Include runtime part of disk interface driver

		SECTION CS0_DRV
		DEFINE	DRV_SYS
	IF PPIDE || CFIDE
		INCLUDE	"../driver/driver.asm"
		INCLUDE	"../driver/drv_ide.asm"
	ELIF JIO
		INCLUDE	"../driver/drv_jio.asm"
	ENDIF

