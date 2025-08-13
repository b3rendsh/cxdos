; ------------------------------------------------------------------------------
; sys.asm
; CXDOS 1 system module
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		INCLUDE	"cxdos1.inc"
		SECTION SYS

		PUBLIC	SysBoot
		PUBLIC	SysEnd
		PUBLIC	SysSize

		EXTERN	DOS_SSBIOS
		EXTERN	DOS_SIN
		EXTERN	DOS_SOUT
		EXTERN	DOS_CPMVER
		EXTERN	DOS_DELETE
		EXTERN	DOS_RENAME
		EXTERN	DOS_OPEN
		EXTERN	DOS_CLOSE
		EXTERN	DOS_CREATE
		EXTERN	DOS_ABSREA
		EXTERN	DOS_ABSWRI
		EXTERN	DOS_SEQRD
		EXTERN	DOS_SEQWRT
		EXTERN	DOS_RNDRD
		EXTERN	DOS_RNDWRT
		EXTERN	DOS_BLKRD
		EXTERN	DOS_BLKWRT
		EXTERN	DOS_ZWRITE
		EXTERN	DOS_SRCHFR
		EXTERN	DOS_SRCHNX
		EXTERN	DOS_FILESI
		EXTERN	DOS_LOGIN
		EXTERN	DOS_SETDMA
		EXTERN	DOS_GETEFA
		EXTERN	DOS_DSKRES
		EXTERN	DOS_WRTFAT
		EXTERN	DOS_GETDRV
		EXTERN	DOS_SETRND
		EXTERN	DOS_SELDSK
		EXTERN	DOS_BUFIN
		EXTERN	DOS_CRLF
		EXTERN	DOS_BUFOUT
		EXTERN	DOS_CONOUT
		EXTERN	DOS_CONSTA
		EXTERN	DOS_CONIN
		EXTERN	DOS_IN
		EXTERN	DOS_RAWIO
		EXTERN	DOS_RAWINP
		EXTERN	DOS_LIST
		EXTERN	DOS_READER
		EXTERN	DOS_PUNCH
		EXTERN	DOS_GETDAT
		EXTERN	DOS_SETDAT
		EXTERN	DOS_GETTIM
		EXTERN	DOS_SETTIM
		EXTERN	DOS_SETRAW

SysBegin:
		PHASE	SYSBASE			; This must be at a 256 byte page boundary

; ------------------------------------------------------------------------------
; SYS BDOS jump table
; ------------------------------------------------------------------------------

JBDOS:		defs	6,0			; BDOS handler is aligned to XX06 for compatibility with CP/M
		jp	XBDOS			; BDOS handler
		dw	DSK_ERR			; disk error handler
		dw	DSK_ERR
		dw	DSK_ERR
		dw	DSK_ERR
		dw	BRK_ERR			; break handler
BDOSV:		dw	JBDOS+6			; start of transient part of COMMAND.COM
ENTERS:		dw	1			; number of entries
CHECKSUM:	dw	-1			; word checksum
BATCHFLAG:	db	0			; batch file running flag
		db	0			; cold boot flag (not used)

; Messages part 1 of 2
; Messages moved to unused space

MsgTermBatch:	db	CR,LF
		db	"Terminate batch file (Y/N)? "
		db	"$"

MsgInsertDos:	db	CR,LF
		db	"Insert DOS disk and press any key..",CR,LF
		db	"$"

MsgBadFat:	db	CR,LF
		db	"Bad FAT, drive "
MsgDrive:	db	"A"
		db	CR,LF
		db	"$"

MsgRead:	db	"read"

MsgWrit:	db	"writ"

		defs	128,0			; space for stack (at least 128 bytes)
		defs	SYSBASE+$100-$,0

; ------------------------------------------------------------------------------
; BIOS jump table
; ------------------------------------------------------------------------------

SysBoot:	jp	Startup			; BOOT entry
SysReboot:	jp	Startup			; WBOOT entry
		jp	ConStat			; CONST entry
		jp	ConIn			; CONIN entry
		jp	ConOut			; CONOUT entry

; ------------------------------------------------------------------------------
; BOOT/WBOOT CP/M BIOS handler
; ------------------------------------------------------------------------------
Startup:	ei
		ld	sp,SysBoot
		ld	hl,SysReboot
		ld	(WBOOT+1),hl		; initialize WBOOT
		ld	hl,JBDOS+6
		ld	(KBDOS+1),hl		; intialize BDOS
		ld	a,$c3
		ld	(WBOOT+0),a
		ld	(KBDOS+0),a
		call	DOS_WRTFAT		; flush buffers
		ld	hl,(BDOSV)		; BDOS handler
		ld	bc,(ENTERS)		; number of entries
		ld	de,0
r001:		ld	a,(hl)
		inc	hl
		add	a,e
		ld	e,a
		ld	a,(hl)
		inc	hl
		adc	a,d
		ld	d,a
		dec	bc
		ld	a,b
		or	c
		jr	nz,r001
		ld	hl,(CHECKSUM)		; checksum
		sbc	hl,de			; checksum correct ?
		jr	nz,r002			; nz=no, load COMMAND.COM
		ld	hl,(BDOSV)		; BDOS handler
		jp	(hl)

r002:		ld	de,CommandM		; COMMAND.COM FCB
		call	DOS_OPEN
		or	a			; error?
		jr	z,LoadOk		; z=no

; handle error
LoadErr:	ld	de,MsgInsertDos		; insert DOS disk message
		call	SPRTBUF
		call	DOS_IN
		jr	r002			; retry

LoadOk:		ld	hl,0
		ld	(CommandM+33+0),hl
		ld	(CommandM+33+2),hl	; random record = 0
		inc	hl
		ld	(CommandM+14),hl	; record size = 1
		ld	hl,TBASE		; TPA base address
		ld	(DMAADD),hl		; initialize DMA address
		ld	de,CommandM		; COMMAND.COM FCB
		ld	hl,JBDOS-TBASE
		call	DOS_BLKRD
		or	a
		jr	z,LoadErr		; z=no, handle error
		jp	TBASE			; start COMMAND.COM

; unsupported function
DosRet:		xor	a
		ld	b,a
		ret

;------------------------------------------------------------------------------
; BDOS handler (jump from $0005)
; Note: the handler doesn't buffer data structures in page 3
;------------------------------------------------------------------------------

XBDOS:		ld	a,1
		ld	(CPMCAL),a		; assume CP/M function
		ld	a,c
		cp	$30+1			; valid function ?
		jr	nc,DosRet		; nc=no, quit with result = ok
		ld	(SPSAVE),sp
		ld	sp,SysBoot

	IF 0	; store/restore pointer in the function itself
		cp	$11			; search for first ?
		jr	nz,xbdos1		; nz=no
		ld	(SRCHFC),de		; store pointer to FCB
xbdos1:		cp	$12			; search for next ?
		jr	nz,xbdos2		; nz=no
		ld	de,(SRCHFC)		; restore pointer to FCB
xbdos2:
	ENDIF

		push	hl
		ld	hl,XBDOS_DONE
		ex	(sp),hl
		push	hl			; after this,
		ld	hl,FNTAB
		ld	b,0
		add	hl,bc
		add	hl,bc
		ld	b,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,b			; pointer to BDOS function
		ex	(sp),hl			; BDOS function handler
		ret

; Returning point from BDOS function
XBDOS_DONE:	push	af
		ld	a,(CPMCAL)
		or	a			; CP/M function ?
		jr	z,xbdos7		; z=no, quit
		pop	af
		ld	l,a
		ld	h,b			; result in hl, CP/M compatible
		ld	sp,(SPSAVE)
		ret

xbdos7:		pop	af
		ld	sp,(SPSAVE)
		ret

FNTAB:		dw	WBOOT			; 00
		dw	DOS_CONIN		; 01
		dw	DOS_CONOUT		; 02
		dw	DOS_READER		; 03
		dw	DOS_PUNCH		; 04
		dw	DOS_LIST		; 05
		dw	DOS_RAWIO		; 06
		dw	DOS_RAWINP		; 07
		dw	DOS_IN			; 08
		dw	SPRTBUF			; 09
		dw	DOS_BUFIN		; 0A
		dw	DOS_CONSTA		; 0B
		dw	DOS_CPMVER		; 0C
		dw	DOS_DSKRES		; 0D
		dw	DOS_SELDSK		; 0E
		dw	DOS_OPEN		; 0F
		dw	DOS_CLOSE		; 10
		dw	DOS_SRCHFR		; 11
		dw	DOS_SRCHNX		; 12
		dw	DOS_DELETE		; 13
		dw	DOS_SEQRD		; 14
		dw	DOS_SEQWRT		; 15
		dw	DOS_CREATE		; 16
		dw	DOS_RENAME		; 17
		dw	DOS_LOGIN		; 18
		dw	DOS_GETDRV		; 19
		dw	DOS_SETDMA		; 1A
		dw	DOS_GETEFA		; 1B
		dw	DosRet			; 1C
		dw	DosRet			; 1D
		dw	DosRet			; 1E
		dw	DosRet			; 1F
		dw	DosRet			; 20
		dw	DOS_RNDRD		; 21
		dw	DOS_RNDWRT		; 22
		dw	DOS_FILESI		; 23
		dw	DOS_SETRND		; 24
		dw	DosRet			; 25
		dw	DOS_BLKWRT		; 26
		dw	DOS_BLKRD		; 27
		dw	DOS_ZWRITE		; 28
		dw	DosRet			; 29
		dw	DOS_GETDAT		; 2A
		dw	DOS_SETDAT		; 2B
		dw	DOS_GETTIM		; 2C
		dw	DOS_SETTIM		; 2D
		dw	DOS_SETRAW		; 2E
		dw	DOS_ABSREA		; 2F
		dw	DOS_ABSWRI		; 30

;------------------------------------------------------------------------------
; BIOS character I/O routines
;------------------------------------------------------------------------------

; CONST CP/M BIOS handler
ConStat:	ld	(SPSAVE),sp
		ld	sp,SysBoot
		call	DOS_SSBIOS		; check if keyboard input available
		ld	sp,(SPSAVE)
		ld	a,0
		ret	z			; no keyboard input, return 0
		dec	a			; keyboard input, return -1
		ret

; CONIN CP/M BIOS handler
ConIn:		ld	(SPSAVE),sp
		ld	sp,SysBoot
		call	DOS_SIN			; get keyboard input
		ld	sp,(SPSAVE)
		ret

; CONOUT CP/M BIOS handler
ConOut:		ld	a,c
		ld	(SPSAVE),sp
		ld	sp,SysBoot
		call	DOS_SOUT		; output to screen
		ld	sp,(SPSAVE)
		ret

;------------------------------------------------------------------------------
; Error / break handlers
;------------------------------------------------------------------------------

; disk error handler
DSK_ERR:	add	a,'A'			; drive id to drive letter
		bit	7,c			; bad FAT ?
		jr	nz,BAD_FAT		; nz=yes
		ld	(MsgError2),a		; store drive letter in error message
		ld	hl,MsgRead		; read string
		ld	de,MsgError1
		ld	a,c
		bit	0,a			; read ?
		ld	bc,4
		jr	z,dskerr1
		ld	hl,MsgWrit		; write string
dskerr1:	ldir
		cp	$0a			; write fault while reading ?
		ld	de,MsgMedia		; unsupported media message
		push	af			; store error
		jr	z,dskerr2		; z=yes, handle error
		and	$fe			; ignore read/write flag
		ld	de,MsgWrProtect		; write protect message
		jr	z,dskerr2		; write proctect, handle error
		cp	2			; not ready ?
		ld	de,MsgNotReady		; not ready message
		jr	z,dskerr2		; z=yes, handle error
		ld	de,MsgDisk		; disk message
dskerr2:	call	SPRTBUF
		ld	de,MsgError		; error drive message
		call	SPRTBUF
		pop	af			; restore error
		ld	c,2			; abort
		ret	z			; unsupported media, abort
dskerr3:	ld	de,MsgAbort		; abort/retry/ignore message
		call	SPRTBUF
		call	DOS_CONIN
		push	af
		call	DOS_CRLF
		pop	af
		and	$5f			; to upper
		ld	c,0			; ignore
		cp	'I'
		ret	z
		inc	C			; retry
		cp	'R'
		ret	z
		inc	c			; abort
		cp	'A'
		ret	z
		jr	dskerr3			; invalid input, retry

; handle bad FAT
BAD_FAT:	ld	(MsgDrive),a		; store drive letter in message
		ld	de,MsgBadFat		; bad FAT message
		call	SPRTBUF
		ld	c,2			; abort
		ret

; break handler
BRK_ERR:	ld	sp,SysBoot
		ld	a,(PFLAG)
		or	a			; console to printer ?
		jr	z,brkerr2		; z=no, skip
		ld	bc,1200
brkerr1:	push	bc
		call	DOS_CONSTA
		pop	bc
		dec	bc
		ld	a,c
		or	b
		jr	nz,brkerr1
brkerr2:	ld	a,3			; CTRL-C
		call	DOS_BUFOUT		; console output (with ^ for control characters)
		ld	a,(BATCHFLAG)
		or	a			; running a batch file ?
		jr	z,brkerr4		; z=no, skip
brkerr3:	ld	de,MsgTermBatch		; terminate batch file message
		call	SPRTBUF
		call	DOS_CONIN
		and	$5f			; to upper
		cp	'N'
		jr	z,brkerr4
		cp	'Y'
		jr	nz,brkerr3
		XOR	A
		ld	(BATCHFLAG),a		; not running a batch file
brkerr4:	jp	WBOOT			; start command processor

CommandM:	db	0
		db	"COMMAND COM"
		db	0,0
		dw	0
		db	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dw	0
		dw	0

; Messages part 2 of 2

MsgWrProtect:	db	CR,LF
		db	"Write protect"
		db	"$"

MsgNotReady:	db	CR,LF
		db	"Not ready"
		db	"$"

MsgMedia:	db	CR,LF
		db	"Unsupported media type"
		db	"$"

MsgDisk:	db	CR,LF
		db	"Disk"
		db	"$"

MsgError:	db	" error "
MsgError1:	db	"reading drive "
MsgError2:	db	"A"
		db	CR,LF
		db	"$"

MsgAbort:	db	"Abort, Retry or Ignore? "
		db	"$"

		DEPHASE

SysEnd:
SysSize		EQU	SysEnd-SysBegin
