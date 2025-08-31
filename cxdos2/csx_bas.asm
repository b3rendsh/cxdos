; ------------------------------------------------------------------------------
; csx_bas.asm
; Disk BASIC.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

; Sections:
; CS0_BAS	Disk BASIC handler
; CS1_BAS	Disk BASIC loader

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

; ------------------------------------------------------------------------------
; *** Disk BASIC handler ***
; ------------------------------------------------------------------------------

		SECTION	CS0_BAS

		PUBLIC	basCallHandler		; sys
		PUBLIC	basVecDisk		; ipl
		PUBLIC	basVecIgnore		; ipl
		PUBLIC	basVecBreak		; ipl

		EXTERN	GETSLT			; sys

basCallHandler:	ei
		ld	a,(H_POSD)
		cp	$c9			; is Disk BASIC initialized?
		scf
		ret	z			; z=no, cancel
		push	hl
		call	GETSLT
		ld	hl,MASTER
		cp	(hl)			; is this the MASTER disk system?
		jr	nz,_handler4		; nz=no, cancel
		ld	hl,StatementTab
_handler1:	ld	de,PROCNM
_handler2:	ld	a,(de)
		cp	(hl)			; compare statement name in jump table
		jr	nz,_handler3		; nz=not the same
		inc	de
		inc	hl
		and	a			; end of name?
		jr	nz,_handler2		; nz=no, compare next character
		ld	e,(hl)			; get routine address
		inc	hl
		ld	d,(hl)
		pop	hl

		; get BASIC character
		dec	hl
		ld	ix,CHRGTR
		call	CALBAS
		ei

		push	de
		pop	ix			; statement routine address
		ld	iy,(RAMAD1-1)		; select RAM page 1
		call	CALSLT			; execute statement routine
		and	a
		ret

_handler3:	ld	c,$ff
		xor	a
		cpir				; proceed to next statement
		inc	hl
		inc	hl
		cp	(hl)			; end of table?
		jr	nz,_handler1		; nz=no, next
_handler4:	pop	hl
		scf				; statement not in table
		ret

StatementTab:	db	"SYSTEM",0
		dw	CALL_SYSTEM
		db	"CHDRV",0
		dw	CALL_CHDRV
		db	"CHDIR",0
		dw	CALL_CHDIR
		db	"MKDIR",0
		dw	CALL_MKDIR
		db	"RMDIR",0
		dw	CALL_RMDIR
		db	0

; Error/abort vectors
basVecDisk:	dw	DiskError
basVecIgnore:	dw	DiskIgnore
basVecBreak:	dw	basBreak
basVecAbort:	dw	basAbort

; BDOS disk error handler
DiskError:	ld	c,2			; request warm start
DiskIgnore:	ret				; ignore abort

; BDOS abort handler
basBreak:	call	basAbort		; get orginal error code
		ld	IX,READYR		; restart diskBASIC
		jp	Z,CALBAS
		ld	ix,BasError
		ld	iy,(RAMAD1-1)
		jp	CALSLT

; BASIC abort handler
basAbort:	cp	_ABORT			; disk operation aborted ?
		jr	nz,_abort1		; nz=no
		ld	a,b			; orginal error code
_abort1:	or	a
		ret

; ------------------------------------------------------------------------------
; *** Disk BASIC loader ***
; ------------------------------------------------------------------------------

		SECTION	CS1_BAS

		PUBLIC	basTxtBasic		; ipl
		PUBLIC	basDiskBasic		; ipl
		PUBLIC	basBsave		; ipl
		PUBLIC	basBload		; ipl

		EXTERN	iplBootCxdos

basTxtBasic:	db	"CXDOS "
	IFNDEF DISKBASIC
		db	"no "
	ENDIF
		db	"Disk BASIC version 2.0",MOD1,MOD2,0

; Load and initialize Disk BASIC code in RAM page 1
basDiskBasic:	ld	a,(RAMAD1)
		ld	h,$40
		call	ENASLT
		ld	hl,DiskBasBegin
		ld	de,$4000
		ld	bc,DiskBasEnd-DiskBasBegin
		ldir

		; patch hooks Disk BASIC
		call	PatchHooks

		; load BASIC ROM in page 1
		ld	a,(EXPTBL+0)
		ld	h,$40
		call	ENASLT

		; load system RAM in page 2 and start Basic routine on stack
		ld	a,(RAMAD2)
		ld	h,$80
		jp	ENASLT

; ------------------------------------------------------------------------------
DiskBasBegin:
		PHASE	$4000

; Process BASIC character / execute BASIC routine
CheckChar:	call	GetChar
		ex	(sp),hl
		cp	(hl)
		jr	nz,_charerr
		inc	hl
		ex	(sp),hl
		inc	hl
GetChar:	dec	hl			; get BASIC character
GetNextChar:	ld	ix,CHRGTR		; get next BASIC character
CallBasic:	call	CALBAS			; execute BASIC routine
		ei
		ret

_charerr:	ld	e,2			; syntax error
		xor	a
		ld	(FLBMEM),a		; i/o channel mode = ascii
		ld	ix,ERROR
		jr	CallBasic

; Patch hooks for Disk BASIC system
PatchHooks:	ld	hl,BasPosd
		ld	de,H_POSD
		ld	bc,5
		ldir
		ld	hl,BasHooks
_patchhooks1:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,e
		or	d
		ret	z
		ex	de,hl
		ld	(hl),$f7		; instruction 'rst $30' (callF)
		inc	hl
		ld	a,(RAMAD1)		; slot
		ld	(hl),a
		inc	hl
		ex	de,hl
		ldi				; routine address
		ldi
		ld	a,$c9			; instruction 'ret'
		ld	(de),a
		jr	_patchhooks1

BasHooks:
	IFDEF DISKBASIC
		dw	H_DSKO,BAS_DSKO
		dw	H_DSKI,BAS_DSKI
		dw	H_NAME,BAS_NAME
		dw	H_KILL,BAS_KILL
		dw	H_COPY,BAS_COPY
		dw	H_DSKF,BAS_DSKF
		dw	H_LSET,BAS_LSET
		dw	H_RSET,BAS_RSET
		dw	H_FIEL,BAS_FIEL
		dw	H_MKIS,BAS_MKIS
		dw	H_MKSS,BAS_MKSS
		dw	H_MKDS,BAS_MKDS
		dw	H_CVI,BAS_CVI
		dw	H_CVS,BAS_CVS
		dw	H_CVD,BAS_CVD
		dw	H_GETP,BAS_GETP
		dw	H_NOFO,BAS_NOFO
		dw	H_NULO,BAS_NULO
		dw	H_NTFL,BAS_NTFL
		dw	H_BINS,BAS_BINS
		dw	H_BINL,BAS_BINL
		dw	H_FILE,BAS_FILE
		dw	H_DGET,BAS_DGET
		dw	H_FILO,BAS_FILO
		dw	H_INDS,BAS_INDS
		dw	H_LOC,BAS_LOC
		dw	H_LOF,BAS_LOF
		dw	H_EOF,BAS_EOF
		dw	H_BAKU,BAS_BAKU
		dw	H_PARD,BAS_PARD
		dw	H_NODE,BAS_NODE
		dw	H_ERRP,BAS_ERRP
		dw	H_PHYD,BAS_PHYD
	ENDIF
DataZero:	dw	$0000

BasPosd:	inc	sp
		inc	sp
		jp	PARDEV+8

; -----------------------------------------------------------------------------
; *** Disk BASIC call statements ***
; -----------------------------------------------------------------------------

CALL_SYSTEM:	ld	de,BUF+10
		jr	z,_system2
		call	CheckChar
		db	"("
		ld	ix,FRMEVL		; evaluate expression
		call	CallBasic
		push	hl
		ld	ix,FRESTR		; todo: ?
		call	CallBasic
		ld	c,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ld	de,BUF+10
		inc	c
		dec	c
		jr	z,_system1
		ld	b,$00
		ldir
_system1:	pop	hl
		call	CheckChar
		db	")"
		ret	nz
_system2:	xor	a
		ld	(de),a
		ld	ix,CLSALL		; close all i/o channels
		call	CallBasic
		call	TOTEXT
		call	ERAFNK
		ld	a,(MASTER)
		ld	h,$80
		call	ENASLT			; enable disk ROM on page 2
		ei
		ld	hl,BUF+10		; command line
		jp	iplBootCxdos		; start CXDOS from boot drive

; -----------------------------------------------------------------------------

	IFNDEF DISKBASIC
basBsave:
basBload:
CALL_CHDRV:
CALL_CHDIR:
CALL_MKDIR:
CALL_RMDIR:	ret

	ELSE

CALL_CHDRV:	call	ValParFile		; validate if ("parameter") is file/drive string
		push	hl
		ld	b,$00			; parse flag = no volume name
		ld	c,FPARSE		; parse pathname
		call	StrBasBdos
		ex	de,hl
		ld	a,b
		and	$05
		xor	$04
		or	(hl)
		jp	nz,Error05		; bad drive name error
		ld	a,c
		call	GetLogin
		dec	a
		ld	e,a
		ld	c,FSELDSK		; select disk
		call	BDOS
		pop	hl
		ret

CALL_CHDIR:	call	ValParFile		; validate if ("parameter") is file/drive string
		push	hl
		ld	c,FCHDIR		; change current directory
		call	StrBasBdos
		pop	hl
		ret

CALL_MKDIR:	call	ValParFile		; validate if ("parameter") is file/drive string
		push	hl
		ld	b,$10			; attributes = directory
		ld	c,FCREATE		; create file handle
		call	StrBasBdos
		call	FlushDiskBuf
		pop	hl
		ret

CALL_RMDIR:	call	ValParFile		; validate if ("parameter") is file/drive string
		ret	nz			; nz=no closing ')'
		push	hl
		ld	b,$10			; search attributes = directories
		call	FindFirst		; execute find first entry
		xor	a
		push	af
_rmdir1:	ld	a,(BUF+10+14)
		and	$10			; directory ?
		jr	z,_rmdir2		; z=no
		ld	de,BUF+10
		ld	c,FDELETE		; delete file or subdirectory
		call	BasBdos
		pop	af
		scf
		push	af
_rmdir2:	call	FindNext
		jr	nc,_rmdir1
		pop	af
		ld	a,$d6
		jp	nc,BasError
		call	FlushDiskBuf
		pop	hl
		ret

; ------------------------------------------------------------------------------
; *** BASIC hooks ***
; ------------------------------------------------------------------------------

; ---------------------------------------------------------
; Hook PHYD: physical disk i/o
; ---------------------------------------------------------
BAS_PHYD:	ei
		push	hl
		push	af
		call	_phyd2
		jr	c,_phyd1
		scf
		ld	a,$0c
		pop	hl
		pop	hl
		ret

_phyd1:		ld	l,a
		pop	af
		ld	a,l
		pop	hl
		push	hl
		ld	ix,A4010		; Driver DSKIO
		call	CALSLT
		pop	hl
		ret

_phyd2:		push	bc
		ld	(TARGET),a
		ld	hl,DRVTBL
		ld	b,4
_phyd3:		sub	(hl)
		jr	nc,_phyd4
		add	a,(hl)
		inc	hl
		ld	h,(hl)
		push	hl
		pop	iy
		pop	bc
		ret

_phyd4:		inc	hl
		inc	hl
		djnz	_phyd3
		pop	bc
		ret

; ---------------------------------------------------------
; Hook GETP: get pointer to i/o channel
; ---------------------------------------------------------
BAS_GETP:	ld	ix,RETRTN
		ld	iy,$0200
		call	StackControl
		pop	hl			; restore pointer to i/o channel
		ld	a,(hl)			; i/o channel mode
		and	a			; zx = i/o channel open
		ret

; ---------------------------------------------------------
; Hook NOFO: open without for
; ---------------------------------------------------------
BAS_NOFO:	ei
		ld	bc,256
		ld	(RECSIZE),bc		; default record size = 256
		call	GetChar			; get BASIC character
		ld	a,e
		ret	z
		push	af
		push	hl			; store BASIC pointer
		ld	l,12+2
		call	StackPos
		ld	a,(hl)
		cp	4			; i/o channel mode = random i/o ?
		jp	nz,Error11		; nz=no, syntax error
		inc	hl
		ld	a,(hl)			; device code
		cp	$09			; disk drive ?
		jp	nc,Error11		; nc=no, syntax error
		pop	hl			; restore BASIC pointer
		call	CheckChar
		db	$ff			; function token
		call	CheckChar
		db	$92			; LEN token
		call	CheckChar
		db	$ef			; = token
		ld	ix,INTID2
		call	CallBasic
		dec	de
		inc	d
		dec	d			; 0 or > 256 ?
		jp	nz,Error10		; nz=yes, illegal function call error
		inc	de
		ld	(RECSIZE),de		; store record size
		pop	af
		ret

; ---------------------------------------------------------
; Hook NULO: open i/o channel
; ---------------------------------------------------------
BAS_NULO:	ei
		ret	nc			; nc=not a disk device, quit
		ld	ix,RETRTN
		ld	iy,$0400
		call	StackControl
		call	CheckOpen		; check for file already open in one of the i/o channels
		ld	(PTRFIL),hl		; interpreter input/output device = i/o channel pointer
		inc	hl
		inc	hl			; +2

		xor	a
		ld	(hl),a			; record size = 1
		inc	hl			; +3
		ld	(hl),a			; clear backup character
		inc	hl			; +4
		ld	(hl),d			; update device
		inc	hl
		inc	hl			; +6
		ld	(hl),a			; position in buffer = 0
		ld	a,e
		push	af			; store i/o channel mode
		and	$82			; i/o channel mode binary save or sequential output ?
		jr	z,_nulo3		; z=no,
_nulo1:		XOR	A			; open mode = normal
		LD	B,A			; attributes = default
		LD	C,FCREATE		; create file handle
		call	StrBasBdos
_nulo2:		pop	af			; restore i/o channel mode
		ld	hl,(PTRFIL)
		ld	(hl),a			; update i/o channel mode
		inc	hl
		ld	(hl),b			; update file handle
		pop	af
		pop	hl
		ret

_nulo3:		ld	a,e
		cp	4			; i/o channel mode = random i/o ?
		jr	nz,_nulo4		; nz=no
		ld	hl,(PTRFIL)
		inc	hl
		inc	hl			; +2
		ld	a,(RECSIZE)
		dec	a
		ld	(hl),a			; update record size -1
		ld	de,(PATHNAM)
		xor	a
		ld	c,FOPEN			; open file handle
		call	BdosNF			; execute BDOS function (allow file not found), handle error
		jr	c,_nulo1		; file not found,
		jr	_nulo2

_nulo4:		cp	1			; i/o channel mode = sequential input ?
		jr	nz,_nulo6		; nz=no
		xor	a
		ld	c,FOPEN			; open file handle
		call	StrBasBdos
		ld	hl,FLBMEM
		xor	a
		cp	(hl)			; i/o channel in raw mode ?
		ld	(hl),a			; i/o channel mode = ascii
		jr	nz,_nulo2		; nz=yes, update mode,file handle and quit
		pop	af
		ld	hl,(PTRFIL)
		ld	(hl),a			; update i/o channel mode
		inc	hl
		ld	(hl),b			; update file handle
		dec	hl			; +0
		ex	de,hl
		ld	hl,6
		add	hl,de			; +6
		ld	(hl),$ff		; position in buffer = end of buffer
		push	hl
		ex	de,hl
		call	ReadCharIO		; read character from i/o channel
		pop	hl
		dec	hl
		dec	hl
		dec	hl			; +3
		ld	(hl),a			; update backup character
		inc	a			; binary save file id ?
		jr	nz,_nulo5		; nz=no
		inc	hl
		inc	hl
		inc	hl
		inc	hl			; +7
		ld	(hl),$80
_nulo5:		pop	af
		pop	hl
		ret

_nulo6:		xor	a
		ld	c,FOPEN			; open file handle
		call	StrBasBdos
		pop	af
		ld	hl,(PTRFIL)
		ld	(hl),1			; i/o channel mode = sequential input
		inc	hl
		ld	(hl),b			; update file handle
		dec	hl
		ex	de,hl
		ld	hl,6
		add	hl,de			; +6
		ld	(hl),$ff		; position in buffer = end of buffer
		ex	de,hl
		ld	bc,$0000
		ld	e,c
		ld	d,b
_nulo7:		push	hl
		push	de
		push	bc
		call	ReadCharIO		; read character from i/o channel
		pop	bc
		pop	de
		pop	hl
		jr	c,_nulo8		; end of file,
		inc	bc
		ld	a,c
		or	b
		jr	nz,_nulo7
		inc	de
		jr	_nulo7

_nulo8:		push	bc
		ld	(hl),$02
		inc	hl
		ld	b,(hl)
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		xor	a
		ld	(hl),a
		pop	hl
		ld	c,FSEEK			; move file handle pointer
		call	BDOS
		pop	af
		pop	hl
		ret

; ---------------------------------------------------------
; Hook INDS: read character from i/o channel
; ---------------------------------------------------------
BAS_INDS:	ld	ix,RETRTN
		ld	iy,$0600
		call	StackControl
		call	ReadCharIO		; read character from i/o channel
		jp	Reg3Return		; restore registers and quit

; ---------------------------------------------------------
; Hook BAKU: backup character to i/o channel
; ---------------------------------------------------------
BAS_BAKU:	ei
		push	hl
		ld	l,8+2
		call	StackPos
		ld	(hl),NOSKCR % 256
		inc	hl
		ld	(hl),NOSKCR / 256
		pop	hl
		inc	hl
		inc	hl
		inc	hl			; +3
		ld	(hl),c			; store backup character
		ret

; ---------------------------------------------------------
; Hook FILO: write character to i/o channel
; ---------------------------------------------------------
BAS_FILO:	ld	ix,RETRTN
		ld	iy,$0800
		call	StackControl
		ld	a,(hl)
		cp	2			; i/o channel mode = sequential output ?
		jp	nz,Error04		; nz=no, bad file mode error
		pop	af			; restore character
		push	af			; store character
		call	WriteCharIO		; write character to i/o channel
		jp	Reg4Return		; restore registers and quit

; ---------------------------------------------------------
; Hook NTFL: close i/o channel
; ---------------------------------------------------------
BAS_NTFL:	ld	ix,RETRTN
		ld	iy,$0400
		call	StackControl
		pop	hl			; restore pointer to i/o channel
		ld	a,(hl)
		sub	2			; i/o channel mode = sequential output ?
		jr	nz,_ntfl1		; nz=no, skip EOF
		push	hl			; store pointer to i/o channel
		ld	hl,FLBMEM
		cp	(hl)			; i/o channel in raw mode ?
		ld	(hl),a			; i/o channel mode = ascii
		pop	hl			; restore pointer to i/o channel
		jr	nz,_ntfl1		; nz=yes, skip EOF
		ld	(hl),4			; i/o channel mode = random i/o
		ld	a,$1A			; EOF
		call	WriteCharIO		; write character to i/o channel
		call	nz,FlushIO		; buffer not full, flush i/o channel buffer
_ntfl1:		xor	a
		cp	(hl)			; i/o channel open ?
		ld	(hl),a			; i/o channel closed
		push	af			; store i/o channel mode/status
		inc	hl			; +1
		ld	b,(hl)			; file handle
		ld	de,$0006
		add	hl,de			; +7
		ld	(hl),a
		ld	l,a
		ld	h,a
		ld	(PTRFIL),hl
		ld	c,FCLOSE		; close file handle
		call	BasBdos
		pop	af			; restore i/o channel mode/status
		pop	hl
		ret

; ---------------------------------------------------------
; Hook BINS: binary save
; ---------------------------------------------------------
BAS_BINS:	call	TakeControl		; take control from hook caller
		push	hl
		ld	ix,SCCPTR
		call	CallBasic
		LD	A,0FFH
		call	_loadsave5		; write byte to file handle
		ld	de,(TXTTAB)
		ld	hl,(VARTAB)
		and	a
		sbc	hl,de
		call	_loadsave7		; write bytes to i/o channel 0
		ld	(NLONLY),a		; not loading basic program, close i/o channels when requested
		pop	hl
		ld	ix,CLSFIL		; close i/o channel
		jp	CallBasic

; ---------------------------------------------------------
; Hook BINL: binary load
; ---------------------------------------------------------
BAS_BINL:	ld	ix,M739A
		ld	iy,$0200
		call	StackControl
		pop	af
		jp	z,Error04		; bad file mode error
		ld	ix,CLSALL		; close all i/o channels
		call	CallBasic
		ld	hl,-199
		add	hl,sp
		ld	de,(TXTTAB)
		sbc	hl,de
		jp	c,Error09		; out of memory error
		push	hl
		call	_loadsave8		; get file handle i/o channel 0
		push	bc
		xor	a			; relative to the beginning of the file
		ld	de,0
		ld	hl,1			; offset = 1
		ld	c,FSEEK			; move file handle pointer
		call	BasBdos
		pop	bc
		ld	de,(TXTTAB)
		pop	hl
		push	hl
		ld	c,FREAD			; read from file handle
		call	BasBdos
		pop	de
		push	hl
		and	a
		sbc	hl,de
		pop	hl
		jp	nc,Error09		; out of memory error
		ld	de,(TXTTAB)
		add	hl,de
		ld	(VARTAB),hl
		ld	ix,LINKER
		call	CallBasic
		ld	a,(FILNAM+0)
		and	a
		ret	nz
		ld	(NLONLY),a		; not loading basic program, close i/o channels when requested
		ld	hl,_binlrun
		ld	de,BUF+10
		ld	bc,$0005
		push	de
		ldir
		pop	hl
		ld	ix,NEWSTT
		jp	CallBasic

_binlrun:	db	$3A,$92			; :RUN
		db	0			; end of line
		dw	0			; end of program

; ---------------------------------------------------------
; Hook DSKI: disk sector input
; ---------------------------------------------------------
BAS_DSKI:	call	TakeControl		; take control from hook caller
		call	GetNextChar		; get next BASIC character
		call	CheckChar
		db	"("
		call	ValByte			; evaluate byte, check for "," and evaluate unsigned integer operand
		call	CheckChar
		db	")"
		push	hl
		ld	hl,NULSTR
		ld	(DAC+2),hl
		ld	a,3
		ld	(VALTYP),a
		ld	l,FRDABS		; absolute sector read
		jr	_dskio

; ---------------------------------------------------------
; Hook DSKO: disk sector output
; ---------------------------------------------------------
BAS_DSKO:	call	TakeControl		; take control from hook caller
		call	ValByte			; evaluate byte, check for "," and evaluate unsigned integer operand
		call	GetChar			; get BASIC character
		ret	nz
		push	hl
		ld	l,FWRABS		; absolute sector write
_dskio:		push	hl
		push	de
		push	bc
		ld	de,BUF+10
		ld	l,c			; store drive id
		ld	c,FDPARM		; get disk parameters
		call	BasBdos
		ld	de,(SDIRBUF)
		ld	c,FSETDTA		; set disk transfer address
		call	BDOS
		pop	hl
		dec	l
		ld	h,1
		pop	de
		pop	bc
		call	BasBdos
		pop	hl
		ret

; ---------------------------------------------------------
; Hook DGET:
; ---------------------------------------------------------
BAS_DGET:	ld	ix,RETRTN
		ld	iy,$0400
		call	StackControl
		ld	a,(hl)			; i/o channel mode
		cp	4			; random i/o ?
		jp	nz,Error04		; nz=no, bad file mode error
		ex	(sp),hl			; store pointer to i/o channel, restore basic pointer
		call	GetChar			; get BASIC character
		jr	z,_dget2		; z=end of statement
		call	CheckChar
		db	","
		ld	ix,FRMEVL		; evaluate expression
		call	CallBasic
		push	hl			; store BASIC pointer
		call	DacToInt		; convert DAC to 32 bit integer
		ld	a,c
		or	b
		or	l
		or	h			; record number = 0 ?
		jp	z,Error10		; z=yes, illegal function call error
		ld	a,c
		or	b
		dec	bc
		jr	nz,_dget1
		dec	hl			; record number zero based
_dget1:		ex	de,hl
		pop	hl			; restore BASIC pointer
		ex	(sp),hl			; store BASIC pointer, restore pointer to i/o channel
		push	hl			; store pointer to i/o channel
		push	de			; store high word
		inc	hl
		inc	hl			; +2
		ld	e,(hl)
		ld	d,0
		inc	de			; record size
		call	Multiply1
		pop	ix
		push	bc
		push	ix
		pop	bc			; store low word result, restore high word
		call	Multiply2
		ld	a,l
		or	h			; record within 32 bit limit ?
		jp	nz,Error10		; nz=no, illegal function call error
		ld	e,c
		ld	d,b
		pop	hl			; restore low word result
		pop	bc			; restore pointer to i/o channel
		push	bc			; store pointer to i/o channel
		inc	bc			; +1
		ld	a,(bc)
		ld	b,a			; file handle
		xor	a			; relative to the beginning of the file
		ld	c,FSEEK			; move file handle pointer
		call	BDOS
		pop	hl			; restore pointer to i/o channel
		ex	(sp),hl			; store pointer to i/o channel, restore BASIC pointer
_dget2:		ex	(sp),hl			; store BASIC pointer, restore pointer to i/o channel
		inc	hl			; +1
		ld	b,(hl)			; file handle
		inc	hl			; +2
		ld	e,(hl)
		ld	d,0
		inc	de			; record size
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl			; +9
		ex	de,hl
		pop	ix			; restore BASIC pointer
		pop	af			; restore GET/PUT flag
		push	ix			; store BASIC pointer
		and	a			; PUT ?
		ld	c,FREAD			; read from file handle
		jr	z,_dget3		; z=no, GET
		ld	c,FWRITE		; write to file handle
_dget3:		call	BasBdos
		jp	_file9			; restore BASIC pointer and output back to screen

; ---------------------------------------------------------
; Hook FIEL: field
; ---------------------------------------------------------
BAS_FIEL:	call	TakeControl		; take control from hook caller
		cp	'#'
		call	z,GetNextChar		; get next BASIC character
		ld	ix,GETBYT
		call	CallBasic		; evaluate byte operand
		jp	z,Error11		; end of statement, syntax error
		push	hl			; store BASIC pointer
		ld	ix,FILIDX
		call	CallBasic		; get pointer to i/o channel
		ld	e,l
		ld	d,h			; store pointer to i/o channel
		jp	z,Error15		; i/o channel closed, file not open error
		jp	c,Error10		; not a diskdrive device, illegal function call error
		ld	a,(hl)			; i/o channel mode
		cp	4			; random i/o ?
		jp	nz,Error04		; nz=no, bad file mode error
		inc	hl
		inc	hl			; +2
		ld	l,(hl)
		ld	h,0
		inc	hl			; record size
		ld	(BUF+10),hl		; store record size
		ld	hl,0
		ld	(BUF+12),hl		; total field size = 0
		ld	bc,$0009		; offset = to i/o channel buffer
		pop	hl			; restore BASIC pointer
_fiel1:		ex	de,hl
		add	hl,bc
		ex	de,hl			; update pointer in buffer
		ld	a,(hl)
		cp	','			; field definition follow ?
		ret	nz			; nz=no, quit
		push	de			; store pointer in buffer
		ld	ix,GTBYTC		; evaluate ,byte operand
		call	CallBasic
		push	af			; store field size
		call	CheckChar
		db	"A"
		call	CheckChar
		db	"S"
		ld	ix,PTRGET
		call	CallBasic		; get address of variable
		ld	ix,GETYPR
		call	CallBasic		; get DAC type
		jp	nz,Error08		; no string, type mismatch error
		pop	af			; restore field size
		ex	(sp),hl			; store BASIC pointer, restore pointer in buffer
		push	de			; store address of variable
		push	hl			; store pointer in buffer
		ld	hl,(BUF+12)
		ld	c,a
		ld	b,0			; field size
		add	hl,bc
		ld	(BUF+12),hl		; update total field size
		ex	de,hl
		ld	hl,(BUF+10)		; record size
		rst	R_DCOMPR		; compare
		jp	c,Error12		; total field size > record size, field overflow error
		pop	de			; restore pointer in buffer
		pop	hl			; restore address of variable
		ld	(hl),c			; size of string = field size
		inc	hl
		ld	(hl),e
		inc	hl
		ld	(hl),d			; pointer to string = pointer in buffer
		ld	b,0			; ?? b is already zero
		pop	hl			; restore basic pointer
		jr	_fiel1			; next field

; ---------------------------------------------------------
; Hook RSET: right set
; ---------------------------------------------------------
BAS_RSET:	db	$f6			; OR xx: skip next instruction

; ---------------------------------------------------------
; Hook LSET: left set
; ---------------------------------------------------------
BAS_LSET:	SCF				; LSET flag
		call	TakeControl		; take control from hook caller
		push	af			; store LSET flag
		ld	ix,PTRGET
		call	CallBasic		; get address of variable
		ld	ix,GETYPR
		call	CallBasic		; get DAC type
		jp	nz,Error08		; not a string, type mismatch error
		push	de			; store address of variable
		ld	ix,FRMEQL
		call	CallBasic		; evaluate =expression
		pop	bc			; restore address of variable
		ex	(sp),hl			; store BASIC pointer, restore LSET flag
		push	hl			; store LSET flag
		push	bc			; store address of variable
		ld	ix,FRESTR
		call	CallBasic		; free temporary string descriptor
		ld	b,(hl)			; size of string
		ex	(sp),hl			; store string descriptor, restore address of variable
		ld	a,(hl)
		ld	c,a			; size of field
		push	bc			; store size of string, size of field
		push	hl			; store address of variable
		push	af			; store string released
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to string
		or	a			; size of field = 0 ?
		jr	z,_lset5		; z=yes, quit
		ld	hl,(NULBUF)		; pointer to system i/o channel buffer
		dec	hl
		rst	R_DCOMPR		; compare
		jr	c,_lset2		; pointer to buffer, skip allocation
		ld	hl,(VARTAB)		; start of variable area
		rst	R_DCOMPR		; compare
		jr	c,_lset2		; not a string constant in program, skip allocation
		ld	e,c
		ld	d,0			; size of field
		ld	hl,(STKTOP)
		add	hl,de
		ex	de,hl
		ld	hl,(FRETOP)
		rst	R_DCOMPR
		jp	c,_lset7
		pop	af			; restore string released
_lset1:		ld	a,c			; size of field
		ld	ix,GETSPA
		call	CallBasic		; allocate string space
		pop	hl			; restore address of variable
		pop	bc			; restore size of string, size of field
		ex	(sp),hl			; store address of variable, restore string descriptor
		push	de
		push	bc
		ld	ix,FRESTR
		call	CallBasic		; free temporary string descriptor
		pop	bc
		pop	de
		ex	(sp),hl
		push	bc
		push	hl
		inc	hl
		push	af			; store string released
		ld	(hl),e
		inc	hl
		ld	(hl),d			; update pointer to string
_lset2:		pop	af			; restore string released
		pop	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to string
		pop	bc
		pop	hl
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a			; pointer to string
		ld	a,c
		cp	b			; compare size of string with size of field
		jr	nc,_lset3		; size of field >= size of string
		ld	b,a
_lset3:		sub	b
		ld	c,a			; size difference
		pop	af			; restore LSET flag
		call	nc,AddSpaces		; RSET, add spaces (before)
		inc	b
_lset4:		dec	b
		jr	z,_lset6
		ld	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		jr	_lset4

_lset5:		pop	bc
		pop	bc
		pop	bc
		pop	bc
		pop	bc			; clean up stack
_lset6:		call	c,AddSpaces		; LSET, add spaces (after)
		pop	hl
		ret

; Add spaces
AddSpaces:	ld	a,' '
		inc	c
_spaces1:		dec	c
		ret	z
		ld	(de),a
		inc	de
		jr	_spaces1

_lset7:		pop	af			; store string released
		pop	hl			; restore address of variable
		pop	bc			; restore size of string, size of field
		ex	(sp),hl			; store address of variable, restore string descriptor
		ex	de,hl
		jr	nz,_lset8		; string not released, skip allocation
		push	bc			; store size of string, size of field
		ld	a,b
		ld	ix,STRINI
		call	CallBasic		; allocate temp string
		call	mkds1
		pop	bc			; restore size of string, size of field
_lset8:		ex	(sp),hl			; store , restore address of variable
		push	bc			; store size of string, size of field
		push	hl			; store address of variable
		jp	_lset1			; continue

; ---------------------------------------------------------
; Hook MKI: make integer
; ---------------------------------------------------------
BAS_MKIS:	ld	a,2			; size of string = 2
		db	$01

; ---------------------------------------------------------
; Hook MKS: make single float
; ---------------------------------------------------------
BAS_MKSS:	ld	a,4			; size of string = 4
		db	$01

; ---------------------------------------------------------
; Hook MKD: make double float
; ---------------------------------------------------------
BAS_MKDS:	ld	a,8			; size of string = 8
		call	TakeControl		; take control from hook caller
		push	af			; store size of string
		ld	ix,DOCNVF
		call	CallBasic		; convert DAC to other type
		pop	af			; restore size of string
		ld	ix,STRINI
		call	CallBasic		; allocate temp string
		ld	hl,(DSCTMP+1)		; pointer to string
		call	VMOVMF			; copy variable content from DAC

; Subroutine 
mkds1:		ld	de,DSCTMP
		ld	hl,(TEMPPT)
		ld	(DAC+2),hl
		ld	a,3
		ld	(VALTYP),a
		call	VMOVE			; copy string descriptor
		ld	DE,FRETOP
		rst	R_DCOMPR
		ld	(TEMPPT),hl
		jp	z,Error07		; string formula too complex error
		ret

; ---------------------------------------------------------
; Hook CVI: convert to integer
; ---------------------------------------------------------
BAS_CVI:	ld	a,2-1			; target size -1
		db	$01

; ---------------------------------------------------------
; Hook CVS: convert to single float
; ---------------------------------------------------------
BAS_CVS:	ld	a,4-1			; target size -1
		db	$01

; ---------------------------------------------------------
; Hook CVD: convert to double float
; ---------------------------------------------------------
BAS_CVD:	ld	a,8-1			; target size -1
		call	TakeControl		; take control from hook caller
		push	af			; store target size -1
		ld	ix,FRESTR
		call	CallBasic		; free temporary string descriptor
		pop	af			; resture target size -1
		cp	(hl)			; field size to small ?
		jp	nc,Error10		; nc=yes, illegal function call error
		inc	a			; target size
		inc	hl
		ld	c,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,c			; pointer to string
		ld	(VALTYP),a		; target type
		jp	VMOVFM			; copy variable content to DAC

; ---------------------------------------------------------
; Hook EOF: end of file
; ---------------------------------------------------------
BAS_EOF:	call	TakeControl		; take control from hook caller
		push	hl
		call	ReadCharIO		; get character from i/o channel
		ld	hl,$0000
		jr	nc,_eof1		; not end of file, result = 0
		dec	hl			; end of file, result = -1
_eof1:		push	af
		call	MAKINT
		pop	af
		pop	hl
		inc	hl
		inc	hl
		inc	hl			; +3
		ld	(hl),a			; update backup character
		ret

; ---------------------------------------------------------
; Hook FILE:
; ---------------------------------------------------------
BAS_FILE:	call	TakeControl		; take control from hook caller
		call	GetChar			; get BASIC character
		jr	z,_file1
		cp	','
		jr	z,_file1
		call	ValFileStr		; evaluate file expression and check if disk drive
		jr	_file2

_file1:		xor	a
		ld	de,(PATHNAM)
		ld	(de),a
_file2:		call	GetChar			; get BASIC character
		scf
		jr	z,_file3
		call	CheckChar
		db	","
		call	CheckChar
		db	"L"
		and	a
_file3:		push	hl
		ld	a,(PRTFLG)
		inc	a
		dec	a
		push	af
		ld	ix,CRDONZ
		call	CallBasic
		pop	af
		push	af
		ld	a,$16
		jr	nc,_file4
		ld	a,$10			; search attributes = normal
_file4:		call	_open5
		ld	a,c
		add	a,'A'-1
		rst	R_OUTDO
		ld	a,':'
		rst	R_OUTDO
		ld	a,'\\'
		rst	R_OUTDO
		ld	de,BUF+75
		push	de
		ld	c,FWPATH		; get whole path string
		call	BasBdos
		ld	(hl),a
		dec	hl
		ld	(hl),a
		pop	hl
		call	_file19
		ld	ix,CRDO
		call	CallBasic
_file5:		pop	af
		push	af
		call	_file10
		call	CKCNTC
		pop	af
		push	af
		jr	nc,_file7
		ld	a,(LINLEN)
		ld	b,a
		ld	a,(TTYPOS)
		jr	z,_file6
		ld	b,80
		ld	a,(LPTPOS)
_file6:		and	a
		jr	z,_file8
		add	a,$0d
		cp	b
		jr	nc,_file7
		ld	a,$20
		rst	R_OUTDO
		jr	_file8

_file7:		ld	ix,CRDO
		call	CallBasic
_file8:		ld	ix,BUF+10
		ld	c,FFNEXT		; find next entry
		call	BDOS
		jr	z,_file5
		pop	af
_file9:		pop	hl			; restore basic pointer
		ld	ix,FINPRT
		jp	CallBasic		; output back to screen

; Subroutine 
_file10:	jr	nc,_file14

; Subroutine 
_file11:	ld	de,BUF+11
		ld	hl,(PATHNAM)
		ld	c,FPFILE		; parse filename
		call	BasBdos
		ld	b,$08
		call	_file13
		ld	a,(hl)
		cp	$20
		jr	z,_file12
		ld	a,'.'
_file12:	rst	R_OUTDO
		ld	b,$03

; Subroutine 
_file13:	ld	a,(hl)
		rst	R_OUTDO
		inc	hl
		djnz	_file13
		ret

_file14:	call	_file11
		ld	a,$20
		rst	R_OUTDO
		ld	a,(BUF+24)
		ld	c,a
		bit	4,c
		ld	a,$64
		call	_file17
		BIT	0,C
		ld	a,$72
		call	_file17
		bit	1,c
		ld	a,$68
		call	_file17
		bit	2,c
		ld	a,$73
		call	_file17
		bit	5,c
		ld	a,$61
		call	_file17
		ld	bc,(BUF+31+0)
		ld	hl,(BUF+31+2)		; file size
		call	_loc3
		ld	ix,JPFOUT
		call	CallBasic
		inc	hl
		push	hl
		ld	b,$0c
_file15:	ld	a,(hl)
		inc	hl
		dec	b
		and	a
		jr	nz,_file15
		ld	a,$20
_file16:	rst	R_OUTDO
		djnz	_file16
		pop	hl
		call	_file19
		ret

; Subroutine 
_file17:	jr	nz,_file18
		ld	a,$2d
_file18: 	rst	R_OUTDO
		ret

; Subroutine 
_file19:	ld	a,(hl)
		and	a
		inc	hl
		ret	z
		rst	R_OUTDO
		jr	_file19

; ---------------------------------------------------------
; Hook KILL: remove file
; ---------------------------------------------------------
BAS_KILL:	call	TakeControl		; take control from hook caller
		call	ValFileStr		; evaluate file expression and check if disk drive
		call	GetChar			; get BASIC character
		ret	nz
		push	hl
		call	FindFirstN		; find first entry (normal attributes)
		ld	c,FDELETE		; c=found, delete file or subdirectory
		jr	_name1

; ---------------------------------------------------------
; Hook NAME: rename file
; ---------------------------------------------------------
BAS_NAME:	call	TakeControl		; take control from hook caller
		call	ValFileStr		; evaluate file expression and check if disk drive
		push	hl
		call	FindFirstN		; execute find first entry (normal attributes)
		pop	hl
		call	CheckChar
		db	"A"
		call	CheckChar
		db	"S"
		call	ValFileStr		; evaluate file expression and check if disk drive
		push	hl
		ld	c,FRENAME		; rename file or subdirectory
_name1:		push	bc
		ld	de,BUF+10
		ld	hl,(PATHNAM)
		call	BasBdos
		call	FindNext
		pop	bc
		jr	nc,_name1
		call	FlushDiskBuf		; flush disk buffers
		pop	hl
		ret

; ---------------------------------------------------------
; Hook LOF:
; ---------------------------------------------------------
BAS_LOF:	LD	A,2
		db	011H

; ---------------------------------------------------------
; Hook LOC:
; ---------------------------------------------------------
BAS_LOC:	ld	a,1
		call	TakeControl		; take control from hook caller
		push	af			; store function
		ld	ix,CONINT
		call	CallBasic
		ld	ix,FILIDX
		call	CallBasic
		jp	c,Error10		; illegal function call error
		jp	z,Error15		; file not open error
		inc	hl
		ld	b,(hl)			; file handle

		inc	hl
		ld	c,(hl)			; record size -1
		ld	a,1			; relative to the current position
		ld	de,0
		ld	hl,0			; offset = 0
		push	bc			; store file handle, record size -1
		ld	c,FSEEK			; move file handle pointer
		call	BasBdos
		pop	bc			; restore file handle, record size -1
		pop	af			; restore function
		dec	a			; loc ?
		jr	nz,_loc1		; nz=no, return file size
		push	bc			; store file handle, record size -1
		call	_loc4			; calculate LOC value
		pop	bc			; restore file handle, record size -1
		jr	_loc2			; return LOC value

; Subroutine 
_loc1:		push	hl
		push	de			; store current position
		ld	a,2			; relative to the end of the file
		ld	de,0
		ld	hl,0			; offset = 0
		ld	c,FSEEK			; move file handle pointer
		push	bc			; store file handle
		call	BasBdos
		pop	bc			; restore file handle
		pop	ix
		ex	(sp),hl
		push	de
		push	ix
		pop	de			; store file size, restore orginal position
		xor	a			; relative to start of the file
		call	BasBdos
		pop	de
		pop	hl			; restore file size
_loc2:		ld	c,l
		ld	b,h
		ex	de,hl			; value to HLBC

; Subroutine 
_loc3:		push	bc			; store low word value
		ld	ix,FLTLIN
		call	CallBasic		; convert to single float (high word value)
		ld	bc,$6545
		ld	de,$6053
		call	SGNMUL			; * 65536
		ld	hl,DAC
		ld	de,ARG
		ld	bc,$0008
		ldir				; ARG = DAC
		pop	hl			; restore low word value
		ld	ix,FLTLIN
		call	CallBasic		; convert to single float (low word value)
		call	CONDS			; convert to double float
		jp	DECADD			; DAC = DAC + ARG

; Subroutine calculate LOC value
_loc4:		inc	c			; record size = 256 ?
		jr	nz,_loc5		; nz=no, divide
		ld	a,l
		ld	l,h
		ld	h,e
		ld	e,d
		ld	d,0
		jr	_loc9

_loc5:		xor	a
		ld	b,32+1
_loc6:		adc	a,a
		jr	c,_loc7
		cp	c
		ccf
		jr	nc,_loc8
_loc7:		sub	c
		scf
_loc8:		adc	hl,hl
		ex	de,hl
		adc	hl,hl
		ex	de,hl
		djnz	_loc6
_loc9:		or	a
		ret	z
		inc	l
		ret	nz
		inc	h
		ret	nz
		inc	e
		ret	nz
		inc	d
		ret

; ---------------------------------------------------------
; Hook DSKF: disk free
; ---------------------------------------------------------
BAS_DSKF:	call	TakeControl		; take control from hook caller
		ld	ix,CONINT
		call	CallBasic
		and	a
		call	nz,GetLogin
		ld	e,a
		ld	c,FALLOC		; get allocation information
		call	BDOS
		jp	MAKINT

; ---------------------------------------------------------
; Hook COPY: copy file
; COPY "source_file" TO "target_file"
; ---------------------------------------------------------
BAS_COPY:	call	TakeControl		; take control from hook caller
		call	ValFileStr		; source_file:  evaluate file expression and check if disk drive
		push	hl
		call	FindFirstN		; find first entry (normal attributes)
		pop	hl
		call	GetChar			; get BASIC character
		ld	a,$00
		push	hl
		ld	hl,(PATHNAM)
		ld	(hl),a
		pop	hl
		jr	z,_copy1
		call	CheckChar
		db	$D9			; TO token
		call	ValFileStr		; target_file: evaluate file expression and check if disk drive
		call	GetChar			; get BASIC character
		ret	nz
_copy1:		push	hl

		; copy (first/next) source_file to target_file
_copy2:		call	CKCNTC			; check if CTRL+STOP key pressed
		ld	de,BUF+10
		xor	a
		ld	c,FOPEN			; open file handle
		call	BasBdos
		ld	a,b
		ld	(BUF+138),a
		xor	a			; 0: get date and time
		ld	c,FHFTIME		; get/set file handle date and time
		call	BasBdos
		ld	(BUF+140),de
		ld	(BUF+142),hl
		ld	a,$ff
		ld	(BUF+139),a
		ld	hl,basVecAbort
		ld	(BREAKV),hl
		call	GetWorkArea		; bc=address hl=size (at least 256 bytes)
		ld	e,c
		ld	d,b
_copy3:		push	hl
		push	de

		; copy from source_file until workarea is full or EOF
_copy4:		ld	a,l
		or	h
		jr	z,_copy6
		push	hl
		push	de
		ld	a,(BUF+138)		; source_file handle
		ld	b,a
		ld	c,FREAD			; read from file handle
		call	BDOS
		jr	z,_copy5
		cp	_EOF			; end of file error ?
_copy5:		jp	nz,BdosAbort
		ex	de,hl
		pop	hl
		add	hl,de
		ex	(sp),hl
		sbc	hl,de
		ld	a,e
		or	d
		pop	de
		jr	nz,_copy4

		; copy to target_file
_copy6:		ex	de,hl
		pop	de
		push	de
		sbc	hl,de
		ld	a,(BUF+139)
		inc	a			; target_file already open?
		jr	nz,_copy7		; nz=yes

		; open target_file handle
		push	hl
		ld	hl,BUF+11
		ld	de,BUF+75
		ld	bc,13
		ldir
		ld	de,(PATHNAM)
		ld	b,$00
		ld	ix,BUF+74
		ld	c,FFNEW			; find new entry
		call	BdosHandler
		ld	de,BUF+74
		xor	a
		ld	c,FOPEN			; open file handle
		call	BdosHandler
		ld	a,b
		ld	(BUF+139),a
		pop	hl

		; write content in workarea to target_file
_copy7:		pop	de
		ld	a,(BUF+139)
		ld	b,a
		push	de
		ld	c,FWRITE		; write to file handle
		call	BdosHandler
		pop	de
		pop	bc
		sbc	hl,bc
		ld	l,c
		ld	h,b
		jr	nc,_copy3

		; target_file set date/time
		ld	a,(BUF+139)
		ld	b,a
		ld	a,$01			; 1: set date and time
		ld	ix,(BUF+140)
		ld	hl,(BUF+142)
		ld	c,FHFTIME		; get/set file handle date and time
		call	BdosHandler

		; close source_file and target_file
		ld	a,(BUF+138)
		ld	b,a
		ld	c,FCLOSE		; close file handle
		call	BdosHandler
		ld	a,(BUF+139)
		ld	b,a
		ld	c,FCLOSE		; close file handle
		call	BdosHandler
		ld	hl,basVecBreak
		LD	(BREAKV),hl

		; any more files to copy?
		call	FindNext
		jp	NC,_copy2
		pop	hl
		ret

; ---------------------------------------------------------
; Hook PARD: parse device
; ---------------------------------------------------------
BAS_PARD:	ei
		ld	a,':'
		cp	(hl)
		jp	z,Error14		; bad file name error
		push	hl
		push	de
		ld	a,e
		cp	$40
		jp	nc,Error14		; bad file name error
		ld	c,e
		ld	b,0
		ld	de,(PATHNAM)
		push	bc
		push	de
		ldir
		xor	a
		ld	(de),a
		pop	hl
		pop	bc
		cpir
		jp	z,Error14		; bad file name error
		ld	c,FPARSE		; parse pathname
		call	StrBasBdos
		ld	a,(de)
		cp	':'
		jr	nz,_pard1
		pop	de
		pop	hl
		ret

_pard1:		bit	2,b
		jr	nz,_pard2
		ld	c,$00
_pard2:		ld	a,b
		and	$c2
		jr	z,_pard3
		ld	a,(de)
		or	a
		jp	nz,Error14		; bad file name error
		pop	de
		ld	e,a
		push	de
		push	bc
		jr	_pard8

_pard3:		pop	de
		pop	hl
		ld	ix,(PATHNAM)
		bit	2,b
		jr	z,_pard4
		inc	hl
		inc	hl
		dec	e
		dec	e
		inc	ix
		inc	ix
_pard4:		push	hl
		push	de
		push	bc
		inc	e
		dec	e
		jr	z,_pard8
		ld	c,e
		ld	a,(hl)
		cp	' '
		jp	z,Error14		; bad file name error
		ld	b,$08
		call	_pard10
		jr	z,_pard7
		bit	1,d
		jr	z,_pard5
		dec	ix
_pard5:		ld	a,$2e
		ld	(ix),a
		inc	ix
		cp	(hl)
		jr	nz,_pard6
		inc	hl
		dec	c
		jr	z,_pard7
_pard6:		ld	b,c
		call	_pard10
		jp	nz,Error14		; bad file name error
_pard7:		ld	(ix),c
_pard8:		ld	l,12+2
		call	StackPos
		ld	(hl),RETRTN % 256
		inc	hl
		ld	(hl),RETRTN / 256
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	hl
		ld	hl,BSAVE+3
		rst	R_DCOMPR
		ld	bc,(BSVCHK)
		jr	z,_pard9
		ld	hl,BLOAD+3
		rst	R_DCOMPR
		ld	bc,(BLDCHK)
		jr	z,_pard9
		ld	c,e
		ld	b,d
_pard9:		pop	hl
		ld	(hl),b
		dec	hl
		ld	(hl),c
		pop	bc
		ld	a,c
		or	a
		call	nz,GetLogin
		pop	de
		pop	hl
		ret

; Subroutine
_pard10:	ld	d,$01
_pard11:	push	hl
		push	bc
		ld	e,(hl)
		ld	c,FCHKCHR		; check character
		call	BasBdos
		pop	bc
		pop	hl
		ld	a,e
		bit	4,d
		jr	nz,_pard13
		ld	(ix),a
		inc	ix
		inc	hl
		dec	c
		ret	z
		djnz	_pard11
		ret

_pard12:	ld	a,(hl)
_pard13:	cp	$20
		ret	nz
		inc	hl
		dec	c
		ret	z
		djnz	_pard12
		ret

; ---------------------------------------------------------
; Hook NODE: no device
; ---------------------------------------------------------
BAS_NODE:	ei
		ld	a,0			; device = default drive
		ret

; ---------------------------------------------------------
; Hook ERRP: get pointer to error string
; ---------------------------------------------------------
BAS_ERRP:	ei
		ld	a,e
		sub	$3c			; min basic error number?
		ret	c			; c=no
		cp	BASNMSG			; max BASIC error number?
		ret	nc			; nc=no
		inc	a
		ld	b,a
		ld	hl,BasMessages
_errp1:		ld	a,(hl)
		and	a
		inc	hl
		jr	nz,_errp1
		djnz	_errp1
		dec	hl
		ld	de,BUF+10
		push	de
		ld	bc,$001A
		ldir
		ld	e,01H
		pop	hl
		ret

; ---------------------------------------------------------
; *** Command BSAVE ***
; ---------------------------------------------------------
basBsave:	push	de			; store device code
		call	_loadsave2		; check for ',' and evaluate address operand
		ld	(SAVENT),de		; store start address
		push	de			; store start address
		call	_loadsave2		; check for ',' and evaluate address operand
		ld	(SAVEND),de		; store end address
		ex	(sp),hl			; store BASIC pointer, restore start address
		ex	de,hl
		rst	R_DCOMPR
		jp	c,Error10		; illegal function call error
		ex	de,hl
		ex	(sp),hl			; store start address, restore BASIC pointer
		call	GetChar			; get BASIC character
		scf				; source = RAM
		jr	z,_bsave2		; end of statement, source = RAM, continue
		call	CheckChar
		db	","
		cp	'S'			; VRAM ?
		jr	nz,_bsave1		; nz=no, evaluate execute address and continue
		call	GetNextChar		; get next BASIC character
		and	a			; source = VRAM
		jr	_bsave2			; continue

_bsave1:	call	_loadsave3		; evaluate address operand
		ld	(SAVENT),de		; store execute address
		scf				; source = RAM
_bsave2:	pop	bc			; restore start address
		jr	NC,_bsave3		; source = VRAM,
		inc	b
		dec	b			; start address in page 2 or 3 ?
		jp	p,Error10		; p=no, illegal function call error
_bsave3:	pop	de			; restore device code
		push	hl			; store basic pointer
		push	bc			; store start address
		push	af			; store source type
		xor	a			; i/o channel = 0
		ld	e,2			; file mode = sequential output
		ld	ix,OPNFIL
		call	CallBasic		; open i/o channel
		ld	a,$fe			; file id = BSAVE
		call	_loadsave5		; write byte to file handle
		pop	af			; restore source type
		pop	hl			; restore start address
		push	hl			; store start address
		push	af			; store source type
		call	_loadsave4		; write word to file handle
		ld	hl,(SAVEND)		; end address
		call	_loadsave4		; write word to file handle
		ld	hl,(SAVENT)		; execute address
		call	_loadsave4		; write word to file handle
		pop	af			; restore source type
		pop	bc			; restore start address
		push	af			; store source type
		ld	hl,(SAVEND)		; end address
		and	a
		sbc	hl,bc
		inc	hl			; number of bytes = end address - start address + 1
		pop	af			; restore source type
		jr	nc,_bsave5		; source = VRAM
		ld	e,c
		ld	d,b			; transfer address = start address
		call	_loadsave7		; write bytes to i/o channel 0
_bsave4:	ld	a,$ff
		ld	(FLBMEM),a		; i/o channel mode = raw
		xor	a			; i/o channel = 0
		ld	ix,CLSFIL
		call	CallBasic		; close i/o channel
		jp	_file9			; restore BASIC pointer and output back to screen

_bsave5:	call	VramWorkArea		; store VRAM start address and number of bytes, and get work area
_bsave6:	push	hl			; store size of work area
		ld	de,(SAVENT)		; number of bytes
		rst	R_DCOMPR		; number of bytes fits in work area ?
		push	af			; store fit flag
		push	bc			; store start of work area
		ld	c,l
		ld	b,h			; size of VRAM transfer = size of work area
		ld	hl,(SAVEND)
		push	hl			; store VRAM start address
		add	hl,bc
		ld	(SAVEND),hl		; update VRAM start address for next
		pop	hl			; restore VRAM start address
		pop	de			; restore start of work area
		push	de			; store start of work area
		call	LDIRMV			; copy VRAM to RAM
		pop	bc			; restore start of work area
		pop	af			; restore fit flag
		jr	nc,_bsave7		; fits, write remainer and finish
		pop	hl			; restore size of work area
		push	hl			; store size of work area
		push	bc			; store start of work area
		ld	e,c
		ld	d,b			; transfer address = start of work area
		call	_loadsave7		; write bytes to i/o channel 0
		pop	bc			; restore start of work area
		pop	de			; restore size of work area
		ld	hl,(SAVENT)
		and	a
		sbc	hl,de
		ld	(SAVENT),hl		; update number of bytes
		ex	de,hl			; size of work area
		jr	_bsave6			; next

_bsave7:	pop	hl			; restore size of work area
		ld	hl,(SAVENT)		; number of bytes
		ld	e,c
		ld	d,b			; transfer address = start of work area
		call	_loadsave7		; write bytes to i/o channel 0
		jr	_bsave4			; finish

; ---------------------------------------------------------
; *** Command BLOAD ***
; ---------------------------------------------------------
basBload:	push	de			; store device code
		xor	a
		ld	(RUNBNF),a		; assume destination = RAM without execute
		ld	c,a
		ld	b,a			; assume offset = 0
		call	GetChar			; get BASIC character
		jr	z,_bload3		; end of statement, use defaults
		call	CheckChar
		db	","
		cp	'R'			; execute ?
		jr	z,_bload1		; z=yes
		cp	'S'			; VRAM ?
		jr	nz,_bload2		; nz=no, evaluate offset and continue
_bload1:	ld	(RUNBNF),a		; update destination/execute flag
		call	GetNextChar		; get next BASIC character
		jr	z,_bload3		; end of statement, continue
		call	CheckChar
		db	","
_bload2:	call	_loadsave3		; evaluate address operand
		ld	b,d
		ld	c,e			; store offset
_bload3:	pop	de			; restore device code
		push	hl			; store BASIC pointer
		push	bc			; store offset
		ld	a,$ff
		ld	(FLBMEM),a		; i/o channel mode = raw
		xor	a			; i/o channel = 0
		ld	e,1			; file mode = sequential input
		ld	ix,OPNFIL
		call	CallBasic		; open i/o channel
		call	_loadsave6		; read byte from file handle
		cp	$fe			; BSAVE file id ?
		jp	nz,Error04		; nz=no, bad file mode error
		pop	bc			; restore offset
		call	_loadsave1		; read word from file handle, adjust with offset
		push	hl			; store start address
		call	_loadsave1		; read word from file handle, adjust with offset
		push	hl			; store end address
		call	_loadsave1		; read word from file handle, adjust with offset
		ld	(SAVENT),hl		; store execute address
		pop	hl			; restore end address
		pop	bc			; restore start address
		and	a
		sbc	hl,bc
		inc	hl			; number of bytes = end - start + 1
		ld	a,(RUNBNF)
		cp	'S'			; destination = VRAM ?
		jr	z,_bload5		; z=yes
		ld	e,c
		ld	d,b			; transfer address = start address
		call	_loadsave8		; get file handle i/o channel 0
		ld	c,FREAD			; read from file handle
		call	BasBdos
_bload4:	ld	ix,FINPRT
		call	CallBasic		; output back to screen
		pop	hl			; restore BASIC pointer
		ret

_bload5:	call	VramWorkArea		; store VRAM start address and number of bytes, and get work area
_bload6:	push	hl			; size of work area
		push	bc			; start of work area
		ld	de,(SAVENT)		; number of bytes
		rst	R_DCOMPR		; fits in work area ?
		push	af			; store fit flag
		ld	e,c
		ld	d,b			; transfer address = start of work area
		call	_loadsave8		; get file handle i/o channel 0
		ld	c,FREAD			; read from file handle
		call	BDOS
		pop	af			; restore fit flag
		pop	hl			; restore start of work area
		pop	bc			; restore size of work area
		push	bc			; store size of work area
		push	hl			; store start of work area
		push	af			; store fit flag
		ld	hl,(SAVEND)
		push	hl			; store VRAM address
		add	hl,bc
		ld	(SAVEND),hl		; update VRAM address for next
		pop	de			; restore VRAM address
		pop	af			; restore fit flag
		pop	hl			; restore start of work area
		jr	nc,_bload7		; fit, copy remainer to VRAM and finish
		push	hl			; store start of work area
		call	LDIRVM			; copy RAM to VRAM
		pop	bc			; restore start of work area
		pop	de			; restore size of work area
		ld	hl,(SAVENT)
		and	a
		sbc	hl,de
		ld	(SAVENT),hl		; update number of bytes
		ex	de,hl			; size of work area
		jr	_bload6			; next

_bload7:	pop	bc			; restore size of work area
		ld	bc,(SAVENT)		; number of bytes
		call	LDIRVM			; copy RAM to VRAM
		xor	a
		ld	(RUNBNF),a		; clear flag
		jr	_bload4			; finish

; Subroutine read word from file handle, adjust with offset
_loadsave1:	push	bc			; store offset
		call	_loadsave6		; read byte from file handle
		push	af			; store low byte
		call	_loadsave6		; read byte from file handle
		ld	h,a			; high byte
		pop	af
		ld	l,a			; low byte
		pop	bc			; restore offset
		add	hl,bc			; word + offset
		ret

; Subroutine check for ',' and evaluate address operand
_loadsave2:	call	CheckChar
		db	","

; Subroutine evaluate address operand
_loadsave3:	ld	ix,ADRGET
		jp	CallBasic

; Subroutine write word to file handle
_loadsave4:	push	hl
		ld	a,l
		call	_loadsave5		; write byte to file handle
		pop	af

; Subroutine write byte to file handle
_loadsave5:	ld	c,FWRITE		; write to file handle
		db	$21

; Subroutine read byte from file handle
_loadsave6:	ld	c,FREAD			; read from file handle
		call	_loadsave8		; get file handle i/o channel 0
		push	af			; store byte (bogus for read)
		ld	hl,1
		add	hl,sp
		ex	de,hl			; transfer address = high byte
		ld	hl,1			; number of bytes = 1
		push	bc			; store file handle
		call	BasBdos
		pop	bc			; restore file handle
		pop	af			; restore byte from file handle
		ret

; Subroutine write bytes to i/o channel 0
_loadsave7:	call	_loadsave8		; get file handle i/o channel 0
		ld	c,FWRITE		; write to file handle
		jp	BasBdos

; Subroutine get file handle i/o channel 0
_loadsave8:	push	hl
		ld	hl,(FILTAB)
		ld	b,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,b			; pointer to i/o channel
		inc	hl
		ld	b,(hl)			; file handle
		pop	hl
		ret

; ------------------------------------------------------------------------------
; *** Subroutines ***
; ------------------------------------------------------------------------------

; Disk BASIC runs in RAM in page 1. All stack manipulation code must check
; if the RAM segment RAMAD1 is in an expanded slot, not the MASTER ROM.
; Add another 2 bytes in the offset for the call to this routine.
; Input:  l  = offset
; Output: hl = stack position
StackPos:	ld	a,(RAMAD1)
		ld	h,0
		add	a,a
		ld	a,h
		jr	nc,_stackpos1		; nc=not expanded
		ld	a,8
_stackpos1:	add	a,l
		ld	l,a
		add	hl,sp
		ret

; Subroutine
GetLogin:	push	af
		ld	c,FLOGIN		; get login vector
		call	BDOS
		pop	af
		push	af
_login1:	srl	h
		rr	l
		dec	a
		jr	nz,_login1
		jp	nc,Error05		; bad drive name error
		pop	af
		ret

; Check for file already open in one of the i/o channels
CheckOpen:	push	hl
		push	de
		push	bc
		xor	a
		ld	(BUF+10),a
		ld	hl,(FILTAB)
		ld	a,(MAXFIL)		; number of user i/o channels
_open1:		push	af
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to i/o channel
		inc	hl
		push	hl			; store pointer in filtab
		ex	de,hl
		ld	a,(hl)
		and	a			; i/o channel open ?
		jr	z,_open3		; z=no, next
		inc	hl
		inc	hl
		inc	hl
		inc	hl			; +4
		ld	a,(hl)			; device
		cp	$09			; disk drive ?
		jr	nc,_open3		; nc=no, next
		ld	a,(BUF+10)
		and	a
		jr	nz,_open2
		push	hl
		ld	de,(PATHNAM)
		ld	b,$06			; search attributes = hidden, system
		ld	ix,BUF+10
		ld	c,FFFIRST		; find first entry
		call	BdosNF
		pop	hl
		jr	c,_open4		; c=file not found, quit
_open2:		dec	hl
		dec	hl
		dec	hl			; +1
		ld	b,(hl)			; file handle
		ld	de,BUF+10
		ld	c,FHTEST		; test file handle
		call	BasBdos
		ld	a,b
		and	a			; same file ?
		jp	nz,Error13		; nz=yes, file already open error
_open3:		pop	hl			; restore pointer in FILTAB
		pop	af
		dec	a			; more i/o channels ?
		jp	p,_open1		; p=yes, next
		jp	Reg3Return		; restore registers and quit

_open4:		pop	hl
		pop	hl
		jp	Reg3Return		; restore registers and quit

; Subroutine
_open5:		push	af			; store search attributes
		ld	b,$00			; parse flag = no volume name
		ld	c,FPARSE		; parse pathname
		call	StrBasBdos
		pop	af			; restore search attributes
		push	bc
		push	de
		push	hl
		push	af			; store search attributes
		ld	b,a			; search attributes
		call	FindFirst
		pop	af			; restore search attributes
		pop	hl
		pop	de
		pop	bc
		bit	5,b
		ret	nz
		ld	d,a			; store search attributes
		ld	a,(BUF+24)
		and	$10
		ret	z
		ld	a,l
		cp	e
		ret	z
		push	bc
		ld	b,d			; search attributes
		ld	de,BUF+10
		push	de
		pop	ix
		ld	hl,DataZero
		ld	c,FFFIRST		; find first entry
		call	BasBdos
		pop	bc
		ret

; Find first entry
FindFirstN:	ld	b,0			; search attributes = normal
FindFirst:	ld	c,FFFIRST		; find first entry
		jp	FibBasBdos

; Find next entry
FindNext:	ld	ix,BUF+10
		ld	c,FFNEXT		; find next entry

; Execute BDOS function, allow file not found and handle other error
BdosNF:		call	BDOS
		ret	z
		cp	_NOFIL			; file not found ?
		scf
		ret	z			; z=yes, return with Cx set
		jp	BasError		; handle error

; Subroutine take control from caller (move parameters on stack)
; Input: IX = return address replacement, IYH = number of bytes to move
;
; This is what the stack looks like at entry:
; prim	exp
; +0	+0	return address StackControl caller
; +2	+2	callF BIOS registers
; +6	+14	return address callF caller
; +8	+16	return address hook caller

StackControl:	ei
		push	hl
		push	de
		push	bc
		push	af
		ld	l,16+2
		call	StackPos
		push	ix
		pop	bc
		ld	(hl),c
		inc	hl
		ld	(hl),b
		ld	hl,10
		add	hl,sp
		ex	de,hl
		jr	_control3
_control1:	push	iy
		pop	bc
_control2:	ld	c,(hl)
		ld	a,(de)
		ld	(hl),a
		ld	a,c
		ld	(de),a
		inc	hl
		inc	de
		djnz	_control2
_control3:	ld	l,18+2
		call	StackPos
		ld	a,e
		sub	l
		ld	a,d
		sbc	a,h
		jr	c,_control1
Reg4Return:	pop	af
Reg3Return:	pop	bc
		pop	de
		pop	hl
		ret

; Subroutine read character from i/o channel
ReadCharIO:	push	hl			; store pointer to i/o channel
		ld	a,(hl)
		cp	1			; i/o channel mode = sequential input ?
		jp	nz,Error04		; nz=no, bad file mode error
		ld	e,l
		ld	d,h			; store pointer to i/o channel
		inc	hl
		inc	hl
		inc	hl			; +3
		ld	a,(hl)
		and	a			; backup character ?
		jr	nz,_readchar4		; nz=yes, use backup character
		inc	hl
		inc	hl
		inc	hl			; +6
		inc	(hl)			; update position in buffer
		ld	a,(hl)			; postition in buffer
		inc	hl
		inc	hl
		inc	hl			; +9
		jr	nz,_readchar2		; still in buffer,
		push	hl			; store pointer to i/o channel buffer
		ex	de,hl			; transfer address = i/o channel buffer
		inc	hl			; +1
		ld	b,(hl)			; file handle
		ld	hl,256			; number of bytes = 256
		ld	c,FREAD			; read from file handle
		call	BDOS
		jr	z,_readchar1		; no error, continue
		cp	_EOF			; end of file error ?
		jp	nz,BasError		; nz=no, handle error
		pop	hl			; restore pointer to i/o channel buffer
		ld	(hl),$1A		; put EOF character in buffer
		jr	_readchar3		; continue

_readchar1:	ld	c,l
		ld	b,h			; number of bytes read
		dec	h
		ld	a,l
		or	h			; 256 bytes read ?
		pop	hl			; restore pointer to i/o channel buffer
		jr	z,_readchar2		; z=yes
		push	hl			; store pointer to i/o channel buffer
		ld	e,l
		ld	d,h
		inc	d
		dec	de			; end of the i/o channel buffer
		add	hl,bc
		ld	a,c			; store number of bytes in buffer
		dec	hl			; end of data in i/o channel buffer
		lddr
		dec	hl
		dec	hl			; +6
		neg
		ld	(hl),a			; update position in buffer
		pop	hl			; restore pointer to i/o channel buffer
_readchar2:	ld	c,a
		ld	b,0			; position in buffer
		add	hl,bc			; pointer to character in buffer
_readchar3:	ld	a,(hl)			; get character
_readchar4:	sub	$1a
		sub	1			; EOF character (set Cx) ?
		ld	a,(hl)			; get character
		pop	hl			; restore pointer to i/o channel
		inc	hl
		inc	hl
		inc	hl			; +3
		ld	(hl),0			; clear backup character
		ret	nc			; not end of file, quit
		ld	(hl),a			; end of file, backup character = eof
		ret

; Subroutine write character to i/o channel
WriteCharIO:	push	hl			; store pointer to i/o channel
		ld	bc,6
		add	hl,bc			; +6
		ld	c,(hl)			; position in buffer
		inc	(hl)			; update position in buffer
		inc	hl
		inc	hl
		inc	hl			; +9
		add	hl,bc			; pointer in buffer
		ld	(hl),a			; put character in buffer
		pop	hl			; restore pointer to i/o channel
		ret	nz			; buffer not full, quit

; Subroutine flush i/o channel buffer
FlushIO:	push	hl			; store pointer to i/o channel
		inc	hl			; +1
		ld	b,(hl)			; file handle
		ld	de,5
		add	hl,de			; +6
		ld	a,(hl)			; position in buffer
		inc	hl
		inc	hl
		inc	hl			; +9
		ex	de,hl			; transfer address = i/o channel buffer
		dec	a
		ld	l,a
		ld	h,0
		inc	hl			; number of bytes
		ld	c,FWRITE		; write to file handle
		call	BasBdos
		pop	hl			; restore pointer to i/o channel
		ret

; Subroutine evaluate byte, check for "," and evaluate unsigned integer operand
ValByte:	ld	ix,GETBYT		; evaluate byte operand
		call	CallBasic
		push	de			; store byte
		call	CheckChar
		db	","
		ld	ix,GETUIN		; evaluate unsigned integer operand
		call	CallBasic
		pop	bc			; restore byte
		ret

; Subroutine convert DAC to 32 bit integer
DacToInt:	ld	ix,GETYPR
		call	CallBasic		; get DAC type
		jp	m,_toint1			; integer,
		jp	z,Error08		; string, type mismatch error
		ld	hl,DAC+0
		ld	a,(hl)
		and	a			; positive float ?
		jp	m,Error10		; m=no, illegal function call error
		ld	de,BUF+10
		ld	bc,8
		ldir				; store DAC
		ld	hl,IntTable
		ld	de,ARG
		ld	c,8
		ldir				; 65536 (double float)
		call	DECDIV			; DAC = DAC / ARG
		and	a			; DAC type = double float
		call	INT			; integer part of float
		ld	ix,GETUI
		call	CallBasic		; convert to unsigned integer
		push	de			; store high word value
		ex	de,hl
		ld	ix,FLTLIN
		call	CallBasic		; convert to single float
		call	CONDS			; convert to double float
		ld	bc,$6545
		ld	de,$6053
		call	SGNMUL			; * 65536
		ld	hl,DAC
		push	hl
		ld	de,ARG
		ld	bc,8
		ldir				; ARG = DAC
		ld	hl,BUF+10
		pop	de
		ld	c,8
		ldir				; restore DAC
		call	DECSUB			; DAC = DAC - ARG
		ld	ix,GETUI
		call	CallBasic		; convert to unsigned integer
		ld	c,e
		ld	b,d			; low word value
		pop	hl			; restore high word value
		ret

_toint1:	ld	bc,(DAC+2)		; integer in DAC
		inc	b
		dec	b			; positive integer ?
		jp	m,Error10		; m=no, illegal function call error
		ld	hl,0			; high word value = 0
		ret

IntTable:	db	$45,$65,$53,$60,$00,$00,$00,$00

; Validate if parameter is file expression and check if disk drive
ValParFile:	call	CheckChar
		db	"("
		call	ValFileStr
		call	CheckChar
		db	")"
		ret

ValFileStr:	ld	ix,FILEVL		; evaluate file expression
		call	CallBasic
		ld	a,d
		cp	$09			; device = disk drive ?
		ret	c			; c=yes, quit
		jp	Error05			; bad drive name error

; Store VRAM start address and number of bytes, and get work area
VramWorkArea:	ld	(SAVENT),hl		; store number of bytes
		ld	(SAVEND),bc		; store VRAM start address

; Get work area
; Output: bc = start address
;         hl = size of workarea (at least 256 bytes)
GetWorkArea:	ld	hl,-512
		add	hl,sp
		jr	nc,_getwork1
		ld	bc,(STREND)
		and	a
		sbc	hl,bc
		jr	c,_getwork1
		ld	a,h
		and	a
		ret	nz
_getwork1:	ld	bc,(NULBUF)
		ld	hl,256
		ret

; Subroutine
BdosHandler:	call	BDOS
		ret	z			; z=no error
BdosAbort:	push	af
		ld	hl,basVecBreak		; handle orginal BDOS error, restart DiskBASIC when none
		ld	(BREAKV),hl		; install BDOS abort handler
		ld	a,(BUF+138)
		ld	b,a
		ld	c,FCLOSE		; close file handle
		call	BDOS
		ld	a,(BUF+139)
		ld	b,a
		inc	a
		ld	c,FCLOSE		; close file handle
		call	nz,BDOS
		pop	af
		jr	BasError

; Flush disk buffers
FlushDiskBuf:	ld	b,$ff			; drive = all
		ld	d,0			; flush only
		ld	c,FFLUSH		; flush disk buffers
		jr	BasBdos

; Execute BDOS function
FibBasBdos:	ld	ix,BUF+10 		; result FIB in BUF
StrBasBdos:	ld	de,(PATHNAM)		; ASCIIZ string in buffer
BasBdos:	call	BDOS
		ret	z			; z=no error

BasError:	push	af			; store error code
		call	FlushDiskBuf		; flush disk buffers
		pop	af			; restore error code
		cp	$9f			; CTRL-STOP pressed ?
		jr	z,Error01		; z=yes
		cp	$9e			; CTRL-C pressed ?
Error01:	ld	ix,READYR
		jp	z,CallBasic		; z=yes, restart diskBASIC
		ld	e,a			; store error code
Error02:	cp	$ba			; DOS2 error code which need translation to BASIC error code ?
		jr	c,Error03		; c=no
		ld	c,a
		ld	b,0
		ld	hl,DskBasError-$ba
		add	hl,bc
		ld	a,(hl)			; translate BASIC error code
		ld	e,a			; store error code
		cp	$3c			; need to close i/o channels ?
		jr	c,Error06		; c=no, skip
Error03:	db	$01			; skip next instruction
Error04:	ld	e,$3d
		db	$01
Error05:	ld	e,$3e
		xor	a
		ld	(NLONLY),a		; not loading basic program, close i/o channels when requested
		push	de			; store BASIC error code
		ld	ix,CLSFIL
		call	CallBasic		; close i/o channel
		pop	de			; restore BASIC error code
Error06:	db	$01
Error07:	ld	e,$10
		db	$01
Error08:	ld	e,$0d
		db	$01
Error09:	ld	e,$07
		db	$01
Error10:	ld	e,$05
		db	$01
Error11:	ld	e,$02
		db	$01
Error12:	ld	e,$32
		db	$01
Error13:	ld	e,$36
		db	$01
Error14:	ld	e,$38
		db	$01
Error15:	ld	e,$3B
		xor	a
		ld	(FLBMEM),a		; i/o channel mode = ascii
		ld	IX,ERROR
		jp	CallBasic

; Take control from hook caller
TakeControl:	ei
		push	hl
		push	af
		ld	l,12+2
		call	StackPos
		ld	(hl),RETRTN % 256
		inc	hl
		ld	(hl),RETRTN / 256
		pop	af
		pop	hl
		ret

; Multiply
Multiply1:	ld	hl,$0000
Multiply2:	ld	a,$11
_multiply01:	rr	b
		rr	c
		dec	a
		ret	z
		jr	nc,_multiply02
		add	hl,de
_multiply02:	rr	h
		rr	l
		jr	_multiply01

; ------------------------------------------------------------------------------
; Error messages
; ------------------------------------------------------------------------------

BasMessages:	db	0
		db	"Bad FAT",0
		db	"Bad file mode",0
		db	"Bad drive name",0
		db	"Bad sector number",0
		db	"File still open",0
		db	"File already exists",0
		db	"Disk full",0
		db	"Too many files",0
		db	"Disk write protected",0
		db	"Disk I/O error",0
		db	"Disk offline",0
		db	"Rename across disk",0
		db	"File write protected",0
		db	"Directory already exists",0
		db	"Directory not found",0
BASNMSG		equ	15

; Tramslate DOS errors to BASIC errors
DskBasError:	db	$ba
		db	$3e		; .NRAMD -> Bad drive name
		db	$4b		; .RAMDX -> RAM disk already exists
		db	$bd
		db	$be
		db	$bf
		db	$c0
		db	$c1
		db	$c2
		db	$c3
		db	$c4
		db	$c5
		db	$c6
		db	$37		; .EOF -> Input past end
		db	$3c		; .FILE -> Bad allocation table
		db	$c9
		db	$40		; .FOPEN -> File still open
		db	$41		; .FILEX -> File already exists
		db	$49		; .DIRX -> Directory already exists
		db	$41		; .SYSX -> File already exists
		db	$38		; .DOT -> Bad file name
		db	$cf
		db	$41		; .DIRNE -> File already exists
		db	$48		; .FILRO -> File write protected
		db	$d2
		db	$41		; .DUPF -> File already exists
		db	$42		; .DKFUL -> Disk full
		db	$43		; .DRFUL -> Too many files
		db	$4a		; .NODIR -> Directory not found
		db	$35		; .NOFIL -> File not found
		db	$38		; .PLONG -> Bad filename
		db	$38		; .IPATH -> Bad filename
		db	$38		; .IFNM -> Bad filename
		db	$3e		; .IDRV -> Bad drivename
		db	$dc
		db	$dd
		db	$de
		db	$df
		db	$e0
		db	$e1
		db	$e2
		db	$e3
		db	$e4
		db	$e5
		db	$e6
		db	$e7
		db	$e8
		db	$e9
		db	$ea
		db	$eb
		db	$ec
		db	$ed
		db	$ee
		db	$ef
		db	$f0
		db	$f1
		db	$3c		; .IFAT -> Bad allocation table
		db	$45		; .SEEK -> Disk I/O error
		db	$45		; .WFILE -> Disk I/O error
		db	$45		; .WDISK -> Disk I/O error
		db	$45		; .NDOS -> Disk I/O error
		db	$45		; .UFORM -> Disk I/O error
		db	$44		; .WPROT -> Disk write protected
		db	$45		; .RNF -> Disk I/O error
		db	$45		; .DATA -> Disk I/O error
		db	$45		; .VERFY -> Disk I/O error
		db	$46		; .NRDY -> Disk offline
		db	$45		; .DISK -> Disk I/O error
		db	$45		; .WRERR -> Disk I/O error
		db	$45		; .NCOMP -> Disk I/O error

	ENDIF ; DISKBASIC

		DEPHASE

DiskBasEnd:
; ------------------------------------------------------------------------------
