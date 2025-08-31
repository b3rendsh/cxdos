; ------------------------------------------------------------------------------
; cs0_fhs.asm
; Disk functions msxdos 2 (fhs)
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS0_FHS

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_DPARM			; 31 Get disk parameters
		PUBLIC	F_FFIRST		; 40 Find first entry
		PUBLIC	F_FNEXT			; 41 Find next entry
		PUBLIC	F_FNEW			; 42 Find new entry
		PUBLIC	F_OPEN			; 43 Open file handle
		PUBLIC	F_CREATE		; 44 Create file handle
		PUBLIC	F_CLOSE			; 45 Close file handle
		PUBLIC	F_ENSURE		; 46 Ensure file handle
		PUBLIC	F_DUP			; 47 Duplicate file handle
		PUBLIC	F_READ			; 48 Read from file handle
		PUBLIC	F_WRITE			; 49 Write to file handle
		PUBLIC	F_SEEK			; 4A Move file handle pointer
		PUBLIC	F_IOCTL			; 4B I/O control for devices
		PUBLIC	F_HTEST			; 4C Test file handle
		PUBLIC	F_DELETE		; 4D Delete file or subdirectory
		PUBLIC	F_RENAME		; 4E Rename file or subdirectory
		PUBLIC	F_MOVE			; 4F Move file or subdirectory
		PUBLIC	F_ATTR			; 50 Get/set file attributes
		PUBLIC	F_FTIME			; 51 Get/set file date and time
		PUBLIC	F_HDELETE		; 52 Delete file handle
		PUBLIC	F_HRENAME		; 53 Rename file handle
		PUBLIC	F_HMOVE			; 54 Move file handle
		PUBLIC	F_HATTR			; 55 Get/set file handle attributes
		PUBLIC	F_HFTIME		; 56 Get/set file handle date and time
		PUBLIC	F_GETDTA		; 57 Get disk transfer address
		PUBLIC	F_GETVFY		; 58 Get verify flag settings
		PUBLIC	F_GETCD			; 59 Get current directory
		PUBLIC	F_CHDIR			; 5A Change current directory
		PUBLIC	F_PARSE			; 5B Parse pathname
		PUBLIC	F_PFILE			; 5C Parse filename
		PUBLIC	F_CHKCHR		; 5D Check character
		PUBLIC	F_WPATH			; 5E Get whole path string
		PUBLIC	F_FORK			; 60 For a child process
		PUBLIC	F_JOIN			; 61 Rejoin parent process
		PUBLIC	F_ASSIGN		; 6A Logical drive assignment
		PUBLIC	F_DSKCHK		; 6E Get/set disk check status
		PUBLIC	fhsPtrFIB		; con

		EXTERN	sysChainRmv
		EXTERN	sysCheckChar
		EXTERN	ramVarAlloc
		EXTERN	ramVarFree
		EXTERN	ramFreeUserSeg
		EXTERN	K_CON_CLEAR
		EXTERN	conClearBuf

		; dsk
		EXTERN	dskFlushPhys
		EXTERN	dskParsePath
		EXTERN	dskParFileName
		EXTERN	dskParFileN
		EXTERN	dskCheckDevice
		EXTERN	dskMergeName
		EXTERN	dskUpdateFIB
		EXTERN	dskUseFIB
		EXTERN	dskFindFirst
		EXTERN	dskFindNext
		EXTERN	dskFindFIB
		EXTERN	dskSavEntry
		EXTERN	dskGetEntry
		EXTERN	dskSelSubdir
		EXTERN	dskDirFirst
		EXTERN	dskDirEntry
		EXTERN	dskEnsureFIB
		EXTERN	dskEnsure2FIB
		EXTERN	dskUpdFIB
		EXTERN	dskSetupFIB
		EXTERN	dskCheckOpen
		EXTERN	dskCompareFIB
		EXTERN	dskDeleteFIB
		EXTERN	dskRenameFIB
		EXTERN	dskTimeFIB
		EXTERN	dskChangedFIB
		EXTERN	dskCheckDot
		EXTERN	dskDot1
		EXTERN	dskWriteFIB
		EXTERN	dskReadFIB
		EXTERN	dskRwFIB
		EXTERN	dskBufDirty
		EXTERN	dskFlushLog
		EXTERN	dskValFIB
		EXTERN	dskZapDir
		EXTERN	STOR_7
	
; ---------------------------------------------------------
; Function $31 DPARM
; Input:  de = pointer to buffer
;         l  = drive number (0=default, 1=A: ec.)
; Output: de = pointer to filled buffer
;         a  = error
; Filled buffer:
;      +0,1  = physical drive number (1=A: etc)
;      +1,2  = sector size (always 512)
;      +3,1  = sectors per cluster (non-zero power of 2)
;      +4,2  = number of reserved sectors (usually 1)
;      +6,1  = number of copies of the FAT (usually 2)
;      +7,2  = number of root directory entries
;      +9,2  = total number of logical sectors (if FAT16 then 0)
;      +11,1 = media descriptor byte
;      +12,1 = number of sectors per FAT
;      +13,2 = first root directory sector number
;      +15,2 = first data sector number
;      +17,2 = maximum cluster number
;      +19,1 = dirty disk flag
;      +20,4 = volume id (-1 => no volume id)
;      +24,3 = if FAT12 then reserved (always 0)
;              if FAT16 then total number of sectors (24 bits)
;      +27,5 = reserved (always 0)
; ---------------------------------------------------------
F_DPARM:	ld	ix,varB9DA		; buffer
		ld	b,$00
		ld	c,l
		push	de
		call	dskValFIB		; validate drive
		pop	de
		or	a			; disk error?
		ret	nz			; nz=yes
		push	hl
		pop	ix
		push	de

		; clear buffer
		ld	b,32
		xor	a
_dparm1:	ld	(de),a
		inc	de
		djnz	_dparm1
		pop	de			; restore buffer pointer
		push	de

		; physical drive number
		ld	bc,8
		add	hl,bc
		ldi

		; sector size 512 (2*256 + 0)
		inc	de
		ld	a,2
		ld	(de),a
		inc	de

		;sectors per cluser
		inc	hl
		ld	a,(hl)
		inc	hl
		inc	a
		ld	(de),a
		inc	de
		inc	hl

		; reserved sectors
		ldi
		ldi

		; FAT copies
		ldi

		; root directory entries
		ld	a,(hl)
		inc	hl
		push	hl
		ld	l,(hl)
		ld	h,$00
		add	hl,hl
		add	hl,hl
		add	hl,hl
		add	hl,hl
		add	a,l
		ld	(de),a
		inc	de
		ld	a,h
		ld	(de),a
		inc	de
		pop	hl
		inc	hl

		inc	de
		inc	de
		push	de			; pointer to media descriptor

		; sectors per fat	1
		; root directory sector	2
		; first data sector	2
		; maximum cluster	2
		; dirty flag		1
		; volume id		4
		inc	de
		ld	bc,12			; 1+2+2+2+1+4
		ldir

		; adjust max clusters
		ex	de,hl
		ld	bc,-7
		add	hl,bc
		inc	(hl)
		jr	nz,_dparm2
		inc	hl
		inc	(hl)
_dparm2:	ex	de,hl

		; media descriptor
		pop	de
		ldi

		; number of logical sectors
		ld	l,(ix+22)
		ld	h,(ix+23)
		dec	hl
		ld	b,(ix+11)

		; FAT16 is 24-bit and FAT12 is 16-bit sector count
		xor	a
_dparm3:	ld	c,$29
		adc	a,a
		djnz	_dparm3+1		; jump in middle of instruction which makes it 'add hl,hl'
		ld	c,(ix+20)
		ld	b,(ix+21)
		add	hl,bc
		ex	de,hl
		dec	hl
		dec	hl
		or	a			; sector count fits in 16 bits?
		jr	z,_dparm4		; z=yes
		ld	bc,$0010		; offset +26 (+10+16)
		add	hl,bc
		ld	(hl),a			; store bits 16..23 of sector count
		dec	hl
_dparm4:	ld	(hl),d			; store bits 8..15 sector count (FAT12 offset +10 / FAT16 offset +25)
		dec	hl
		ld	(hl),e			; store bits 0..7 sector count (FAT12 offset +9 / FAT16 offset +24)

		; restore buffer pointer / no errors
		pop	de
		xor	a
		ret

; ---------------------------------------------------------
; Function $40 FFIRST
; Input:  de = drive/path/filename or FIB pointer
;         hl = filename pointer (if de is FIB pointer)
;         b  = search attributes
;         ix = new FIB pointer
; Output: a  = error
;         ix = filled in FIB pointer
; ---------------------------------------------------------
F_FFIRST:	ld	a,4
		jr	FindFN

; ---------------------------------------------------------
; Function $42 FNEW
; Input:  de = drive/path/filename or FIB pointer
;         hl = filename pointer (if de is FIB pointer)
;         b  = search attributes (bit 7 is create new flag)
;         ix = new FIB pointer
; Output: a  = error
;         ix = filled in FIB pointer
; ---------------------------------------------------------
F_FNEW:		xor	a
FindFN:		ld	(varBBAF),a
		ld	a,(de)
		inc	a
		jr	z,_find1		; z=find FIB
		xor	a
		ld	c,10
		call	dskParsePath
		ret	nz
		or	c
		ld	a,_IPATH		; invalid pathname
		ret	nz
		jr	_find2

		; Find FIB
_find1:		push	hl
		push	de
		ex	(sp),ix
		call	dskUseFIB		; validate provided FIB
		ld	c,(ix+25)
		pop	ix
		jp	nz,_find4
		ld	a,_IDEV			; invalid device operation
		jp	c,_find4
		push	hl
		ld	hl,11
		add	hl,de
		ld	a,(hl)			; file attributes
		pop	hl
		bit	4,a			; is it a subdirectory?
		ld	a,_IATTR		; invalid attributes
		jr	z,_find4		; z=no
		bit	3,b			; is it a volume name?
		jr	nz,_find4		; nz=no
		call	dskSelSubdir

		; copy volume id
		push	bc
		push	hl
		ld	bc,25
		add	hl,bc
		push	ix
		pop	de
		ex	de,hl
		ld	bc,26
		add	hl,bc
		ex	de,hl
		ld	bc,4
		ldir
		pop	hl
		pop	bc

		pop	de			; filename
		ld	a,c			; drive number
		ld	c,$4e			; parsing flags
		call	dskParFileName		; parse filename
		ret	nz
		or	c
		ld	a,_IPATH		; invalid pathname
		ret	nz

_find2:		bit	2,(iy+47)
		jr	nz,_find3
		bit	5,b
		jr	z,_find3
		push	ix
		ex	(sp),hl
		ld	bc,1
		add	hl,bc
		ld	(varBB9E),hl
		pop	hl
		ld	de,varB91B
		call	dskParFileN		; parse filename item
		or	a
		ld	a,_IFNM			; invalid filename
		ret	nz
		push	hl
		ld	hl,varB926
		ld	de,varB91B
		ld	bc,varB926
		call	dskMergeName		; merge name into unique filename item
		pop	hl
		ld	de,varB926
		call	dskCheckDevice		; check if filename is device
		ld	(ix+30),a		; set FIB type file/device

		; copy search filename into FIB pattern field, used by find next
_find3:		ld	de,varB926
		push	ix
		ex	(sp),hl
		ld	bc,32
		add	hl,bc
		ex	de,hl
		push	de
		ld	bc,11
		ldir
		pop	de
		pop	hl
		call	dskFindFirst
		jr	_fnext1
_find4:		pop	de
		ret

; ---------------------------------------------------------
; Function $41 FNEXT
; Input:  ix = previous find first FIB pointer
; Output: a  = error
;         ix = filled in with next matching entry
; ---------------------------------------------------------
F_FNEXT:	ld	(iy+47),$04
		call	dskUseFIB
		ret	nz
		push	ix
		ex	(sp),hl
		ld	de,32
		add	hl,de
		ex	de,hl
		pop	hl
		call	dskFindNext
_fnext1:	push	af
		call	z,dskUpdateFIB
		pop	af
		jp	STOR_7

; ---------------------------------------------------------
; Function $44 CREATE
; ---------------------------------------------------------
F_CREATE:	ex	af,af'
		bit	3,b			; volume attribute?
		ld	a,_IATTR		; invalid attributes
		ret	nz			; nz=yes, return error
		call	CreateFile
		ret	nz
		ld	b,$ff
		bit	4,a			; is it a directory?
		jr	nz,CreateOpenEnd	; nz=yes
		jr	CreateOpen

; ---------------------------------------------------------
; Function $43 OPEN
; ---------------------------------------------------------
F_OPEN:		ex	af,af'
		call	OpenFile
		ret	nz
		bit	4,a			; is it a directory?
		ld	a,_DIRX			; directory exists
		ret	nz
CreateOpen:	push	hl
		call	FindFreeHandle
		jr	nz,CreateOpenRet2
		push	hl
		call	CreateFIB
		jr	nz,CreateOpenRet1	; nz=out of memory

		; setup FIB
		ex	de,hl
		ex	(sp),hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		push	bc
		push	de
		ex	(sp),ix
		pop	hl
		ld	bc,32
		ldir
		pop	bc
		pop	de
		ld	(ix+31),$06		; search attributes
		xor	a
		ld	(ix+45),a		; file pointer
		ld	(ix+46),a
		ld	(ix+47),a
		ld	(ix+48),a
		pop	hl
		ex	af,af'
		call	dskSetupFIB
		call	UpdRedirect
CreateOpenEnd:	xor	a
		ret

CreateOpenRet1:	pop	hl
CreateOpenRet2:	pop	hl
		ret

; ---------------------------------------------------------
; Function $45 CLOSE
; ---------------------------------------------------------
F_CLOSE:	call	fhsPtrFIB
		ret	nc			; nc=invalid handle
		ret	z			; z=handle not open
		call	CloseHandle
		call	UpdRedirect
		jr	_ensure1

; ---------------------------------------------------------
; Function $$46 ENSURE
; ---------------------------------------------------------
F_ENSURE:	call	fhsPtrFIB
		ret	nc			; nc=invalid handle
		ret	z			; z=handle not open
_ensure1:	call	dskEnsureFIB
		ld	a,(ix+25)		; drive number
		call	dskFlushLog
		xor	a
		ret

; ---------------------------------------------------------
; Function $47 DUP
; ---------------------------------------------------------
F_DUP:		call	fhsPtrFIB
		ret	nc			; nc=invalid handle
		ret	z			; z=handle not open
		call	FindFreeHandle
		ret	nz			; nz=no free handles
		call	IncRefHandle
		ret	nz			; nz=max references reached
		ld	(hl),e
		inc	hl
		ld	(hl),d
		call	UpdRedirect
		xor	a
		ret

; ---------------------------------------------------------
; Function $48 READ
; ---------------------------------------------------------
F_READ:		ld	a,$01		; 1: read
		jr	ReadWrite

; ---------------------------------------------------------
; Function $49 WRITE
; ---------------------------------------------------------
F_WRITE:	xor	a		; 0: write
ReadWrite:	ex	af,af'
		push	de
		push	hl
		call	fhsPtrFIB
		pop	bc
		pop	de
		ret	nc
		ret	z
		ex	af,af'
		call	dskRwFIB
		ld	h,b
		ld	l,c
		ret

; ---------------------------------------------------------
; Function $4A SEEK
; ---------------------------------------------------------
F_SEEK:		ex	af,af'
		push	de
		push	hl
		call	fhsPtrFIB
		pop	de
		pop	hl
		ret	nc
		ret	z
		ex	af,af'
		push	hl
		ld	hl,$0000
		ld	bc,$0000
		or	a
		jr	z,_seek1
		ld	l,(ix+45)
		ld	h,(ix+46)
		ld	c,(ix+47)
		ld	b,(ix+48)
		dec	a
		jr	z,_seek1
		ld	l,(ix+21)
		ld	h,(ix+22)
		ld	c,(ix+23)
		ld	b,(ix+24)
_seek1:		add	hl,de
		ex	(sp),hl
		pop	de
		adc	hl,bc
		ex	de,hl
		ld	(ix+45),l
		ld	(ix+46),h
		ld	(ix+47),e
		ld	(ix+48),d
		xor	a
		ret

; ---------------------------------------------------------
; Function $4B IOCTL
; ---------------------------------------------------------
F_IOCTL:	ex	af,af'
		push	de
		call	fhsPtrFIB
		pop	de
		ret	nc
		ret	z
		ex	af,af'
		ld	l,(ix+28)
		ld	h,(ix+29)
		or	a
		jr	z,_ioctl3		; 0: get file handle info
		dec	a
		jr	z,_ioctl2		; 1: set ascii/binary mode
		dec	a
		jr	z,_ioctl8		; 2: check input status
		dec	a
		jr	z,_ioctl9		; 3: check output status
		dec	a
		jr	z,_ioctl12		; 4: geen screen size
_ioctl1:	ld	a,_ISBFN		; invalid sub-function number
		ret

_ioctl2:	bit	7,(ix+30)
		jr	z,_ioctl1
		ld	a,(ix+30)
		xor	e
		and	$df
		xor	e
		res	6,a
		ld	(ix+30),a
_ioctl3:	ld	e,(ix+30)
		xor	a
		ld	d,a
		bit	7,e
		ret	nz

; Subroutine get mode of handle
_ioctl4:	ld	e,(ix+25)
		dec	e
		ld	b,4
_ioctl5:	ld	a,(ix+48)
		cp	(ix+24)
		jr	c,_ioctl7
		jr	nz,_ioctl6
		dec	ix
		djnz	_ioctl5
_ioctl6:	set	6,e
_ioctl7:	xor	a
		ld	d,a
		ret

_ioctl8:	bit	1,(ix+49)
		jr	nz,_ioctl10
		bit	7,(ix+30)
		jr	nz,_ioctl11
		call	_ioctl4
		bit	6,e
		jr	nz,_ioctl10
		ld	e,$00
		ret

_ioctl9:	bit	0,(ix+49)
		jr	nz,_ioctl10
		inc	hl
		inc	hl
		inc	hl
		bit	7,(ix+30)
		jr	nz,_ioctl11
_ioctl10:	ld	e,0ffh
		ret

_ioctl11:	ld	bc,$0006
		add	hl,bc
		ld	c,(ix+30)
		jp	(hl)
_ioctl12:	bit	7,(ix+30)
		jr	nz,_ioctl13
		xor	a
		ld	e,a
		ld	d,a
		ret

_ioctl13:	ld	bc,$000C
		add	hl,bc
		jp	(hl)

; ---------------------------------------------------------
; Function $4C HTEST
; ---------------------------------------------------------
F_HTEST:	push	bc
		call	OpenFile
		pop	bc
		ret	nz
		bit	7,(ix+30)		; device FIB or asciiz string?
		jr	nz,_htest1		; nz=yes,exit
		push	hl
		call	fhsPtrFIB
		pop	hl
		ret	nc
		ret	z
		ld	b,$ff
		call	dskCompareFIB
		ret	z
_htest1:	xor	a
		ld	b,a
		ret

; ---------------------------------------------------------
; Function $4D DELETE
; ---------------------------------------------------------
F_DELETE:	call	OpenFile
		ret	nz			; nz=not found
		ld	a,$ff			; dirty disk flag (allow undelete)
		call	dskDeleteFIB
		ret

; ---------------------------------------------------------
; Function $4E RENAME
; ---------------------------------------------------------
F_RENAME:	push	hl
		call	OpenFile
		pop	bc
		ret	nz
		call	dskRenameFIB
		ret

; ---------------------------------------------------------
; Function $4F MOVE
; ---------------------------------------------------------
F_MOVE:		push	hl
		call	OpenFile
		pop	bc
		ret	nz
		call	MoveFIB
		ret

; ---------------------------------------------------------
; Function $50 ATTR
; ---------------------------------------------------------
F_ATTR:		ex	af,af'
		push	hl
		call	OpenFile
		pop	bc
		ret	nz
		ex	af,af'
		or	a
		call	nz,UpdAttribFIB
		or	a
		ret	nz
		call	dskDirEntry
		ld	hl,$000B
		add	hl,de
		ld	l,(hl)
		xor	a
		ret

; ---------------------------------------------------------
; Function $51 FTIME
; ---------------------------------------------------------
F_FTIME:	ex	af,af'
		push	ix
		push	hl
		call	OpenFile
		pop	bc
		pop	de
		ret	nz
		ex	af,af'
		or	a
		call	nz,dskTimeFIB
		or	a
		ret	nz
		call	dskDirEntry
		ld	hl,$0016
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		xor	a
		ret

; ---------------------------------------------------------
; Function $52 HDELETE
; ---------------------------------------------------------
F_HDELETE:	call	fhsPtrFIB
		ret	nc
		ret	z
		call	CloseHandle
		call	UpdRedirect
		call	dskEnsure2FIB
		set	3,(ix+49)
		or	a
		ret	nz
		call	dskDirEntry
		ld	a,$ff
		call	dskDeleteFIB
		ret

; ---------------------------------------------------------
; Function $53 HRENAME
; ---------------------------------------------------------
F_HRENAME:	push	hl
		call	fhsPtrFIB
		pop	bc
		ret	nc
		ret	z
		push	bc
		call	dskEnsure2FIB
		pop	bc
		or	a
		ret	nz
		call	dskDirEntry
		call	dskRenameFIB
		or	a
		ret	nz
		call	dskUpdFIB
		xor	a
		ret

; ---------------------------------------------------------
; Function $54 HMOVE
; ---------------------------------------------------------
F_HMOVE:	push	hl
		call	fhsPtrFIB
		pop	bc
		ret	nc
		ret	z
		push	bc
		call	dskEnsure2FIB
		pop	bc
		or	a
		ret	nz
		call	dskDirEntry
		call	MoveFIB
		or	a
		ret	nz
		call	dskUpdFIB
		xor	a
		ret

; ---------------------------------------------------------
; Function $55 HATTR
; ---------------------------------------------------------
F_HATTR:	ex	af,af'
		ld	c,l
		call	fhsPtrFIB
		ret	nc
		ret	z
		ex	af,af'
		or	a
		jr	z,_hattr1
		push	bc
		call	dskEnsure2FIB
		pop	bc
		or	a
		ret	nz
		call	dskDirEntry
		call	UpdAttribFIB
		ret	nz
		call	dskUpdFIB
_hattr1:	ld	L,(ix+14)
		xor	a
		ret

; ---------------------------------------------------------
; Function $56 HFTIME
; ---------------------------------------------------------
F_HFTIME:	ex	af,af'
		push	ix
		push	hl
		call	fhsPtrFIB
		pop	bc
		pop	de
		ret	nc
		ret	z
		ex	af,af'
		or	a
		jr	z,_hftime1
		push	bc
		push	de
		call	dskEnsure2FIB
		pop	de
		pop	bc
		or	a
		ret	nz
		call	dskTimeFIB
		ret	nz
		call	dskUpdFIB
_hftime1:	ld	e,(ix+15)
		ld	d,(ix+16)
		ld	l,(ix+17)
		ld	h,(ix+18)
		xor	a
		ret


; ---------------------------------------------------------
; Function $57 GETDTA
; Output: DE = disk transfer address
; ---------------------------------------------------------
F_GETDTA:	ld	de,(DTA_AD)
		xor	a
		ret

; ---------------------------------------------------------
; Function $58 GETVFY
; ---------------------------------------------------------
F_GETVFY:	ld	a,(RAWFLG)
		or	a
		jr	z,_getvfy1
		ld	a,$ff
_getvfy1:	ld	b,a
		xor	a
		ret

; ---------------------------------------------------------
; Function $59 GETCD
; ---------------------------------------------------------
F_GETCD:	ld	a,b
		ld	b,$00
		ld	ix,varB9DA
		ld	c,9
		push	de
		ld	de,_getcd1
		call	dskParsePath
		pop	de
		ret	nz
		jp	F_WPATH

_getcd1:	db	0

; ---------------------------------------------------------
; Function $5A CHDIR
; ---------------------------------------------------------
F_CHDIR:	xor	a
		ld	b,a
		ld	ix,varB9DA
		ld	c,9
		call	dskParsePath
		ret	nz
		or	c
		ld	a,_IPATH		; invalid path
		ret	nz
		ld	bc,30
		add	hl,bc
		ld	de,(varBBE8)
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ex	de,hl
		jp	F_WPATH

; ---------------------------------------------------------
; Function $5B PARSE
; ---------------------------------------------------------
F_PARSE:	ld	c,4
		ld	ix,varB9DA
		xor	a
		call	dskParsePath
		ld	c,(ix+25)
		ld	de,(varBB9E)
		ld	hl,(varBB9C)
		ret

; ---------------------------------------------------------
; Function $5C PFILE
; ---------------------------------------------------------
F_PFILE:	push	hl
		ld	(varBB9E),de
		ex	de,hl
		ld	b,$00
		call	dskParFileN
		ld	de,(varBB9E)
		pop	hl
		xor	a
		ret

; ---------------------------------------------------------
; Function $5D CHKCHR
; ---------------------------------------------------------
F_CHKCHR:	ld	a,e
		ld	c,d
		call	sysCheckChar
		ld	d,c
		ld	e,a
		xor	a
		ret

; ---------------------------------------------------------
; Function $5E WPATH
; ---------------------------------------------------------
F_WPATH:	push	de
		ld	hl,varB931+2
		ld	c,$00
		push	de
		ld	a,(varBB99)
		or	a
		jr	nz,_wpath3
_wpath1:	ld	a,(hl)
		inc	hl
		cp	$02
		jr	z,_wpath3
		cp	$01
		jr	nz,_wpath2
		pop	af
		ld	a,$5c
		inc	de
		push	de
		dec	de
_wpath2:	ld	(de),a
		inc	de
		or	a
		jr	nz,_wpath1
		jr	_wpath4

_wpath3:	ld	a,_PLONG		; pathname too long
_wpath4:	pop	hl
		pop	de
		ret

; ---------------------------------------------------------
; Function $60 FORK
; ---------------------------------------------------------
F_FORK:		ld	hl,64*2
		call	ramVarAlloc
		ret	nz
		ld	de,(varBBF0)
		ld	(varBBF0),hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		ld	a,d
		or	e
		jr	z,r023
		inc	de
		ld	b,$3f
r021:		push	bc
		inc	hl
		inc	de
		ld	a,(de)
		ld	c,a
		inc	de
		ld	a,(de)
		ld	b,a
		or	c
		jr	z,r022
		push	bc
		pop	ix
		bit	2,(ix+49)
		jr	z,r022
		call	IncRefHandle
		jr	nz,r022
		ld	(hl),c
		inc	hl
		ld	(hl),b
		dec	hl
r022:		inc	hl
		pop	bc
		djnz	r021
r023:		ld	a,(varBBFE)
		ld	B,A
		inc	a
		ld	(varBBFE),a
		call	UpdRedirect
		xor	a
		ret

; ---------------------------------------------------------
; Function $61 JOIN
; ---------------------------------------------------------
F_JOIN:		ld	a,b
		or	a
		jr	z,r031
		ld	HL,varBBFE
		cp	(hl)
		ld	a,_IPROC		; invalid process id
		ret	nc
r031:		call	ramFreeUserSeg
		ld	hl,(varBBF0)
		push	hl
r032:		ld	a,h
		or	l
		jr	z,r035
		push	bc
		push	hl
		call	ramVarFree
		ld	b,$ff
r033:		inc	b
		call	fhsPtrFIB
		jr	nc,r034
		call	nz,DecRefHandle
		jr	r033

r034:		pop	hl
		pop	bc
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		ld	(varBBF0),hl
		dec	(iy+$7e)		; varBBFE
		ld	a,(varBBFE)
		inc	b
		dec	b
		jr	z,r032
		cp	b
		jr	nz,r032
		xor	a
		ld	(de),a
		dec	de
		ld	(de),a
r035:		ld	(iy+$7e),b		; varBBFE
r036:		pop	hl
		ld	a,h
		or	l
		jr	z,r038
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		ld	b,$3f			; $3f=max file handles
r037:		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		pop	ix
		ld	a,d
		or	e
		push	hl
		push	bc
		call	nz,dskEnsureFIB
		pop	bc
		pop	hl
		djnz	r037
		jr	r036

r038:		ld	a,$ff
		call	dskFlushPhys
		ld	a,(varBBFE)
		or	a
		jr	nz,r039
		call	F_FORK
		call	DefaultHandles
		call	K_CON_CLEAR
r039:		call	UpdRedirect		; check redir
		call	conClearBuf		; clear console line input buffer
		xor	a
		ret

; ---------------------------------------------------------
; Function $6A ASSIGN
; ---------------------------------------------------------
F_ASSIGN:	ld	HL,varBA1A
		ld	a,b
		or	a
		jr	z,_assign3
		cp	$09
		ld	a,_IDRV			; invalid drive
		ret	nc
		ld	c,b
		ld	b,0
		add	hl,bc
		ld	a,d
		inc	a
		jr	z,_assign2
		dec	a
		jr	z,_assign1
		cp	$09
		ld	a,_IDRV			; invalid drive
		ret	nc
		ld	c,d
_assign1:	ld	(hl),c
_assign2:	ld	d,(hl)
		xor	a
		ret

_assign3:	ld	(hl),a
		inc	hl
		inc	a
		cp	$09
		jr	nz,_assign3
		xor	a
		ld	d,a
		ret

; ---------------------------------------------------------
; Function $6E DSKCHK
; ---------------------------------------------------------
F_DSKCHK:	or	a
		jr	z,_dskchk2
		ld	a,b
		or	a
		jr	z,_dskchk1
		ld	a,$ff
_dskchk1:	ld	(DSK_CHK),a
_dskchk2:	ld	a,(DSK_CHK)
		ld	b,a
		xor	a
		ret

; ---------------------------------------------------------
; *** Subroutines: file and directory ***
; ---------------------------------------------------------

; Subroutine open file and create or update FIB
OpenFile:	ld	(iy+47),04H
		ld	B,16H
		ld	A,(DE)
		INC	A
		JR	Z,_create2
		JR	_create1

; Subroutine create new file and FIB
CreateFile:	ld	(iy+47),00H
_create1:	xor	a
		ld	ix,varB9DA
		ld	c,8
		call	dskParsePath
		ret	nz
		or	c
		ld	a,_IPATH		; invalid pathname
		ret	nz
		bit	5,b
		ld	a,_IFNM			; invalid filename
		ret	nz
		ld	de,varB926
		call	dskFindFirst
		jr	_create3

_create2:	push	de
		pop	ix
		call	dskUseFIB
		ret	nz
		bit	3,(ix+31)
		ld	a,_IATTR		; invalid attributes
		ret	nz
		xor	a
_create3:	or	a
		ret	nz
		push	hl
		ld	hl,11
		add	hl,de
		ld	a,(hl)
		pop	hl
		cp	a
		ret

; ---------------------------------------------------------
; *** Subroutines: file and directory ***
; ---------------------------------------------------------

; Subroutine update redirect status
UpdRedirect:	push	bc
		push	de
		push	hl
		push	ix
		ld	c,$00
		ld	b,$00
		call	fhsPtrFIB
		jr	nc,_updredirect1
		jr	z,_updredirect1
		ld	A,(ix+30)
		AND	$81
		CP	$81
		jr	z,_updredirect1
		set	0,c
_updredirect1:	ld	b,1
		call	fhsPtrFIB
		jr	nc,_updredirect2
		jr	z,_updredirect2
		ld	a,(ix+30)
		and	$82
		cp	$82
		jr	z,_updredirect2
		set	1,c
_updredirect2:	ld	a,c
		ld	(varBB89),a
		pop	ix
		pop	hl
		pop	de
		pop	bc
		ret

; Subroutine: find free file handle
FindFreeHandle:	push	de
		push	ix
		ld	b,$ff
_findfree1:	inc	b
		call	fhsPtrFIB
		ld	a,_NHAND		; no spare file handles
		jr	nc,_findfree2
		jr	nz,_findfree1
		xor	a
_findfree2:	pop	ix
		pop	de
		or	a
		ret

; Subroutine get pointer to FIB of file handle
; Input:  B  = file handle
; Output: Cx = reset if invalid file handle, set if valid
;	  Zx = reset if fib found
fhsPtrFIB:	ld	A,B
		CP	3FH
		JR	NC,J2158
		ld	HL,(varBBF0)
		ld	A,H
		OR	L
		JR	Z,J2158
		PUSH	BC
		INC	HL
		INC	HL
		ld	C,B
		ld	B,0
		ADD	HL,BC
		ADD	HL,BC
		ld	E,(HL)
		INC	HL
		ld	D,(HL)
		DEC	HL
		POP	BC
		PUSH	DE
		POP	ix
		ld	A,D
		OR	E
		SCF
		ld	A,0C2H
		RET

J2158:		ld	A,0C3H
		OR	A
		RET

; Subroutine increase file handle count of FIB
IncRefHandle:	ld	a,(ix-1)
		inc	a
		jr	z,_increfh1
		ld	(ix-1),a
		xor	a
		ret

_increfh1:	ld	a,_NHAND		; no spare file handles
		or	a
		ret

; Subroutine decrease file handle count of FIB and remove FIB if zero count
DecRefHandle:	ld	a,(ix-1)
		dec	a
		ld	(ix-1),a
		ret	nz
		push	de
		push	bc
		push	ix
		ex	(sp),hl
		ld	bc,-3
		add	hl,bc
		ex	de,hl
		ld	hl,varBBF2
		call	sysChainRmv		; remove element from chain
		pop	hl
		pop	bc
		pop	de
		xor	a
		ret

; Subroutine create FIB
CreateFIB:  	ld	hl,56			; FAT16
		call	ramVarAlloc
		ret	nz
		push	de
		ld	de,(varBBF2)
		ld	(varBBF2),hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ld	(hl),$01
		inc	hl
		pop	de
		ret

; Subroutine open default file handles
DefaultHandles:	ld	b,5
		ld	hl,DeviceHandles
next_handle:	push	bc
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl
		push	hl
		push	bc
		ld	a,b
		call	F_OPEN
		pop	bc
		or	a
		ld	de,DevNul
		ld	a,b
		call	nz,F_OPEN
		pop	hl
		pop	bc
		djnz	next_handle
		ret

DeviceHandles:	dw	DevCon
		db	%101		; CON read only
		dw	DevCon
		db	%110		; CON write only
		dw	DevCon
		db	%100		; CON read & write
		dw	DevAux
		db	%100		; AUX read & write
		dw	DevPrn
		db	%110		; PRN write only

DevCon:		db	"CON",0
DevAux:		db	"AUX",0
DevPrn:		db	"PRN",0
DevNul:		db	"NUL",0

; ---------------------------------------------------------

; Subroutine free (close) file handle
CloseHandle:	call	DecRefHandle
		xor	a
		ld	(hl),a
		inc	hl
		ld	(hl),a
		ret

; ---------------------------------------------------------

; Subroutine move current directory entry
; Todo: optimize, use generic buffer (called from F_HMOVE and F_MOVE)
; Input:  ix = pointer to FIB
;         hl =
;         de = pointer to directory entry
;         bc = pointer to new path name
; Output: a = error code
MoveFIB:	xor	a
		bit	7,(ix+30)		; is it a device?
		ret	nz			; nz=yes,exit
		call	dskCheckOpen		; is the file open?
		ret	nz			; nz=yes,exit
		call	dskCheckDot		; is it "." or ".." ?
		ret	c			; c=yes,exit
		push	hl
		ld	hl,26			; offset for start cluster of directory entry
		add	hl,de
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ld	(varBBAB),hl		; save start cluster
		ld	hl,11			; offset for attributes of directory entry
		add	hl,de
		ld	a,(hl)
		pop	hl
		bit	4,a			; is it a directory?
		call	nz,dskZapDir		; nz=yes, zap current directory cluster
		push	bc
		push	hl

		; copy current filename to buffer
		ld	hl,varB91B
		ld	b,11
		ld	a,(de)
		cp	$05
		jr	nz,_movefib1
		ld	a,$e5
_movefib1:	ld	(hl),a
		inc	hl
		inc	de
		ld	a,(de)
		djnz	_movefib1

		; FAT16 STOR_5
		push	af
		ld	a,(varSdir1)
		ld	(varSdir3),a
		pop	af

		ld	hl,varBBDE
		ld	de,varBBC6
		ld	bc,12
		ldir
		pop	hl
		pop	de			; pointer to new path name
		ld	c,$89
		ld	b,(ix+31)
		ld	a,(ix+25)
		ld	(iy+47),$00
		call	dskParsePath
		ret	nz
		or	c
		ld	a,_IPATH		; invalid pathname
		ret	nz
		bit	1,(iy+47)		; move to subdir of current dir?
		ld	a,_DIRE			; invalid directory move
		ret	nz			; nz=yes
		call	dskDirFirst
		ld	bc,varB91B
		ld	(varBBAD),bc
		call	dskFindFIB
		ld	a,_DUPF			; duplicate filename
		ret	c
		call	dskGetEntry
		ret	nz
		call	dskSavEntry
		push	hl

		; FAT16 STOR_6
		push	af
		ld	a,(varSdir3)
		ld	(varSdir1),a
		pop	af

		; move entry from current to new
		ld	hl,varBBC6
		ld	de,varBBDE
		ld	bc,12
		ldir
		pop	hl
		call	dskDirEntry
		push	hl
		ld	hl,varB8D4		; optimize: B8D4 is only used in this routine (32 bytes), replacable with other buffer?
		push	de
		ex	de,hl
		ld	bc,32
		ldir				; copy directory entry to B8D4
		pop	hl
		ld	(hl),$e5		; mark old entry as deleted
		call	dskBufDirty
		pop	hl
		call	dskGetEntry
		call	dskDirEntry
		push	hl
		ld	hl,varB8D4		; optimize: B8D4 is only used here
		ld	bc,32
		push	de
		ldir				; paste directory entry to B8D4
		pop	de
		pop	hl
		call	dskChangedFIB
		ret	z
		call	dskZapDir
		ld	bc,(varBBE8)
		push	bc
		call	dskSelSubdir
		call	dskDirFirst
		ld	bc,dskDot1
		ld	(varBBAD),bc
		call	dskFindFIB
		pop	bc
		ret	nc
		and	$10
		ret	z
		bit	7,b
		jr	z,_movefib2
		ld	bc,0
_movefib2:	push	hl
		ld	hl,26
		add	hl,de
		ld	(hl),c
		inc	hl
		ld	(hl),b
		call	dskBufDirty
		xor	a
		pop	hl
		ret

; Subroutine update attribute directory entry
UpdAttribFIB:	xor	a
		bit	7,(ix+30)
		ret	nz
		call	dskCheckOpen
		ret	nz
		push	hl
		ld	hl,11
		add	hl,de
		ld	a,(hl)
		ld	b,$dd
		bit	4,a
		jr	nz,_updattrib1
		ld	b,$d8
_updattrib1:	xor	c
		and	b
		ld	a,_IATTR		; invalid attributes
		jr	nz,_updattrib2
		ld	(hl),c
		call	dskBufDirty
		xor	a
_updattrib2:	pop	hl
		ret

