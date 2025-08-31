; ------------------------------------------------------------------------------
; cs0_dsk.asm
; Common disk subroutines
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS0_DSK

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_FLUSH			; 5F Flush disk buffers
		
		PUBLIC	dskValFIB		; fcb,fhs
		PUBLIC	dskWriteFIB		; fcb,fhs,con
		PUBLIC	dskReadFIB		; fcb,fhs,con
		PUBLIC	dskParsePath		; fcb,fhs
		PUBLIC	dskFindFirst		; fcb,fhs
		PUBLIC	dskFindNext		; fcb,fhs
		PUBLIC	dskEnsureFIB		; fcb,fhs
		PUBLIC	dskUpdateFIB		; fcb,fhs
		PUBLIC	dskUseFIB		; fcb,fhs
		PUBLIC	dskSetupFIB		; fcb,fhs
		PUBLIC	dskDeleteFIB		; fcb,fhs
		PUBLIC	dskRenameFIB		; fcb,fhs
		PUBLIC	dskRwFIB		; fcb,fhs
		PUBLIC	STOR_7			; fcb,fhs
		PUBLIC	dskFlushLog		; fcb,fhs
		PUBLIC	dskFlushDrives		; fcb
		PUBLIC	dskPhysDrive		; fcb
		PUBLIC	dskUnflag		; fcb
		PUBLIC	CHKDRV			; fcb
		PUBLIC	dskRwSectors		; fcb
		PUBLIC	dskFatSector		; fcb
		PUBLIC	dskNameZ		; fcb
		PUBLIC	dskFat12Entry		; fcb
		PUBLIC	dskParFileName		; fhs
		PUBLIC	dskParFileN		; fhs
		PUBLIC	dskMergeName		; fhs
		PUBLIC	dskSavEntry		; fhs
		PUBLIC	dskGetEntry		; fhs
		PUBLIC	dskSelSubdir		; fhs
		PUBLIC	dskDirFirst		; fhs
		PUBLIC	dskDirEntry		; fhs
		PUBLIC	dskFindFIB		; fhs
		PUBLIC	dskEnsure2FIB		; fhs
		PUBLIC	dskUpdFIB		; fhs
		PUBLIC	dskCompareFIB		; fhs
		PUBLIC	dskTimeFIB		; fhs
		PUBLIC	dskChangedFIB		; fhs
		PUBLIC	dskCheckDevice		; fhs
 		PUBLIC	dskCheckOpen		; fhs
		PUBLIC	dskCheckDot		; fhs
		PUBLIC	dskDot1			; fhs
		PUBLIC	dskBufDirty		; fhs
		PUBLIC	dskZapDir		; fhs
		PUBLIC	dskFlushPhys		; fhs
		PUBLIC	dskFlushBuf		; ram

		EXTERN	sysTpaAbort
		EXTERN	sysTpaCall
		EXTERN	sysCheckChar
		EXTERN	sysFarDos
		EXTERN	timReadRTC

; ---------------------------------------------------------
; Function $5F FLUSH
; ---------------------------------------------------------
F_FLUSH:	ld	a,b
		cp	$ff
		jr	z,r011
		call	dskPhysDrive
		ld	b,a
		ld	a,c
		ret	z
r011:		ld	a,b
		call	dskFlushPhys
		ld	a,d
		or	a
		ret	z
		ld	a,b
		call	UnflagPhys

		; set disk change status of all drives to flushed
		ld	b,$08			; max 8 drives
r012:		ld	a,b
		call	dskPhysDrive
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ld	a,d
		or	e
		jr	z,r013
		ld	hl,$0009
		add	hl,de
		ld	a,(hl)
		or	a
		jr	z,r013
		ld	(hl),$01
r013:		djnz	r012
		xor	a
		ret

; ---------------------------------------------------------
; *** Parser  ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Subroutine parse path
; Input:  a  = drive id
;         b  = search attributes
;         c  = parse flag
;         de = address of path string
;         hl = address of validated drive table (DUB)
;         ix = address of space for new FIB
; Output: a  = error code
; ---------------------------------------------------------
dskParsePath:	ld	(ix+fib_id),$ff
		ld	(ix+fib_search),b
		ld	(iy+32),c
		ld	(varBB9E),de

		; set drive
		or	a
		jr	nz,_parsepath1
		ld	a,(CUR_DRV)
_parsepath1:	ld	d,a
		call	ParDrive		; parse drive
		or	a
		jr	z,_parsepath2
		cp	d
		jr	z,_parsepath2
		ld	d,a
		bit	7,(iy+32)
		ld	a,_IDRV			; invalid drive
		ret	nz
_parsepath2:	ld	(ix+fib_drive),d

		; root directory
		bit	3,(ix+fib_search)	; volume?
		jr	nz,_parsepath3		; nz=yes
		call	ParGetChar
		jr	z,_parsepath4
		cp	$5c			; "\" ?
		jr	nz,_parsepath4
		set	0,b
		set	1,b
_parsepath3:	set	5,(iy+32)		; set root directory flag
		xor	a
_parsepath4:	call	nz,ParUndoChar		; if not "\" then undo get character

		; initialize loop
		ld	de,(varBB9E)
		ld	(varBB9C),de
		call	WpathInit		; initialize whole path
		bit	3,(ix+fib_search)	; volume?
		jr	z,_parsepath5		; z=no
		ld	de,varB926		; filename buffer
		call	ParVolume		; parse volume
		jr	_parfile3

		; loop name components
_parsepath5:	ld	de,varB926
		call	dskParFileN
		cp	$5c			; "\" ?
		jr	nz,_parfile1
		set	1,b
		call	ParGetChar		; get character
		ld	de,(varBB9E)
		LD	(varBB9C),de
		ld	de,varB926
		call	ParNext			; get next name component
		jr	nz,_parfile5
		ld	de,varB926
		call	WpathAdd		; add component to whole path
		jr	z,_parsepath5
		jr	_parfile5

; ---------------------------------------------------------
; Subroutine parse file name
; Input:  B = attributes
;	  C = parse flags
; ---------------------------------------------------------
dskParFileName:	ld	(ix+fib_id),$ff
		ld	(ix+fib_search),b
		ld	(ix+fib_drive),a
		ld	(iy+32),c
		ld	(varBB9E),de
		ld	(varBB9C),de
		ld	b,$00
		ld	de,varB926
		call	dskParFileN

		; wildcards "*.*" ?
_parfile1:	ld	a,b
		and	$18
		jr	nz,_parfile2
		bit	1,(iy+32)
		jr	z,_parfile2
		push	hl
		push	bc
		ld	hl,qname
		ld	de,varB926
		ld	bc,11
		ldir
		pop	af
		or	$39
		ld	b,a
		pop	hl

_parfile2:	xor	a
		bit	0,(iy+32)
		ld	de,varB926
		call	z,dskCheckDevice
		or	a
		jr	nz,_parfile4
_parfile3:	set	4,(iy+32)
		ld	de,varB926
		call	ParNext			; get next name component
		jr	nz,_parfile5
_parfile4:	ld	(ix+fib_mode),a
		ld	de,varB926
		call	WpathAdd			; add last name component to whole path
_parfile5:	push	af
		call	ParGetChar		; get last character
		call	nz,ParUndoChar		; undo get last character pointer
		ld	c,a			; save last character
		pop	af
		or	a
		ret

qname:		db	"????????","???"

; ---------------------------------------------------------
; Subroutine try to parse drive indicator
; ---------------------------------------------------------
ParDrive:	ld	(iy+33),$00
		call	ParGetChar
		jr	z,_pardrive2
		bit	1,(iy+33)
		jr	nz,_pardrive1

		; drive between 'A' and 'Z' ?
		sub	'A'
		jr	c,_pardrive1
		cp	$1a
		jr	nc,_pardrive1

		inc	a
		ld	b,a
		call	ParGetChar
		jr	z,_pardrive1
		cp	':'
		ld	a,b			; drive number
		ld	b,4			; parse flags
		ret	z
		call	ParUndoChar
_pardrive1:	call	ParUndoChar
_pardrive2:	xor	a
		ld	b,a
		ret

; ---------------------------------------------------------
; Subroutine parse volume name
; ---------------------------------------------------------
ParVolume:	push	hl
		ex	de,hl
		ld	a,b
		and	$07
		ld	b,a
		ld	(iy+33),$09
		ld	c,11
		call	ParName
		dec	d
		jr	nz,_parvolume1
		set	3,b
_parvolume1:	pop	hl
		ret

; ---------------------------------------------------------
; Subroutine parse file name
; ---------------------------------------------------------
dskParFileN:	push	hl
		ex	de,hl
		ld	a,b
		and	$07
		ld	b,a
		ld	(iy+33),$00
		ld	c,8
		call	ParGetChar
		jr	z,_parfilen6
		cp	'.'
		jr	nz,_parfilen5
		ld	d,1
		call	ParGetChar
		jr	z,_parfilen2
		bit	4,(iy+33)
		jr	z,_parfilen4
		cp	'.'
		jr	nz,_parfilen1
		set	7,b
		inc	d
		call	ParGetChar
		jr	z,_parfilen2
		bit	4,(iy+33)
		jr	z,_parfilen3
		cp	'.'
		jr	z,_parfilen3
_parfilen1:	call	ParUndoChar
_parfilen2:	ld	(hl),'.'
		inc	hl
		dec	c
		dec	d
		jr	nz,_parfilen2
		set	6,b
		set	3,b
		set	0,b
		jr	_parfilen6

_parfilen3:	res	7,b
		call	ParUndoChar
_parfilen4:	call	ParUndoChar
_parfilen5:	call	ParUndoChar
_parfilen6:	call	ParName
		dec	d
		jr	nz,_parfilen7
		set	3,b
_parfilen7:	cp	'.'
		jr	nz,_parfilen8
		set	4,b
		call	ParGetChar
_parfilen8:	ld	c,3
		call	ParName
		pop	hl
		ret

; ---------------------------------------------------------
; Subroutine parse name
; ---------------------------------------------------------
ParName:	ld	d,$00
		inc	c
		call	ParGetChar
		jr	z,_parname10
		call	ParUndoChar
		cp	SPACE
		jr	z,_parname10
		dec	c
_parname1:	inc	c
_parname2:	call	ParGetChar
		jr	z,_parname10
		bit	1,(iy+33)
		jr	z,_parname4
		dec	c
		dec	c
		jr	nz,_parname3
		ld	a,SPACE
_parname3:	inc	c
		inc	c
_parname4:	bit	4,(iy+33)
		jr	nz,_parname9
		bit	3,(iy+33)
		jr	nz,_parname6
		bit	2,(iy+33)
		jr	nz,_parname6
		cp	'*'
		jr	z,_parname7
		cp	'?'
		jr	nz,_parname6
_parname5:	set	5,b
_parname6:	set	0,b
		ld	d,1
		dec	c
		jr	z,_parname1
		ld	(hl),a
		inc	hl
		jr	_parname2

_parname7:	ld	a,c
_parname8:	ld	c,a
		dec	a
		jr	z,_parname5
		ld	(hl),'?'
		inc	hl
		jr	_parname8

_parname9:	call	ParUndoChar
_parname10:	dec	c
		ret	z
		ld	(hl),SPACE
		inc	hl
		jr	_parname10

; ---------------------------------------------------------
; Subroutine next item
; ---------------------------------------------------------
ParNext:	xor	a
		bit	3,(iy+32)
		ret	z
		push	de
		bit	6,(iy+32)
		set	6,(iy+32)
		call	Z,FirstDir
		pop	de
		or	a
		ret	nz
		bit	4,(iy+32)
		jr	z,_parnext1
		bit	0,(iy+32)
		ret	z
		ld	a,b
		and	$18
		ret	z
_parnext1:	call	NextDir
		or	a
		ret

; ---------------------------------------------------------
; Subroutine check if device and get device flags
; Output: a = device flags
; ---------------------------------------------------------
dskCheckDevice:	push	bc
		push	hl
		call	ValDevName
		ld	a,$00
		jr	nc,_checkdev1
		pop	af
		ld	(ix+fib_device+0),l
		ld	(ix+fib_device+1),h
		push	hl
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	(ix+fib_devjp+0),c
		ld	(ix+fib_devjp+1),b
		ld	bc,7
		add	hl,bc
		ld	a,(hl)
_checkdev1:	pop	hl
		pop	bc
		ret

; ---------------------------------------------------------
; Subroutine first directory
; ---------------------------------------------------------
FirstDir:	push	bc
		ld	c,(ix+fib_drive)
		ld	b,$00
		call	dskValFIB
		pop	bc
		or	a
		ret	nz
		push	bc
		push	hl
		bit	5,(iy+32)
		jr	nz,_firstdir6
		ld	de,30
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		call	CHK_C			; FAT16 (CLST_1)
		jr	nz,_firstdir6
		ld	bc,(varBB9E)
		push	bc
		push	hl
		ld	a,d
		or	e
		jr	z,_firstdir1
		res	3,(iy+32)
_firstdir1:	inc	hl
		ld	(varBB9E),hl
		ld	bc,-32
		add	hl,bc
		push	de
		call	InitWpath
		pop	de
		ld	b,$00
_firstdir2:	push	de
		ld	de,varB910
		call	dskParFileN
		bit	3,(iy+32)
		jr	z,_firstdir3
		ld	de,varB910
		call	NextDir
		or	a
		jr	nz,_firstdir4
		pop	af
		push	de
_firstdir3:	ld	de,varB910
		call	WpathAdd
		jr	nz,_firstdir4
		pop	de
		call	ParGetChar
		jr	nz,_firstdir2
		jr	_firstdir5

_firstdir4:	pop	de
		ld	de,$ffff
_firstdir5:	pop	hl
		ld	(hl),d
		dec	hl
		ld	(hl),e
		pop	bc
		ld	(varBB9E),bc
		set	3,(iy+32)
		call	CHK_C			; FAT16 (CLST_2)
		jr	z,_firstdir6
		inc	hl
		inc	hl
		ld	(hl),$00
_firstdir6:	pop	hl
		pop	bc
		jp	z,SelSubdir

; ---------------------------------------------------------
; Subroutine initialize whole path buffer and select root directory
; ---------------------------------------------------------
InitWpath:	call	WpathInit
		jp	SelRootdir

; ---------------------------------------------------------
; Subroutine next directory
; ---------------------------------------------------------
NextDir:	ld	a,_IPATH
		bit	3,b
		ret	z
		bit	5,b
		ret	nz
		push	de
		call	dskDirFirst
_nextdir1:	jr	z,_nextdir3
		ex	(sp),hl
		push	hl
		push	bc
		xor	a
		call	MatchDir
		pop	bc
		pop	hl
		ex	(sp),hl
		jr	nc,_nextdir2
		bit	4,a
		jr	nz,_nextdir4
_nextdir2:	call	dskDirNext
		jr	nc,_nextdir1
_nextdir3:	pop	de
		ld	a,_NODIR
		ret

_nextdir4:	pop	af
		call	dskSelSubdir
		ret

; ---------------------------------------------------------
; Subroutine does directory entry match the search
; Input:  DE	= pointer to directory entry
;	   HL	= pointer to search string
;	   A(b3) = volume
; Output: Cx set if match
;	   Zx set if free entry or matched
; ---------------------------------------------------------
MatchDir:	and	$08
		ld	c,a
		ld	a,(de)
		or	a
		ret	z
		cp	$e5
		ret	z
		push	de
		ld	b,11
		cp	$05
		jr	nz,_matchdir1
		ld	a,$e5
_matchdir1:	push	af
		ld	a,(hl)
		call	sysCheckChar
		pop	af
		bit	3,c
		jr	nz,_matchdir2
		sub	(hl)
		jr	z,_matchdir2
		bit	2,c
		jr	nz,_matchdir3
		ld	a,(hl)
		sub	$3f
		jr	nz,_matchdir3
_matchdir2:	inc	hl
		inc	de
		ld	a,(de)
		djnz	_matchdir1
		ex	de,hl
		ld	a,c
		xor	(hl)
		and	$08
		jr	nz,_matchdir3
		ld	a,(hl)
		scf
		db	$06
_matchdir3:	or	a
		pop	de
		ret

; ---------------------------------------------------------
; Subroutine copy name and expand wildcard
; Input:  hl = source
;         de = current directory entry
;         bc = destination
; ---------------------------------------------------------
dskMergeName:	push	bc
		ex	(sp),ix
		ld	bc,11*256+0
		ld	a,(de)
		cp	$05
		jr	nz,_mergename1
		ld	a,$e5
_mergename1:	push	de
		ld	d,a
		ld	a,(hl)
		call	sysCheckChar
		bit	2,c
		jr	nz,_mergename2
		cp	'?'
		jr	nz,_mergename2
		ld	a,d
_mergename2:	ld	(ix+0),a
		pop	de
		inc	de
		inc	hl
		inc	ix
		ld	a,(de)
		djnz	_mergename1
		ex	(sp),ix
		pop	bc
		ret

; ---------------------------------------------------------
; Subroutine test name
; ---------------------------------------------------------
TestName:	push	af
		call	dskCheckDot
		pop	bc
		jr	nc,_testname1
		xor	a
		ret

_testname1:	push	hl
		push	de
		ld	a,b
		or	a
		call	nz,ValDevName
		pop	hl
		ld	a,_IDEV
		jr	c,_testname3
		LD	BC,$0b09
		bit	3,(ix+31)
		jr	nz,_testname2
		ld	bc,$0800
		ld	a,$20
		call	ValName
		jr	nz,_testname3
		ld	bc,$0300
_testname2:	xor	a
		call	ValName
_testname3:	pop	hl
		or	a
		ret

; ---------------------------------------------------------
; Subroutine validate name
; Input:  HL = pointer
;	   A  = end marker
;	   B  = maximum length
;	   C  = character flags
; ---------------------------------------------------------
ValName:	cp	(hl)
		jr	z,_valname6
_valname1:	ld	a,(hl)
		call	sysCheckChar
		ld	(hl),a
		inc	hl
		bit	4,c
		jr	nz,_valname4
		bit	2,c
		jr	nz,_valname2
		bit	3,c
		jr	nz,_valname2
		CP	'?'
		jr	z,_valname6
		cp	'*'
		jr	z,_valname6
_valname2:	djnz	_valname1
		jr	_valname5

_valname3:	ld	a,(hl)
		inc	hl
_valname4:	cp	SPACE
		jr	nz,_valname6
		djnz	_valname3
_valname5:	xor	a
		ret

_valname6:	ld	a,_IFNM
		or	a
		ret

; ---------------------------------------------------------
; Subroutine check if device name
; Input:  DE = pointer to string
; Output: Cx = set if device name, reset if no device name
; ---------------------------------------------------------
ValDevName:	ld	hl,(varBBF4)
		push	hl
_valdevname1:	pop	hl
		ld	a,h
		or	l
		ret	z
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl
		push	bc
		push	hl
		push	de
		ld	bc,9
		add	hl,bc
		ld	b,8
_valdevname2:	ld	a,(de)
		cp	(hl)
		jr	nz,_valdevname3
		inc	de
		inc	hl
		djnz	_valdevname2
_valdevname3:	pop	de
		pop	hl
		jr	nz,_valdevname1
		pop	bc
		scf
		ret

; ---------------------------------------------------------
; Subroutine initialize whole path buffer
; ---------------------------------------------------------
WpathInit:	push	hl
		ld	hl,varB931+66
		ld	(hl),$02
		ld	hl,varB931
		ld	(hl),$00
		inc	hl
		ld	(varBB9A),hl
		pop	hl
		ld	(iy+25),$00

; ---------------------------------------------------------
; Subroutine terminate whole path buffer
; ---------------------------------------------------------
WpathTerm:	push	hl
		ld	a,2
		ld	hl,(varBB9A)
		cp	(hl)
		jr	z,_wpath2
		ld	(hl),$00
		inc	hl
		cp	(hl)
		jr	z,_wpath1
		ld	(hl),$00
_wpath1:	ld	a,'*'
_wpath2:	add	A,_NODIR
		pop	hl
		ret

; ---------------------------------------------------------
; Subroutine add item to whole path buffer
; ---------------------------------------------------------
WpathAdd:	push	bc
		push	hl
		ld	hl,(varBB9A)
		ld	a,b
		and	$18
		jr	z,_wpathadd6
		bit	6,b
		jr	z,_wpathadd2
		bit	7,b
		jr	z,_wpathadd4
_wpathadd1:	dec	hl
		ld	a,(hl)
		cp	$01
		jr	z,_wpathadd4
		or	a
		jr	nz,_wpathadd1
		ld	a,_IPATH
		jr	_wpathadd5

_wpathadd2:	push	hl
		ld	hl,varB901
		push	hl
		ld	(hl),$01
		inc	hl
		ld	a,(de)
		call	dskNameZ
		pop	de
		pop	hl
_wpathadd3:	ld	a,(hl)
		cp	$02
		ld	a,_PLONG
		jr	z,_wpathadd5
		ld	a,(de)
		ld	(hl),a
		inc	hl
		inc	de
		or	a
		jr	nz,_wpathadd3
		dec	hl
_wpathadd4:	ld	(varBB9A),hl
		call	WpathTerm
		jr	z,_wpathadd6
_wpathadd5:	ld	(iy+25),$ff
_wpathadd6:	pop	hl
		pop	bc
		bit	2,(iy+32)
		jr	z,_wpathadd7
		xor	a
_wpathadd7:	or	a
		ret

; ---------------------------------------------------------
; Subroutine make ASCIIZ string of file or volume name
; Input:  a  = first character
;	   de = pointer to filename
;	   hl = pointer to ASCIIZ buffer
;	   ix = pointer to FIB
; Output: de = de+11
;	   hl = points to end of ASCIIZ stromg
; ---------------------------------------------------------
dskNameZ:	push	bc

		; optimize: generic routine to fill table
		push	hl
		ld	bc,$0d00
_namez1:	ld	(hl),c
		inc	hl
		djnz	_namez1
		pop	hl

		ld	bc,$0B09
		bit	3,(ix+fib_search)
		jr	nz,_namez2
		ld	bc,$0800
		call	_namez_ext
		ld	(hl),'.'
		inc	hl
		ld	a,(de)
		ld	bc,$0300
_namez2:	call	_namez_ext
		ld	a,c
		pop	bc
		and	$81		; bit 7 or bit 0 is set?
		ret	nz		; nz=yes
		dec	hl
		ld	(hl),$00
		ret

_namez_ext:	inc	de
		call	sysCheckChar
		bit	2,c
		jr	nz,_namez_ext1
		bit	3,c
		jr	nz,_namez_ext1
		cp	SPACE
		jr	z,_namez_ext2
_namez_ext1:	set	7,c
		ld	(hl),a
		inc	hl
_namez_ext2:	ld	a,(de)
		djnz	_namez_ext
		ret

; ---------------------------------------------------------
; Subroutine get parse string character
; ---------------------------------------------------------
ParGetChar:	push	hl
		ld	hl,(varBB9E)
		ld	a,(hl)
		or	a
		jr	z,_pargetchar1
		inc	hl
		ld	(varBB9E),hl
_pargetchar1:	pop	hl
		push	bc
		ld	c,(iy+33)
		call	sysCheckChar
		ld	(iy+33),c
		pop	bc
		or	a
		ret

; ---------------------------------------------------------
; Subroutine undo get parse string character
; ---------------------------------------------------------
ParUndoChar:	push	hl
		ld	hl,(varBB9E)
		dec	hl
		ld	(varBB9E),hl
		res	1,(iy+33)
		res	2,(iy+33)
		pop	hl
		ret

; ---------------------------------------------------------
; *** Directory ***
; ---------------------------------------------------------

; Subroutine update FIB with directory entry info
; Input:  ix = pointer to FIB
;         de = pointer to directory entry
dskUpdateFIB:	push	ix
		ex	(sp),hl
		push	hl
		inc	hl
		ld	a,(de)
		cp	$05
		jr	nz,_updatefib1
		ld	a,$e5
_updatefib1:	call	dskNameZ
		pop	hl
		ld	bc,14
		add	hl,bc
		ld	a,(de)
		ld	(hl),a
		ex	de,hl
		inc	de
		ld	bc,11
		add	hl,bc
		ld	bc,10
		ldir
		pop	hl
		ret

; ---------------------------------------------------------

; Subroutine validate FIB for re-use
dskUseFIB:	push	bc
		bit	7,(ix+fib_mode)
		jr	z,_usefib1
		call	UseDevFIB
		pop	bc
		xor	a
		scf
		ret

_usefib1:	call	GetDirEntry
		ldir
		ld	a,(ix+fib_sdir)		; FAT16 STOR_8
		ld	(varSdir1),a		; "
		ld	c,(ix+fib_drive)
		ld	b,1
		call	dskValFIB
		pop	bc
		or	a
		ret	nz
		call	dskDirEntry
		xor	a
		ret

; Subroutine validate device FIB for re-use
UseDevFIB:	ld	l,(ix+fib_device+0)
		ld	h,(ix+fib_device+1)
		push	hl
		ld	de,9
		add	hl,de
		push	hl
		ld	de,22
		add	hl,de
		ex	de,hl
		push	ix
		pop	hl
		ld	bc,15
		add	hl,bc
		ld	bc,4
		ldir
		pop	de
		pop	hl
		ret

; Subroutine get pointer to directory entry locators
; Input:  ix = pointer to FIB
; Output: HL = pointer to FIB directory entry locators
;         DE = pointer to directory entry locators
GetDirEntry:	ld	de,varBBDE
		push	ix
		pop	hl
		ld	bc,43
		add	hl,bc
		ld	bc,12
		ret

; Subroutine get directory entry
; Input:  DE = file name buffer
dskFindFirst:	bit	2,(iy+47)
		jr	nz,_dskff1
		push	de
		ld	a,(ix+31)
		and	$10
		call	TestName
		pop	de
		ret	nz
		bit	7,(ix+fib_search)
		jr	z,_dskff1
		set	3,(iy+47)
_dskff1:	res	7,(ix+fib_search)
		ld	(varBBAD),de
		bit	7,(ix+fib_mode)
		jr	nz,_dskff2
		call	dskDirFirst
		call	dskFindFIB
		jr	_dskfn2

_dskff2:	push	hl
		ld	de,9
		add	hl,de
		ex	de,hl
		call	DirTime
		pop	hl
		jp	_dskfn11

; Subroutine get next directory entry
; Input:  de = pointer to file name buffer
dskFindNext:	bit	7,(ix+fib_mode)
		ld	a,_NOFIL
		ret	nz
		set	1,(iy+41)
		ld	(varBBAD),de
_dskfn1:	call	FindNextFIB
_dskfn2:	jr	c,_dskfn3
		bit	2,(iy+47)
		ld	a,_NOFIL
		call	z,dskGetEntry
		ret	nz
		jr	_dskfn6

_dskfn3:	bit	2,(iy+47)
		jr	z,_dskfn4
		bit	3,a
		jr	nz,_dskfn11
		and	$16
		cpl
		or	(ix+fib_search)
		inc	a
		jr	nz,_dskfn1
		jr	_dskfn11

_dskfn4:	ld	b,a
		bit	3,(iy+47)
		jr	nz,_dskfn5
		bit	2,b
		ld	a,_SYSX
		jr	nz,_dskfn12
		bit	4,b
		ld	a,_DIRX
		jr	nz,_dskfn12
		bit	4,(ix+fib_search)
_dskfn5:	ld	a,_FILEX
		jr	nz,_dskfn12
		call	dskDirEntry
		xor	a
		call	dskDeleteFIB
		push	af
		call	dskDirEntry
		pop	af
		or	a
		jr	nz,_dskfn12
_dskfn6:	ld	de,(varBBAD)
		call	dskCheckDot
		jr	nc,_dskfn7
		or	a
		ret

_dskfn7:	ld	bc,0
		bit	4,(ix+fib_search)
		jr	z,_dskfn8
		ld	a,$ff
		inc	bc
		call	AllocClus
		ret	nz
		call	InitSubdir
_dskfn8:	call	dskDirEntry
		ld	a,$28
		bit	3,(ix+fib_search)
		jr	nz,_dskfn10
		ld	a,(ix+fib_search)
		set	5,a
		bit	4,a
		jr	z,_dskfn9
		and	$da
_dskfn9:	and	$3f
_dskfn10:	push	hl
		ld	hl,(varBBAD)
		call	SetupDir
		pop	hl
_dskfn11:	xor	a
_dskfn12:	cp	a
		ret

; Subroutine setup the first two directory entries
InitSubdir:	push	de
		ld	d,b
		ld	e,c
		xor	a
		call	ClusToSec
		push	hl
		ex	(sp),ix
		push	bc
		ld	b,1
		call	BUF_2F		; FAT16
		pop	bc
		push	bc
		ld	de,11
		add	hl,de
		ex	de,hl
		ld	hl,dskDot2
		ld	a,$10
		call	SetupDir
		ld	hl,32
		add	hl,de
		ex	de,hl
		ld	hl,dskDot1
		ld	bc,(varBBE8)
		ld	a,$10
		call	SetupDir
		pop	bc
		ex	(sp),ix
		pop	hl
		pop	de
		ret

; Subroutine setup directory entry
; Input:  HL = pointer to file name
;	   DE = pointer to directory entry
;	   BC = cluster
;	   A  = attribute
SetupDir:	push	de
		push	bc
		ld	b,a
		ld	a,(hl)
		cp	$e5
		jr	nz,_setupdir1
		ld	a,$05
_setupdir1:	ld	(de),a
		ld	a,b
		inc	hl
		inc	de
		ld	bc,10
		ldir
		call	dskBufDirty
		ld	(de),a
		ex	de,hl
		ld	b,$14
_setupdir2:	inc	hl
		ld	(hl),c
		djnz	_setupdir2
		pop	de
		call	CHK_C			; FAT16 (CLST_8)
		jr	z,_setupdir3
		ld	d,b
		ld	e,b
_setupdir3:	ld	bc,-4
		add	hl,bc
		ld	(hl),d
		dec	hl
		ld	(hl),e
		pop	de

; Subroutine change time and date directory entry
; Input:  DE = pointer to directory entry
DirTime:	push	hl
		push	bc
		push	de
		ld	hl,22
		add	hl,de
		push	hl
		push	ix
		ld	ix,timReadRTC		; date/time functions are in code segment 1
		call	sysFarDos
		pop	ix
		ex	(sp),hl
		ld	a,c
		add	a,a
		add	a,a
		ld	c,3
_dirtime1:	add	a,a
		rl	b
		dec	c
		jr	nz,_dirtime1
		srl	e
		add	a,e
		ld	(hl),a
		inc	hl
		ld	(hl),b
		inc	hl
		pop	bc
		ld	a,b
		or	a
		rra
		rra
		rra
		rra
		rl	d
		add	a,c
		ld	(hl),a
		inc	hl
		ld	(hl),d
		pop	de
		pop	bc
		pop	hl
		ret

; Subroutine search next directory entry
FindNextFIB:	call	dskDirNext

; Subroutine search directory entry
dskFindFIB:	jr	c,_findfib1
		push	ix
		ex	(sp),hl
		ld	hl,(varBBAD)
		ld	a,(ix+fib_search)
		call	MatchDir
		pop	hl
		ret	c
		jr	nz,FindNextFIB
		bit	0,(iy+47)
		call	z,dskSavEntry
		or	a
		jr	nz,FindNextFIB
_findfib1:	xor	a
		ret

; Subroutine register free directory entry
dskSavEntry:	push	hl
		push	de
		push	bc
		call	GetDirEntry
		ex	de,hl

		; FAT16 STOR_1
		push	af
		ld	a,(varSdir1)	; BBE2h-3h
		ld	(varSdir2),a	; BBD6h-7h
		pop	af

		ld	de,varBBD2
		ldir
		set	0,(iy+47)
		pop	bc
		pop	de
		pop	hl
		ret

; Subroutine get free or registered directory entry
dskGetEntry:	push	bc
		bit	0,(iy+47)
		jr	z,_getentry1
		push	hl
		call	GetDirEntry

		; FAT16 STOR_2
		push	af
		ld	a,(varSdir2)
		LD	(varSdir1),a
		pop	af

		ld	hl,varBBD2
		ldir
		pop	hl
		jr	_getentry2

_getentry1:	ld	de,(varBBE6)
		ld	a,d
		call	CHK_A			; FAT16 (CLST_3)
		ld	a,_DRFUL
		jr	nz,_getentry3
		ld	a,$ff
		ld	bc,1
		call	AllocClus
		jr	nz,_getentry3
		push	bc
		call	SetFatEntry
		pop	bc
		ld	(varBBE4),bc
		call	dskDirNext
_getentry2:	xor	a
_getentry3:	pop	bc
		ret

; Subroutine select sub directory
dskSelSubdir:	push	hl
		ld	hl,26
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		pop	hl
		ld	a,d
		or	e
		jr	nz,SelSubdir

; Subroutine select root directory
SelRootdir:	ld	de,$ffff

; Subroutine select sub directory (cluster number)
SelSubdir:	ld	(varBBE8),de
		push	hl
		ld	hl,(varBBAB)
		xor	a
		sbc	hl,de
		jr	nz,_selsubdir1
		set	1,(iy+47)
_selsubdir1:	pop	hl
		ret

; Subroutine get first directory entry
; Input:  HL = pointer to drive table
dskDirFirst:	ld	de,(varBBE8)
		call	CHK_C			; FAT16 (CLST_4)
		jr	z,_dirnext3
		push	bc
		push	hl
		ld	bc,15
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		pop	hl
		ld	b,$ff
		push	bc
		push	bc
		jr	_dirnext4

; Subroutine get current directory entry
; Input:  hl = pointer to drive table
dskDirEntry:	push	hl
		ld	de,8
		add	hl,de
		bit	7,(hl)
		ex	de,hl
		inc	de
		pop	hl
		jp	nz,_dirnext11
		set	1,(iy+41)
		inc	(iy+96)
		push	bc
		ld	bc,0
		jr	_dirnext0

; Subroutine get next directory entry
; Input:  hl = pointer to drive table
;         de = pointer to current directory entry
dskDirNext:	push	bc
		ld	bc,32
_dirnext0:	push	hl
		ex	de,hl
		add	hl,bc
		ld	de,(varBBE2)
		ld	a,(varBBE0)
		dec	a
		jr	z,_dirnext1
		bit	1,(iy+41)
		jp	z,_dirnext10
		jr	_dirnext8

_dirnext1:	pop	hl
		inc	de

		; FAT16 _dirnext216
		ld	a,d
		or	e
		jr	nz,_dirnext2
		ld	a,(varSdir1)
		inc	a
		ld	(varSdir1),a

_dirnext2:	ld	a,(varBBE1)
		dec	a
		jr	nz,_dirnext5
		cp	(iy+94)
		jr	nz,_dirnext6

		; FAT16 (CLST_9)
		ld	de,(varBBE8)
		call	CHK_C
		ld	de,(varBBE4)
		call	z,CHK_C

		scf
		pop	bc
		ret	nz
_dirnext3:	push	bc
		push	de
		call	FatEntry
		ld	b,d
		ld	c,e
		pop	de
		push	de
		xor	a
		call	ClusToSec

		; FAT16 GETSUB
		ld	a,(varBit16)
		ld	(varSdir1),a

		push	de
		ex	de,hl
		ld	hl,10
		add	hl,de
		ld	a,(hl)
		inc	a
		ex	de,hl
		pop	de
		push	bc
		ld	c,$00
_dirnext4:	ld	(iy+94),c
		pop	bc
		ld	(varBBE4),bc
		pop	bc
		ld	(varBBE6),bc
_dirnext5:	ld	b,$10
_dirnext6:	or	a
		jr	nz,_dirnext7
		ld	b,(iy+94)
		ld	(varBBDE),a
		inc	a
_dirnext7:	ld	(varBBE1),a
		ld	(varBBE2),de
		ld	(iy+95),b
		ld	a,b
		push	hl
_dirnext8:	ex	(sp),ix
		push	af
		ld	b,1
		call	BUF_4F			; FAT16
		pop	bc
		ex	(sp),ix
		ld	de,-21
		add	hl,de
		ld	a,(varBBDF)
		inc	a
		sub	b
		ld	de,32
_dirnext9:	add	hl,de
		dec	a
		jr	nz,_dirnext9
		ld	a,b
		res	1,(iy+41)
_dirnext10:	ld	(varBBE0),a
		ex	de,hl
		pop	hl
		pop	bc
_dirnext11:	ld	a,(de)
		or	a
		ret

; ---------------------------------------------------------

; Subroutine get directory entry
DirGet:		bit	7,(ix+fib_mode)
		jr	z,_dirget1
		call	UseDevFIB
		xor	a
		ret

_dirget1:	ld	c,(ix+fib_drive)
		ld	b,1
		call	dskValFIB
		or	a
		ret	nz
		ld	e,(ix+fab_parentcl+0)
		ld	d,(ix+fab_parentcl+1)
		ld	(varBBE8),de
		push	ix
		ex	(sp),hl
		ld	bc,1
		add	hl,bc
		ld	(varBB9E),hl
		pop	hl
		ld	de,varB926
		call	dskParFileN
		or	a
		ld	a,_IFNM
		ret	nz
		ld	de,varB926
		ld	(iy+47),$04
		call	dskFindFirst
		or	a
		ret

; Subroutine ensure directory entry (when file is modified)
dskEnsureFIB:	xor	a
		bit	7,(ix+fab_access)
		ret	z

; Subroutine ensure directory entry
dskEnsure2FIB:	bit	3,(ix+fab_access)
		ld	a,_HDEAD
		ret	nz
		call	DirGet
		ret	nz
		bit	7,(ix+fib_mode)
		ret	nz
		bit	7,(ix+fab_access)
		ret	z
		call	DirTime
		push	ix
		ex	(sp),hl
		ld	bc,21
		add	hl,bc
		ex	de,hl
		ld	bc,11
		add	hl,bc
		set	5,(hl)
		ld	bc,15
		add	hl,bc
		ld	c,(ix+fab_startcl+0)
		ld	b,(ix+fab_startcl+1)
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ex	de,hl
		ld	bc,4
		ldir
		call	dskBufDirty
		pop	hl

; Subroutine update FIB (directory entry changed)
dskUpdFIB:	call	dskDirEntry
		ld	a,(ix+fab_access)

; Subroutine setup FIB for open
dskSetupFIB:	push	bc
		ex	af,af'
		call	dskUpdateFIB
		ex	af,af'
		and	$07
		ld	(ix+fab_access),a
		ld	(ix+fab_dub+0),l
		ld	(ix+fab_dub+1),h
		ld	bc,(varBBE2)
		ld	(ix+fab_dirsec+0),c
		ld	(ix+fab_dirsec+1),b

		; FAT16 STOR_3
		ld	a,(varSdir1)
		ld	(ix+$32),a		; file handle

		ld	a,(varBBDF)
		sub	(iy+96)
		ld	(ix+36),a
		ld	bc,(varBBE8)
		ld	(ix+fab_parentcl+0),c
		ld	(ix+fab_parentcl+1),b
		ld	c,(ix+fib_clstart+0)
		ld	b,(ix+fib_clstart+1)
		ld	(ix+fab_startcl+0),c
		ld	(ix+fab_startcl+1),b
		ld	(ix+fab_abscl+0),c
		ld	(ix+fab_abscl+1),b
		xor	a
		ld	(ix+fab_logcl+0),a
		ld	(ix+fab_logcl+1),a
		pop	bc
		ret

; ---------------------------------------------------------

; Subroutine check if file is opened by some other FIB
dskCheckOpen:	push	de
		push	bc
		ex	de,hl
		push	ix
		pop	bc
		ld	ix,varBBF2+3
_checkopen1:	ld	l,(ix-3)
		ld	h,(ix-2)
		ld	a,h
		or	l
		jr	z,_checkopen2
		push	de
		ld	de,3
		add	hl,de
		pop	de
		push	hl
		pop	ix
		or	a
		sbc	hl,bc
		jr	z,_checkopen1
		ex	de,hl
		call	dskCompareFIB
		ex	de,hl
		jr	nz,_checkopen1
		ld	a,_FOPEN
_checkopen2:	push	bc
		pop	ix
		ex	de,hl
		pop	bc
		pop	de
		or	a
		ret

; Subroutine compare with FIB
; Input:  ix = pointer to FIB
dskCompareFIB:	bit	7,(ix+fib_mode)
		ret	nz
		ld	a,(varBBDF)
		sub	(iy+96)
		cp	(ix+fab_dirent)
		ret	nz
		push	hl
		ld	de,(varBBE2)
		ld	l,(ix+fab_dirsec+0)
		ld	h,(ix+fab_dirsec+1)
		or	a
		sbc	hl,de

		; FAT16 STOR_4
		jr	nz,STOR4A
		ld	a,(varSdir1)
		sub	(ix+$32)

STOR4A:		pop	hl
		ret	nz
		ld	e,(ix+fab_dub+0)
		ld	d,(ix+fab_dub+1)
		xor	a
		sbc	hl,de
		add	hl,de
		ret

; Subroutine mark current directory entry deleted and remove FAT chain
; Input:  A = 0 (deleted is not recoverable), <> 0 (delete is recoverable)
dskDeleteFIB:	ld	c,a
		xor	a
		bit	7,(ix+fib_mode)
		ret	nz
		call	dskCheckOpen
		ret	nz
		push	hl
		ld	hl,11
		add	hl,de
		ld	b,(hl)
		pop	hl
		bit	0,b
		ld	a,_FILRO
		ret	nz
		bit	4,b
		jr	z,_deletefib3
		call	dskCheckDot
		ret	c
		call	dskSavEntry
		call	dskSelSubdir
		call	dskDirFirst
_deletefib1:	jr	z,_deletefib2
		call	dskCheckDot
		ld	a,_DIRNE
		ret	nz
		call	dskDirNext
		jr	nc,_deletefib1
_deletefib2:	call	dskGetEntry
_deletefib3:	call	dskDirEntry
		ld	a,c
		push	hl
		ld	hl,26
		add	hl,de
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		pop	hl
		ld	d,b
		ld	e,c
		ld	a,d
		or	e
		call	NZ,DelChain
		call	dskDirEntry
		push	hl
		ld	hl,12
		add	hl,de
		ld	a,(de)
		ld	(hl),a
		ld	a,$e5
		ld	(de),a
		pop	hl
_deletefib4:	call	dskChangedFIB
		call	nz,dskZapDir
		xor	a
		ret

; Subroutine rename current directory entry
dskRenameFIB:	xor	a
		bit	7,(ix+fib_mode)
		ret	nz
		call	dskCheckOpen
		ret	nz
		call	dskCheckDot
		ret	c
		ld	(varBB9E),bc
		push	de
		ld	de,varB91B
		call	dskParFileN
		pop	de
		or	a
		ld	a,_IFNM
		ret	nz
		push	hl
		ld	hl,varB91B
		ld	bc,varB91B
		call	dskMergeName
		pop	hl
		ld	de,varB91B
		ld	(varBBAD),de
		ld	a,$ff
		call	TestName
		ret	nz
		call	dskCheckDot
		ret	c
		call	dskSavEntry
		call	dskDirFirst
		call	dskFindFIB
		ld	a,_DUPF
		ret	c
		call	dskGetEntry
		call	dskDirEntry
		push	hl
		push	de
		ld	hl,varB91B
		ld	b,11
		ld	a,(hl)
		cp	$e5
		jr	nz,_renamefib1
		ld	a,$05
_renamefib1:	ld	(de),a
		inc	hl
		inc	de
		ld	a,(hl)
		djnz	_renamefib1
		pop	de
		pop	hl
		jp	_deletefib4

; ---------------------------------------------------------
; *** Disk i/o and buffers ***
; ---------------------------------------------------------

; Subroutine update timestamp directory entry
dskTimeFIB:	xor	a
		bit	7,(ix+fib_mode)
		ret	nz
		call	dskCheckOpen
		ret	nz
		push	hl
		push	de
		call	dskDirEntry
		ld	hl,22
		add	hl,de
		ex	de,hl
		ex	(sp),hl
		ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		pop	de
		pop	hl

; Subroutine mark directory entry as changed
dskChangedFIB:	push	hl
		ld	hl,11
		add	hl,de
		bit	4,(hl)
		jr	nz,_changedfib1
		set	5,(hl)
_changedfib1:	call	dskBufDirty
		pop	hl
		ld	a,$00
		ret

; Subroutine check if special subdirectory directory entry
; Output: Cx = set if special directory entry
;         Zx = set if free or special directory entry
dskCheckDot:	push	hl
		push	bc
		ld	hl,dskDot2
		xor	a
		call	MatchDir
		jr	z,_checkdskDot1
		ld	hl,dskDot1
		xor	a
		call	MatchDir
_checkdskDot1:	pop	bc
		pop	hl
		ld	A,_DOT
		ret

dskDot1:	db	"."
dskDot2:	db	".          "

; Subroutine read/write sectors
; Input:  de = transfer address
;         b  = number of sectors
dskRwSectors:	xor	a
		cp	b
		ret	z
		ld	a,(varBBC4)
		and	$04
		ld	de,(varBBC2)
		;call	dskGetSegment
		;set	7,d
		ex	af,af'
		push	hl
		ld	hl,ABE00
		or	a
		sbc	hl,de
		jr	c,_rwsectors6
		ld	a,h
		srl	a
		inc	a
		cp	b
		jr	c,_rwsectors1
		ld	a,b
_rwsectors1:	ld	c,a
		pop	hl
		sub	b
		neg
		ld	b,a
		push	bc
		ld	b,c
		ex	af,af'
		ld	c,a
		push	bc
		xor	a
_rwsectors2:	push	bc
		push	de
		push	de
		ex	(sp),ix
		ld	de,(varBBB4)

		; FAT16 RAMRED
		; Read a sector for Random block access
		push	af
		ld	a,(varBit16)
		LD	(varRW16),a	; write bit16-23 of sector number
		pop	af

		push	de
		dec	a
		jr	z,_rwsectors4
		ld	a,0
		bit	0,(iy+68)
		jr	nz,_rwsectors3
		ld	a,1
_rwsectors3:	call	DriverRwSec
		jr	_rwsectors5
_rwsectors4:	ld	b,1
_rwsectors5:	ex	af,af'
		pop	de
		pop	ix
		push	hl
		ld	l,b
		ld	h,$00
		add	hl,de
		ld	(varBBB4),hl
		jr	nc,_secinc		; FAT16 (NUM_1)
		inc	(iy-6)			; varBit16
_secinc:	pop	hl
		pop	de
		ld	a,d
		add	a,b
		add	a,b
		ld	d,a
		ld	a,b
		pop	bc
		sub	b
		neg
		ld	b,a
		ex	af,af'
		push	de
		ld	de,(varBBB4)
		call	nz,HandleIgnErr
		pop	de
		inc	b
		dec	b
		jr	nz,_rwsectors2
		pop	bc
		call	CorrectBuffer
		pop	bc
		ld	a,(iy+67)
		add	a,c
		add	a,c
		ld	(iy+67),a
		jp	dskRwSectors

_rwsectors6:	pop	hl
		dec	b
		push	bc
		ld	de,512
		ld	b,e
		ld	c,e
		call	rwSec
		pop	bc
		inc	(iy+52)
		jr	nz,_rwsectors7
		inc	(iy+53)
		jr	nz,_rwsectors7		; FAT16 (NUM_2)
		inc	(iy-6)			; varBit16
_rwsectors7:	jp	dskRwSectors

; Subroutine correct buffer
CorrectBuffer:	call	FlagDrives
_correctbuf1:	push	bc
		exx
		call	FindFlagBuf
		jp	z,_correctbuf6
		ld	bc,4
		add	hl,bc
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		ld	bc,(varBBB4)

		; FAT16 BEGIN DSKBUF
		sbc	hl,bc
		push	af
		push	de
		dec	de
		dec	de
		dec	de
		ex	de,hl
		ld	a,(hl)		; drive number of buffer
		dec	a
		add	a,a
		ld	hl,varBA23+2	; (DRIVE-1)*2+BA25h=(DPB address)
		add	a,l
		ld	l,a
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		add	a,$1d
		ld	l,a
		ld	a,0
		adc	a,h		; HL=DPB+1Dh
		bit	7,(hl)		; Z=1 FAT16  Z=0 FAT12
		ex	de,hl
		pop	de
		jr	z,CALUC		; FAT16
		pop	af
		pop	bc
		jr	_correctbuf2

CALUC:		pop	af
		push	de
		inc	de
		inc	de
		inc	de
		ld	a,(de)
		ld	bc,(varBit16)
		sbc	a,c
		pop	de
		pop	bc
		inc	a
		jp	nz,_correctbuf4
		ld	a,l
		add	a,b
		jr	nc,_correctbuf3
		ld	a,0
		adc	a,h
		jr	_correctbuf3
		; FAT16 END DSKBUF

_correctbuf2:	ld	a,h
		inc	a
		jr	nz,_correctbuf4
		ld	a,l
		add	a,b
_correctbuf3:	jr	nc,_correctbuf4
		dec	de
		dec	de
		ex	de,hl
		bit	0,(iy+68)
		jr	nz,_correctbuf5
		dec	hl
		ld	(hl),$00
		dec	hl
		dec	hl
		call	BufStart
_correctbuf4:	exx
		jr	_correctbuf1

_correctbuf5:	bit	7,(hl)
		jr	z,_correctbuf4
		ld	a,c
		exx
		push	de
		exx
		ld	bc,8
		add	hl,bc
		ex	(sp),hl
		ld	d,e
		ld	e,b
		add	hl,de
		add	hl,de
		ex	de,hl
		res	7,d
		pop	hl
		ld	bc,512
		ldir				; old: call P0_LDIR
		jr	_correctbuf4

_correctbuf6:	exx
		pop	bc
		ret

; Subroutine read/write sector
; Input:  hl = pointer to drive table
;         ix = pointer to FIB
;         de = number of bytes
;         bc = start offset
rwSec:		push	hl
		ex	(sp),ix
		push	de
		push	bc
		bit	0,(iy+68)
		jr	nz,_rwsec2
		bit	1,d
		jr	nz,_rwsec3
		bit	1,(iy+68)
		jr	nz,_rwsec2
		push	de
		ld	hl,(varBBC0)
		ld	de,(varBBB2)
		or	a
		sbc	hl,de
		pop	de
		jr	c,_rwsec1
		ld	a,b
		or	c
		jr	nz,_rwsec2
		sbc	hl,de
		jr	c,_rwsec3
		jr	_rwsec2

_rwsec1:	add	hl,bc
		jr	nc,_rwsec3
_rwsec2:	ld	b,3
		db	$21
_rwsec3:	ld	b,2
		ld	de,(varBBB4)
		call	BUF_2F			; FAT16
		pop	de
		add	hl,de
		ld	bc,11
		add	hl,bc
		pop	bc
		push	de
		push	bc
		ld	de,(varBBC2)
		push	de
		ld	a,(varBBC4)

		; replace with normal ldir
		;call	dskSegLdir
		bit	0,a
		jr	nz,_doldir
		ex	de,hl
_doldir:	call	XFER
		jr	nz,_endldir
		ex	de,hl
_endldir:

		pop	hl
		pop	bc
		add	hl,bc
		ld	(varBBC2),hl
		bit	0,(iy+68)
		call	z,dskBufDirty
		pop	hl
		add	hl,bc
		bit	1,h
		ld	hl,(varBBF6)
		call	nz,FlushStart
		ex	(sp),ix
		pop	hl
		ret

; Subroutine write to FIB
; Input:  bc = size
;	   de = transfer address
dskWriteFIB:	and	$04			; segment type
		jr	dskRwFIB

; Subroutine read from FIB
; Input:  a  = operation flags
;	   bc = size
;	   de = transfer address
dskReadFIB:	and	$04			; segment type
		or	$01			; read flag

; Subroutine read/write from FIB
; Input:  bc = size
;	   de = transfer address
;	   ix = pointer to FIB
;	   a  = operation flags
dskRwFIB:	bit	7,(ix+fib_mode)		; device?
		jr	nz,_rwfib04		; nz=yes, r/w device
		ld	(varBBC4),a
		and	$10
		or	b
		or	c
		ret	z
		xor	a
		ld	(varBBBE+0),a
		ld	(varBBBE+1),a
		ld	(varBBC0),bc
		ld	(varBBC2),de
		call	ReadWriteFile
		ld	bc,(varBBBE)
		ld	de,(varBBC2)
_rwfib01:	ld	l,(ix+fab_file+0)
		ld	h,(ix+fab_file+1)
		add	hl,bc
		ld	(ix+fab_file+0),l
		ld	(ix+fab_file+1),h
		jr	nc,_rwfib02
		inc	(ix+fab_file+2)
		jr	nz,_rwfib02
		inc	(ix+fab_file+3)
_rwfib02:	or	a
		ret	nz
		bit	4,(iy+68)
		jr	nz,_rwfib03
		ld	a,b
		or	c
		ld	a,_EOF
		jr	z,_rwfib02
_rwfib03:	xor	a
		ret

; r/w device FIB
_rwfib04:	ld	h,a
		ld	a,b
		or	c
		ret	z
		bit	0,h			; read or write?
		jr	nz,_rwfib10		; nz=read
		ld	l,(ix+fib_devjp+0)
		ld	h,(ix+fib_devjp+1)
		inc	hl
		inc	hl
		inc	hl
		push	bc
_rwfib05:	ex	de,hl
		call	RD_RAM			; a=(hl)
		ex	de,hl
		bit	5,(ix+fib_mode)		; ASCII mode?
		jr	z,_rwfib06		; z=no
		cp	$1a			; CTRL+Z?
		jr	z,_rwfib07		; z=yes

; write binary
_rwfib06:	push	ix
		push	bc
		push	de
		push	hl
		call	ReadWriteDev
		pop	hl
		pop	de
		pop	bc
		pop	ix
		or	a			; device error?
		jr	nz,_rwfib08		; nz=yes
		inc	de
		dec	bc
		ld	a,b
		or	c
		jr	nz,_rwfib05
		jr	_rwfib08
_rwfib07:	xor	a
		inc	de
		dec	bc
_rwfib08:  	pop	hl
		or	a
		sbc	hl,bc
		ld	b,h
		ld	c,l
_rwfib09:	jr	_rwfib01

; read device
_rwfib10:	ld	l,(ix+fib_devjp+0)
		ld	h,(ix+fib_devjp+1)
		push	bc
		res	6,(ix+fib_mode)
_rwfib11:	push	bc
		push	ix
		push	de
		push	hl
		call	ReadWriteDev
		pop	hl
		pop	de
		pop	ix
		ld	c,$00
		or	a			; device error?
		jr	z,_rwfib13		; z=no
		cp	_EOF
		jr	z,_rwfib12
		cp	_EOL
		jr	nz,_rwfib14
		bit	2,(ix+fib_mode)
		jr	nz,_rwfib13
		inc	c
_rwfib12:	inc	c
		bit	5,(ix+fib_mode)
		jr	nz,_rwfib13
		ld	c,$00
_rwfib13:	ex	de,hl
		ld	a,b
		call	WR_RAM
		ex	de,hl
		ld	a,c
		pop	bc
		dec	a			; end of file?
		jr	z,_rwfib15		; z=yes
		inc	de
		dec	bc
		dec	a			; end of line?
		jr	z,_rwfib16		; z=yes
		ld	a,b
		or	c
		jr	nz,_rwfib11
		jr	_rwfib16
_rwfib14:	pop	bc
		jr	_rwfib16

_rwfib15:	set	6,(ix+fib_mode)
_rwfib16:	pop	hl
		or	a
		sbc	hl,bc
		ld	b,h
		ld	c,l
		jr	_rwfib09

; Subroutine call device handler
ReadWriteDev:	ld	c,(ix+fib_mode)		; ASCII/Binary mode
		jp	(hl)			; RDCH / WRCH routine

; Subroutine read/write file
ReadWriteFile:	ld	a,_HDEAD		; file handle deleted
		bit	3,(ix+fab_access)
		ret	nz
		ld	a,_ACCV			; file access violation
		bit	0,(iy+68)
		jr	z,_rwfile1
		bit	1,(ix+fab_access)
		ret	nz
		jr	_rwfile2

_rwfile1:	bit	0,(ix+fab_access)
		ret	nz
		ld	a,_FILRO
		bit	0,(ix+fib_attrib)
		ret	nz
_rwfile2:	bit	0,(iy+127)
		jr	z,_rwfile3
		ld	hl,(varBBC2)
		add	hl,bc
		ld	a,_OV64K
		ret	c
_rwfile3:	ld	c,(ix+fib_drive)
		ld	b,1
		call	dskValFIB
		or	a
		ret	nz
		call	StartRwFile
		ret	nz
		ld	bc,(varBBC0)
		ld	a,b
		or	c
		ret	z
		ld	de,(varBBBC)
		call	LogToPhysCl
		ld	(varBBB6),de
		ret	nz
		ld	bc,(varBBB9)
		ld	a,b
		or	c
		call	nz,CopySecPart
		ret	nz
_rwfile4:	ld	c,(iy+65)
		srl	c
		jr	z,_rwfile10
		call	UpdNextSec
		ret	nz
		sub	(iy+59)
		ld	b,a
		ld	de,(varBBB6)
		call	CalcSec
_rwfile5:	ld	a,b
		add	a,(iy+56)
		ld	b,a
		cp	c
		jr	nc,_rwfile8
		push	de
		call	FatEntry
		ex	(sp),hl
		or	a
		inc	hl
		sbc	hl,de
		jr	nz,_rwfile7
		inc	(iy+60)
		jr	nz,_rwfile6
		inc	(iy+61)
_rwfile6:	pop	hl
		jr	_rwfile5

_rwfile7:	add	hl,de
		ex	de,hl
		dec	de
		pop	hl
		ld	a,(varBBB8)
		jr	_rwfile9

_rwfile8:	ld	a,(varBBB8)
		sub	b
		add	a,c
		ld	b,c
_rwfile9:	dec	a
		ld	(varBBBB),a
		ld	(varBBB6),de
		push	bc
		call	dskRwSectors
		pop	bc
		sla	b
		ld	c,$00
		call	UpdDoneLeft
		set	3,(iy+68)
		jr	_rwfile4

_rwfile10:	ld	bc,$0000
		call	CopySecPart
		ret	nz
		ld	de,(varBBB6)
		ld	(ix+fab_abscl+0),e
		ld	(ix+fab_abscl+1),d
		ld	de,(varBBBC)
		ld	(ix+fab_logcl+0),e
		ld	(ix+fab_logcl+1),d
		ret

; Subroutine transfer sector partly
; Inputs  BC = offset, (BC) = size
CopySecPart:	push	hl
		ld	hl,512
		or	a
		sbc	hl,bc
		ex	de,hl
		ld	hl,(varBBC0)
		sbc	hl,de
		jr	nc,_copysecpart1
		add	hl,de
		ld	e,l
		ld	d,h
_copysecpart1:	ld	a,d
		or	e
		pop	hl
		ret	z
		call	UpdNextSec
		ret	nz
		call	CalcSec
		call	rwSec
		set	3,(iy+68)
		call	UpdDoneLeft
		xor	a
		ret

; Subroutine update to next sector
UpdNextSec:	xor	a
		bit	3,(iy+68)
		ret	z
		res	3,(iy+68)
		ld	a,(varBBBB)
		inc	a
		cp	(iy+56)
		jr	nz,_updnextsec1
		push	de
		ld	de,(varBBBC)
		inc	de
		ld	(varBBBC),de
		ld	de,(varBBB6)
		call	FatEntry
		ld	(varBBB6),de
		pop	de
		ld	a,_FILE
		ret	nz
		xor	a
_updnextsec1:	ld	(varBBBB),a
		xor	a
		ret

; Subroutine calculate physical sector from physical cluster and sector in cluster
CalcSec:	push	de
		ld	a,(varBBBB)
		ld	de,(varBBB6)
		call	ClusToSec
		ld	(varBBB4),de
		pop	de
		ret

; Subroutine update size done, size left
UpdDoneLeft:	push	hl
		ld	hl,(varBBBE)
		add	hl,bc
		ld	(varBBBE),hl
		ld	hl,(varBBC0)
		sbc	hl,bc
		ld	(varBBC0),hl
		pop	hl
		ret

; Subroutine start read/write file
; Input:  ix = FIB
;         hl = pointer to drive table
StartRwFile:	push	hl
		ld	c,(ix+fab_file+0)
		ld	b,(ix+fab_file+1)
		ld	e,(ix+fab_file+2)
		ld	d,(ix+fab_file+3)
		push	de
		push	bc
		call	PosToSec
		jr	nz,_startrwfile3
		ld	(varBBBC),de
		ld	(varBBBB),a
		ld	(varBBB9),bc
		pop	hl
		ld	de,(varBBC0)
		dec	de
		add	hl,de
		ex	(sp),hl
		pop	bc
		ex	de,hl
		ex	(sp),hl
		push	hl
		ld	hl,$0000
		bit	4,(iy+68)
		jr	z,_startrwfile1
		dec	hl
_startrwfile1:	adc	hl,de
		ex	de,hl
		bit	7,d
		jr	nz,_startrwfile4
		pop	hl
		push	de
		push	bc
		call	PosToSec
		jr	nz,_startrwfile3
		push	de
		ld	c,(ix+fib_fsize+0)
		ld	b,(ix+fib_fsize+1)
		ld	e,(ix+fib_fsize+2)
		ld	d,(ix+fib_fsize+3)
		ld	a,b
		or	c
		dec	bc
		jr	nz,_startrwfile2
		or	d
		or	e
		dec	de
_startrwfile2:	push	de
		push	bc
		call	nz,PosToSec
		exx
		pop	hl
		pop	de
		exx
		pop	bc
_startrwfile3:	jr	nz,_rwfileret3
		exx
		pop	bc
		xor	a
		sbc	hl,bc
		ex	(sp),hl
		ex	de,hl
		sbc	hl,de
		exx
		bit	7,d
		ex	af,af'
		bit	0,(iy+68)
		jp	nz,_startrwfile15
		push	hl
		ld	h,b
		ld	l,c
		inc	de
		or	a
		sbc	hl,de
		ld	c,l
		ld	b,h
		inc	bc
		jr	nc,_startrwfile5
		add	hl,de
		ex	de,hl
		pop	hl
		bit	4,(iy+68)
		jr	z,_startrwfile8
		call	LogToPhysCl
		jr	nz,_rwfileret1
		push	de
		call	FatEntry

		; FAT16
		call	z,DelChain
		pop	de
		ld	bc,$ffff

		call	SetFatEntry
		ld	c,(ix+fab_startcl+0)
		ld	b,(ix+fab_startcl+1)
		scf
		ex	af,af'
		jr	_startrwfile7

_startrwfile4:	exx
		ex	af,af'
		pop	hl
		ld	e,(ix+fab_startcl+0)
		ld	d,(ix+fab_startcl+1)
		ld	a,d
		or	e
		call	nz,DelChain
		push	de
		ld	bc,$0000
		jr	_startrwfile7

_startrwfile5:	pop	hl
		ld	a,(iy+68)
		and	$02
		call	AllocClus
		jr	nz,_rwfileret2
		dec	de
		bit	7,d
		jr	nz,_startrwfile7
		push	bc
		call	LogToPhysCl
		pop	bc
		jr	nz,_rwfileret2
		call	SetFatEntry
		jr	_startrwfile8

_rwfileret3:	pop	de
_rwfileret2:	pop	de
_rwfileret1:	pop	de
		ret

_startrwfile7:	ld	(ix+fab_startcl+0),c
		ld	(ix+fab_startcl+1),b
		xor	a
		ld	(ix+fab_abscl+0),c
		ld	(ix+fab_abscl+1),b
		ld	(ix+fab_logcl+0),a
		ld	(ix+fab_logcl+1),a
_startrwfile8:	set	7,(ix+fab_access)
		exx
		ex	af,af'
		jr	nz,_startrwfile9
		jr	nc,_startrwfile11
_startrwfile9:	inc	bc
		ld	a,b
		or	c
		jr	nz,_startrwfile10
		inc	de
_startrwfile10:	ld	(ix+fib_fsize+0),c
		ld	(ix+fib_fsize+1),b
		ld	(ix+fib_fsize+2),e
		ld	(ix+fib_fsize+3),d
_startrwfile11:	pop	bc
		pop	de
		ld	a,b
		or	c
		dec	bc
		jr	nz,_startrwfile12
		dec	hl
_startrwfile12:	bit	7,h
		jr	z,_startrwfile13
		ld	a,h
		and	l
		inc	a
		ld	hl,$ffff
		jr	nz,_startrwfile14
		inc	hl
		sbc	hl,bc
		jr	nz,_startrwfile14
		dec	hl
		jr	_startrwfile14

_startrwfile13:	ld	hl,$0000
_startrwfile14:	ld	(varBBB2),hl
		exx
		xor	a
		ret

_startrwfile15:	ex	af,af'
		pop	de
		ex	(sp),hl
		jr	nz,_startrwfile16
		jr	nc,_startrwfile18
		exx
		ld	a,h
		and	l
		inc	a
		exx
		jr	nz,_startrwfile16
		add	hl,de
		jr	c,_startrwfile17
_startrwfile16:	ld	hl,$ffff
_startrwfile17:	inc	hl
		ld	(varBBC0),hl
_startrwfile18:	xor	a
		pop	hl
		ret

; Subroutine convert position in file to cluster, sector and offset
; Input:  DE,BC = position
;         HL    = pointer to drive table
; Output: DE    = relative cluster
;         A     = sector in cluster
;         HL    = offset in sector
PosToSec:	push	hl
		ld	a,b
		srl	d
		rr	e
		rra
		ld	b,$00
		rl	b
		push	bc
		ld	bc,10
		add	hl,bc
		push	af
		and	(hl)
		ld	c,a
		ld	a,(hl)
		inc	a
		ld	(varBBB8),a
		pop	af
		inc	hl
		ld	b,(hl)
		jr	_postosec2

_postosec1:	srl	d
		rr	e
		rra
_postosec2:	djnz	_postosec1
		inc	d
		dec	d
		jr	nz,_postosec3
		bit	7,e
		jr	nz,_postosec3
		ld	d,e
		ld	e,a
		ld	a,c
		db	$21
_postosec3:	ld	a,_FILE
		pop	bc
		pop	hl
		ret

; Subroutine translate logical cluster to physical cluster
LogToPhysCl:	push	hl
		ex	de,hl
		ld	e,(ix+fab_abscl+0)
		ld	d,(ix+fab_abscl+1)
		ld	a,d
		or	e
		jr	z,_logtophyscl3
		ld	c,(ix+fab_logcl+0)
		ld	b,(ix+fab_logcl+1)
		sbc	hl,bc
		jr	nc,_logtophyscl1
		add	hl,bc
		ld	e,(ix+fab_startcl+0)
		ld	d,(ix+fab_startcl+1)
_logtophyscl1:	ex	(sp),hl
		pop	bc
_logtophyscl2:	ld	a,b
		or	c
		ret	z
		dec	bc
		call	FatEntry
		jr	z,_logtophyscl2
		db	$3e
_logtophyscl3:	pop	hl
		ld	a,_FILE
		or	a
		ret

; Subroutine get FAT sector
dskFatSector:
		; FAT16 BUF_1F
		xor	a
		ld	(varDskex),a

		ld	b,1
		ld	a,(ix+14)
		dec	a
		jr	nz,_getsector1

; Subroutine get sector
; Input:  b  = flag (b0 reset do not real read, b1 reset ignore not recommended)
;         ix = pointer to drive table (dub)
;         de = sector number
GetSector:	ld	a,1
_getsector1:	ld	(varBBA2),a
		ld	c,(ix+dub_unit)
		ld	hl,(varBBF6)
		ld	a,h
		sub	$01
		call	nc,TestBufSec
		ret	z
		ld	hl,(varBBF8)
		push	hl
_getsector2:	call	TestBufSec
		jp	z,_getsector11
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		or	h
		jr	nz,_getsector2
		pop	hl
		inc	hl
		inc	hl
		bit	7,(hl)
		dec	hl
		ld	a,(hl)
		dec	hl
		jr	z,_getsector3
		ld	l,(hl)
		ld	h,a
_getsector3:	call	dskFlushBuf
		push	hl
		inc	hl
		inc	hl
		ld	(hl),c
		inc	hl
		ld	a,b
		and	$02
		ld	(hl),a
		inc	hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ld	a,(iy+34)
		ld	(hl),a
		inc	hl

		; FAT16 SECNUM
		; Write bit16-23 of sector number at buffer+8
		bit	7,(ix+dub_media)
		ld	a,0
		jr	nz,SECN_1	; FAT12
		ld	a,(varDskex)	; FAT16
SECN_1:		ld	c,(ix+dub_fatsec)
		ld	(hl),c
		inc	hl
		ld	(hl),a

		push	de
		inc	hl
		inc	hl
		inc	hl
		push	hl
		ld	e,l
		ld	d,h
		inc	de
		ld	(hl),$00
		push	bc
		ld	bc,512-1
		ldir
		pop	bc
		ex	(sp),ix
		pop	hl
		ld	(ix-2),l
		ld	(ix-1),h
		pop	de
		bit	0,b
		jr	z,_getsector10
_getsector4:	push	bc
		push	de
		ld	b,(iy+34)
		jr	_getsector6

_getsector5:	ld	a,e
		add	a,c
		ld	e,a
		jr	nc,_getsector6
		inc	d
_getsector6:	push	de
		push	bc
		ld	b,1
		ld	a,$00
		call	REDBUF			; FAT16
		pop	bc
		pop	de
		jr	z,_getsector9
		cp	$f1
		jr	z,_getsector7
		djnz	_getsector5
_getsector7:	pop	de
		pop	bc
		or	a
		bit	1,b
		jr	nz,_getsector8
		scf
_getsector8:	call	HandleErrARI
		jr	z,_getsector4
		db	$ca
_getsector9:	pop	de
		pop	bc
_getsector10:	ex	(sp),hl
		pop	ix
		db	$fe
_getsector11:	pop	bc
		set	1,(iy+41)
		ld	(varBBF6),hl
		call	BufEndChain
		xor	a
		ret

; Subroutine buffer contains sector of drive ?
TestBufSec:	push	hl
		inc	hl
		inc	hl
		ld	a,(hl)
		sub	c
		jr	nz,_testbufsec1
		inc	hl
		inc	hl
		ld	a,(hl)
		sub	e
		jr	nz,_testbufsec1

		; FAT16 CMPSEC
		; Compare sector number at buffer
		inc	hl
		ld	a,(hl)
		sub	d
		jr	nz,_testbufsec1		; Z=0 different sector
		bit	7,(ix+dub_media)
		jr	z,CMPS_1		; Z=1 FAT16
		xor	a
		jr	_testbufsec1
CMPS_1:		ld	a,(varDskex)
		inc	hl
		inc	hl
		inc	hl
		cp	(hl)			; bit16-23 of sector number

_testbufsec1:	pop	hl
		ret

; Subroutine mark buffer last read as changed
dskBufDirty:	ld	hl,(varBBF6)
		inc	hl
		inc	hl
		inc	hl
		set	7,(hl)
		ret

; Subroutine flush sector buffers of logical drive
dskFlushLog:	push	bc
		push	hl
		call	dskPhysDrive
		pop	hl
		pop	bc
		db	$fe			; opcode for 'cp n'

; Subroutine flush sector buffers of drive table
dskFlushDrives:	db	$f6			; opcode for 'or n'

; Subroutine flush sector buffers of physical drive
dskFlushPhys:	scf
		call	FlagLog
		push	hl
_flushphys1:	call	FindFlagBuf
		jr	z,_flagdrives5
		call	dskFlushBuf
		jr	_flushphys1

; Subroutine flag sector buffers of drive table unused
dskUnflag: 	db	$f6			; opcode for 'or n'

; Subroutine flag sector buffers of physical drive unused
UnflagPhys:	scf
		call	FlagLog
		push	hl
_unflphys1:	call	FindFlagBuf
		jr	z,_flagdrives5
		call	BufStart
		inc	hl
		inc	hl
		ld	(hl),$00
		jr	_unflphys1

; Subroutine flag buffers of drive
FlagLog:	jr	c,_flagdrives1

; Subroutine flag buffers of drive table
FlagDrives:	push	hl
		push	bc
		ld	bc,8
		add	hl,bc
		ld	a,(hl)
		pop	bc
		pop	hl
_flagdrives1:	push	hl
		push	de
		push	bc
		ld	b,a
		ld	hl,(varBBF8)
_flagdrives2:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		res	0,(hl)
		or	a
		jr	z,_flagdrives4
		cp	b
		jr	z,_flagdrives3
		rlca
		jr	c,_flagdrives4
		ld	a,b
		inc	a
		jr	nz,_flagdrives4
_flagdrives3:	set	0,(hl)
_flagdrives4:	ld	a,d
		or	e
		ex	de,hl
		jr	nz,_flagdrives2
		pop	bc
		pop	de
_flagdrives5:		pop	hl
		ret

; Subroutine find flagged sector buffer
FindFlagBuf:	ld	hl,(varBBF8)
_findflagbuf1:	push	hl
		inc	hl
		inc	hl
		inc	hl
		bit	0,(hl)
		res	0,(hl)
		pop	hl
		ret	nz
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		or	h
		jr	nz,_findflagbuf1
		ret

; Subroutine flush buffer, buffer at start of chain
FlushStart:	call	dskFlushBuf

; Subroutine buffer at start of chain
; Input:  HL = pointer to buffer
BufStart:	push	de
		ld	de,(varBBF8)
		or	a
		sbc	hl,de
		add	hl,de
		jr	z,_bufendchain2
		push	bc
		push	hl
		ld	(varBBF8),hl
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		push	bc
		ld	(hl),d
		dec	hl
		ld	(hl),e
		ex	de,hl
		ld	b,d
		ld	c,e
		call	BufSearch
		pop	bc
		jr	_bufendchain1

; Subroutine buffer at end of chain
BufEndChain:	ld	a,(hl)
		inc	hl
		or	(hl)
		dec	hl
		ret	z
		push	de
		push	bc
		push	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		xor	a
		ld	(hl),a
		dec	hl
		ld	(hl),a
		ld	b,h
		ld	c,l
		ld	hl,varBBF8
		call	BufSearch
		ex	de,hl
		pop	de
		ld	(hl),e
		inc	hl
		ld	(hl),d
		dec	hl
		call	BufSearch
_bufendchain1:	ex	de,hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		pop	hl
		pop	bc
_bufendchain2:	pop	de
		ret

; Subroutine search requested buffer
; Input:  HL = pointer to starting buffer
;         BC = pointer to requested buffer
; Output: HL = pointer to requested buffer
;         DE = pointer to previous buffer
BufSearch:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		dec	hl
		ex	de,hl
		ld	a,h
		or	l
		ret	z
		sbc	hl,bc
		add	hl,bc
		jr	nz,BufSearch
		or	a
		ret

; Subroutine flush buffer
dskFlushBuf:	set	1,(iy+41)
		push	hl
		inc	hl
		inc	hl
		ld	a,(hl)
		or	a
		jr	z,_flushbuf10
		inc	hl
		bit	7,(hl)
		jr	z,_flushbuf10
		res	7,(hl)
		push	de
		push	bc
		push	ix
_flushbuf1:	push	hl
		bit	6,(hl)
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl
		jr	z,_flushbuf4
		ld	a,e
_flushbuf2:	add	a,(hl)

		; FAT16 FSIZE1
		jr	nc,FSIZ_1
		inc	d
FSIZ_1:		inc	(hl)
		dec	(hl)
		jr	nz,_flushbuf3

		inc	d
_flushbuf3:	djnz	_flushbuf2
		ld	e,a
		ld	b,1
_flushbuf4:	push	de
		inc	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	de,hl
		push	de
		pop	ix
		ld	c,1
		pop	de
		push	de
_flushbuf5:	push	de
		push	bc
		ld	b,1
		ld	a,1
		call	WRTBUF			; FAT16
		pop	bc
		pop	de
		jr	nz,_flushbuf1C
		inc	c
_flushbuf1C:	cp	$f1
		jr	nz,_flushbuf6
		ld	bc,$0101
_flushbuf6:	push	af
		ld	a,e

		; FAT16 FSIZE2
		ld	a,(ix-4)
		or	a
		jr	nz,FSIZ_2
		inc	d
FSIZ_2:		add	a,e

		ld	e,a
		jr	nc,_flushbuf7
		inc	d
_flushbuf7:	pop	af
		djnz	_flushbuf5
		pop	de
		dec	c
		jr	nz,_flushbuf9
		bit	1,(ix-8)
		jr	nz,_flushbuf8
		scf
_flushbuf8:	call	HandleErrARI
_flushbuf9:	pop	hl
		jr	z,_flushbuf1
		pop	ix
		pop	bc
		pop	de
_flushbuf10:	pop	hl
		ret

; Subroutine get FAT entry content
; Input: de = cluster number
;	  hl = pointer to drive table
FatEntry:	; FAT16 FATRED
		push	af
		call	CHKDRV
		jr	z,Z0018			; use FAT16
		pop	af
		call	dskFat12Entry		; FAT12 routine
		bit	7,d
		ret

;Read 16 bit FAT
Z0018:		pop	af
		call	FATADR			; get address & sector set
		jr	z,Z0019			; no error
Z0020:		xor	a
		ld	(varBBEA),a
		call	ErrorFAT
		jr	z,Z0020
		jr	Z0021			; error

Z0019:		push	hl
		ld	a,(de)			; DE=FAT address
		ld	l,a
		inc	de
		ld	a,(de)
		ld	h,a
		ex	de,hl			; DE=next cluster number
		ld	hl,$fff7		; HL=wrong cluster number
		or	a
		sbc	hl,de
		pop	hl
		jr	nc,Z1021		; not end of cluster
Z0021:		ld	de,$ffff		; end of cluster
		or	d			; z=0
		scf				; cy=1
		ret
Z1021:		xor	a			; cy=0 z=1
		ret


dskFat12Entry:	call	FindCluster
		jr	z,_fatentry2
_fatentry1:	xor	a
		ld	(varBBEA),a
		call	ErrorFAT
		jr	z,_fatentry1
		jr	_fatentry4

_fatentry2:	push	hl
		ld	a,(de)
		ld	l,a
		inc	de
		ld	a,(de)
		jr	nc,_fatentry3
		ld	h,a
		call	Shift4Left
		ld	l,h
_fatentry3:	and	$0f
		ld	h,a
		ex	de,hl
		ld	hl,$0ff7
		sbc	hl,de
		pop	hl
		ret	nc
_fatentry4:	ld	de,$ffff
		ret

; Subroutine convert cluster number to sector number
; Input:  HL = pointer to drive table
;	   DE = cluster number
;	   A  = sector in cluster
ClusToSec:	push	hl
		push	bc
		ld	bc,11
		add	hl,bc
		ld	b,(hl)
		ex	de,hl
		dec	hl
		dec	hl

		; FAT16 GETSEC
		; Change cluster number to sector number
		; Input:  DE=cluster number
		; Output: DE=sector number bit0-15
		;  	  varBit16 = bit16-23
		; Change cluster number to sector number
		; Input:  DE=cluster number
		; Output: DE=sector number bit0-15
		;  	  varBit16 = bit16-23
		ld	c,a
		xor	a
		jr	Z0022
Z0023:		add	hl,hl		; bit 0-15 of sector number
		adc	a,a		; bit16-23 of sector number
Z0022:		djnz	Z0023
		ld	b,a		; bit16-23
		ld	a,c
		add	a,l
		ld	l,a
		ex	de,hl		; BDE=sector number
		ld	a,b
		ld	bc,$0009
		add	hl,bc		; DPB+14h start sector of data area
		ld	c,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,c
		add	hl,de
		jr	nc,Z0024
		inc	a
Z0024:		ld	(varBit16),a	; save bit16-23
		; FAT16 END

		ex	de,hl		; BDE=sector number
		pop	bc
		pop	hl
		ret

; Subroutine set FAT entry
; Input:  BC = cluster number
SetFatEntry:	push	de

		; FAT16 FATWRT
		call	CHKDRV
		jr	nz,_setfatentry1	; nz=12bit FAT
FATWR2:		call	FATADR
		jr	z,FATWR3
		ld	a,$0ff
		ld	(varBBEA),a
FATWR4:		call	ErrorFAT
		jr	z,FATWR4
		jp	_setfatentry8
FATWR3:		push	hl
		ld	a,c
		ld	(de),a		; FAT write
		inc	de
		ld	a,b
		ld	(de),a
		jr	_setfatentry6
		; FAT16 END

_setfatentry1:	ld	a,b
		cp	$10
		jr	c,_setfatentry2
		ld	bc,$0fff
_setfatentry2:	call	FindCluster
		jr	z,_setfatentry4
_setfatentry3:	ld	a,$ff
		ld	(varBBEA),a
		call	ErrorFAT
		jr	z,_setfatentry3
		jr	_setfatentry8

_setfatentry4:	push	hl
		jr	c,_setfatentry5
		ld	a,c
		ld	(de),a
		inc	de
		ld	a,(de)
		and	$f0
		or	b
		ld	(de),a
		jr	_setfatentry6

_setfatentry5:	ld	h,b
		ld	l,c
		call	Shift4Left
		ld	a,(de)
		and	$0f
		or	l
		ld	(de),a
		inc	de
		ld	a,h
		ld	(de),a
_setfatentry6:	call	dskBufDirty
		bit	0,(iy+41)
		jr	z,_setfatentry7
		ld	bc,8
		add	hl,bc
		ld	(hl),a
		dec	de
		ld	a,(de)
		ld	de,(varBBA7)
		ex	(sp),ix
		push	af
		call	dskFatSector
		call	dskBufDirty
		ld	bc,512+7
		add	hl,bc
		pop	af
		ld	(hl),a
		ex	(sp),ix
_setfatentry7:	pop	hl
_setfatentry8:	pop	de
		ret

; Subroutine find cluster
; Input:  de = cluster number
;	   hl = pointer to drive table (dub)
FindCluster:	push	ix
		push	bc
		push	hl
		push	hl
		pop	ix
		res	0,(iy+41)
		ld	l,(ix+dub_ncluster+0)
		ld	h,(ix+dub_ncluster+1)
		xor	a
		sbc	hl,de
		jr	c,_findclus2
		ld	h,d
		ld	l,e
		add	hl,hl
		add	hl,de
		srl	h
		rr	l
		push	af
		push	hl
		ld	e,h
		srl	e
		ld	d,a
		ld	l,(ix+dub_reserved+0)
		ld	h,(ix+dub_reserved+1)
		add	hl,de
		ex	de,hl
		call	dskFatSector
		ld	bc,11
		add	hl,bc
		pop	bc
		ld	a,b
		and	$01
		ld	b,a
		add	hl,bc
		add	a,c
		jr	nc,_findclus1
		ld	a,(hl)
		set	0,(iy+41)
		ld	(varBBA7),de
		ld	(varBBA5+0),a
		inc	de
		call	dskFatSector
		ld	bc,11
		add	hl,bc
		ld	a,(hl)
		ld	(varBBA5+1),a
		ld	hl,varBBA5
_findclus1:	ex	de,hl
		pop	af
		sbc	a,a
		cp	a
		rrca
_findclus2:	pop	hl
		pop	bc
		pop	ix
		ret

; Subroutine shift 4 bits left
Shift4Left:	xor	a
		add	hl,hl
		rla
		add	hl,hl
		rla
		add	hl,hl
		rla
		add	hl,hl
		rla
		ret

; Subroutine allocate clusters
; Input:  HL = drive table
;	  BC = number of clusters
;	  DE = previous cluster in chain (0FFFF if none)
;	  A <> 0 clears directory
; AL_CLUSTERS:
AllocClus:	ld	(varBBAA),a
		push	de
		push	bc
		ld	de,$ffff
		ld	(varBBA3),de
		push	de
		ld	de,1
		jr	_allocclus2

_allocclus1:	push	bc
		push	de
_allocclus2:	inc	de
		push	hl
		ld	bc,22
		add	hl,bc
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		sbc	hl,de
		pop	hl
		jr	c,_allocclus4
		push	de
		call	FatEntry
		ld	a,d
		or	e
		pop	de
		jr	nz,_allocclus2
		ld	b,d
		ld	c,e
		pop	de
		push	bc
		call	CHK_C			; FAT16 (CLST_5)
		jr	z,_allocclus3
		ld	(varBBA3),bc
_allocclus3:	call	z,SetFatEntry
		pop	de
		ld	a,(varBBAA)
		or	a
		call	nz,ClearDir
		pop	bc
		dec	bc
		ld	a,b
		or	c
		jr	nz,_allocclus1
		ld	bc,$ffff
		call	SetFatEntry
		ld	bc,(varBBA3)
		pop	de
		xor	a
		ret

_allocclus4:	pop	de
		pop	de
		ld	de,(varBBA3)
		call	CHK_C			; FAT16 (CLST_6)
		call	z,DelChain
		pop	de
		ld	a,_DKFUL
		or	a
		ret

; Subroutine clear directory (cluster)
; Input:  DE = cluster number
ClearDir:	push	de
		ld	a,d
		or	e
		jr	nz,_cleardir1
		push	hl
		ld	bc,18
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ld	h,d
		ld	l,e
		sbc	hl,bc

		; FAT16 BUF_5F
		xor	a
		ld	(varBit16),a

		ld	b,l
		dec	b
		dec	de
		pop	hl
		jr	_cleardir2

_cleardir1:	push	hl
		ld	bc,10
		add	hl,bc
		ld	b,(hl)
		pop	hl
		ld	a,b
		call	ClusToSec
_cleardir2:	ld	a,b
		inc	a
		pop	bc
		push	hl
		ex	(sp),ix
		push	bc
		ld	b,a
_cleardir3:	push	bc
		jr	nz,_cleardir4
		call	BufStart
_cleardir4:	ld	b,$00
		call	BUF_2F			; FAT16
		push	hl
		ld	bc,11
		add	hl,bc
		xor	a
_cleardir5:	ld	(hl),a
		inc	hl
		ld	(hl),a
		inc	hl
		djnz	_cleardir5
		call	dskBufDirty
		pop	hl
		pop	bc
		dec	de
		xor	a
		djnz	_cleardir3
		pop	de
		ex	(sp),ix
		pop	hl
		ret

; Subroutine delete chain
DelChain:	push	de
		call	FatEntry
		pop	bc
		push	de
		ld	d,b
		ld	e,c
		ld	bc,0
		call	SetFatEntry
		pop	de
		ld	a,d
		or	e
		ret	z
		call	CHK_C			; FAT16 (CLST_7)
		jr	z,DelChain
		ret

; Subroutine validate FIB / check disk change
; Input:  c  = drive
;	  b  = type (0=for disk, 1=for file, 2=flush dirty sector buffers)
;	  ix = pointer to FIB
; VAL_FIB:
dskValFIB:	ld	a,c
		call	dskPhysDrive		; logical to physical drive
		ld	(varBBEB),a		; save physical drive
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; de = pointer to drive table
		ld	a,d
		or	e
		ld	a,c
		ret	z			; z=invalid drive
		push	de
		dec	b			; validate for file?
		jr	nz,_valdrive8		; nz=no

		; validate for file
_valdrive1:	pop	hl			; pointer to drive table
		call	UpdCountAll		; update disk change tick counter for all drives
		push	ix
		push	hl
		ld	c,9
		add	hl,bc
		ld	a,(hl)			; a = disk change tick counter
		pop	hl
		or	a
		jr	nz,_valdrive2
		call	ReadBootSec		; read boot sector and make valid
		push	hl
		ld	c,1			; set flag to update drive table
		jr	_valdrive5

_valdrive2:	ld	c,a
		push	hl
		dec	c
		jr	z,_valdrive4		; z=skip check, treat as disk changed
		dec	c
		jr	nz,_valdrive13		; nz=skip check, within 0.5s of last disk operation of drive
		call	GetDiskChange		; get driver disk change status 
		jr	z,_valdrive13		; z=unchanged
_valdrive4:	call	ReadBootSec		; read boot sector and make valid
		ld	c,$00			; reset flag to update drive table
		
_valdrive5:	call	NoDskChange		; restart disk change tick counter
		pop	hl
		push	hl
		dec	c
		jr	nz,_valdrive13		; nz=don't update drive table
		call	UpdDrvTable		; update drive table and BPB
		call	dskUnflag		; mark sector buffers of drive table unused
		jr	_valdrive13		; done

		; validate for disk
_valdrive8:	pop	hl
		call	dskFlushDrives		; flush sector buffers of drive table
		call	UpdCountAll		; update disk change tick counter for all drives
		push	ix
		push	hl
		ld	de,9
		add	hl,de
		ld	a,(hl)			; a = disk change tick counter
		pop	hl
		; use DOS v2.31 change
		dec	b
		jr	z,_valdrive11		; z=flush dirty sector buffers 
		or	a
		jr	z,_valdrive9		; z=initialize, treat as disk changed
		dec	a
		jr	z,_valdrive9		; z=invalid, treat as disk changed
		dec	a
		jr	nz,_valdrive12		; nz=within 0.5s of last disk operation of drive
		call	GetDiskChange		; get driver disk change status
		jr	c,_valdrive9		; c=driver doesn't know, treat as disk changed
		jr	z,_valdrive12		; z=unchanged
_valdrive9:	call	ReadBootSec		; read boot sector and make valid
_valdrive10:	call	UpdDrvTable		; update drive table and BPB
		call	dskUnflag		; mark sector buffers of drive table unused
_valdrive11:	call	NoDskChange		; restart disk change tick counter
_valdrive12:	pop	de
		push	de
		push	hl
		
_valdrive13:	pop	hl
		pop	ix
		xor	a
		ret

; FAT16 read/write a sector for buffer
REDBUF:		push	af
		ld	a,(varDskex)
		jr	REDB_1
WRTBUF:		push	af
		ld	a,(ix-3)		; bit16-23
REDB_1:		ld	(varRW16),a		; use bit16-23 of sector number
		pop	af

; Subroutine execute disk driver read/write sectors (with disk check)
; Input:  a = function (0 = read sectors, 1 = write sectors)
DriverRwSec:	call	ValSameDisk
		call	Call16Driver		; FAT16

; Subroutine restart disk change tick counter / next 0.5 seconds no disk change
NoDskChange:	push	hl
		call	UpdCountAll
		ex	(sp),ix
		ld	(ix+dub_time),2+5
		ex	(sp),ix
		pop	hl
		or	a
		ret

; Subroutine validate if same disk
; VAL_SAME:
ValSameDisk:	call	UpdCountAll
		push	af
		push	de
		push	hl
		ld	de,9
		add	hl,de
		ld	a,2
		cp	(hl)			; disk change tick counter: init or invalid?
		pop	hl
		jr	c,_valsamedisk4		; c=yes, quit
		push	bc
		call	z,GetDiskChange		; z=get disk change status from driver
		jr	z,_valsamedisk3		; z=unchanged
		push	ix
_valsamedisk1:	call	ReadBootSec		; read boot sector and make valid
_valsamedisk2:	pop	ix
_valsamedisk3:	pop	bc
_valsamedisk4:	pop	de
		pop	af
		ret

; Subroutine get disk change status
; Output: Zx = set: unchanged, reset: changed
;         Cx = set: disk driver does not know
GetDiskChange:	push	bc
		push	de
		ld	a,2			; DSKCHG
		call	dskCallDriver
		jr	z,_diskchange1
		cp	_NRDY			; not ready?
		ld	de,$ffff
		call	z,HandleError
		ld	b,$ff
_diskchange1:	dec	b
		jr	z,_diskchange2
		inc	b
		jr	nz,_diskchange2
		ld	a,(DSK_CHK)
		inc	a
		scf
_diskchange2:	pop	de
		pop	bc
		ret

; Subroutine read bootsector and make valid
ReadBootSec:	push	bc
		push	de

		ld	ix,(SSECBUF)		; temporary boot sector buffer

_readbootsec1:	ld	de,0			; sector 0: boot sector
		ld	b,1			; read 1 sector
		ld	a,$00			; DSKIO read
		call	dskCallDriver
		jr	nz,_readbootsec4	; nz=error

		; copy first 48 bytes of boot sector to buffer
		push	hl
		ld	de,varB6D4
		ld	hl,(SSECBUF)
		ld	bc,$0030
		ldir
		pop	hl
		ld	ix,varB6D4

		call	ValBootSec
		; FAT16/ROM32K: don't update the BPB in the boot sector
		jr	nz,_readbootsec3
_readbootsec2:	pop	de
		pop	bc
		ret

_readbootsec3:	ld	a,_NDOS			; not a DOS disk
_readbootsec4:	ld	de,$ffff
		call	HandleError
		jr	_readbootsec1

; Subroutine validate bootsector and update DPB
; Input:  ix = pointer to bootsector
;         hl = pointer to DPB
ValBootSec:	push	ix
		ex	(sp),hl
		ld	de,11
		add	hl,de
		ld	d,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		sub	$02
		or	d
		jr	nz,_valbootsec1
		or	(hl)
		jr	z,_valbootsec1
		neg
		and	(hl)
		cp	(hl)
		inc	hl
		jr	nz,_valbootsec1
		inc	hl
		inc	hl
		ld	a,(hl)
		dec	a
		cp	$07
		jr	nc,_valbootsec1
		ld	de,6
		add	hl,de
		ld	a,(hl)

		; FAT16 GETDPB
		pop	hl
		xor	a
		ret

_valbootsec1:	pop	hl
		or	h
		ret


; Subroutine update drive table with bootsector BPB info
; Input:  hl = pointer to drive table (dub)
;         ix = pointer to bootsector
UpdDrvTable:	push	hl
		ld	bc,10
		add	hl,bc
		ld	a,(ix+13)	; get cluster size (in sectors)
		dec	a
		ld	(hl),a		; set cluster mask (0x0A)
		inc	hl
		ld	c,$00
_updrvtab1:		inc	c
		rrca
		jr	c,_updrvtab1
		ld	(hl),c		; set cluster shift (0x0B)
		inc	hl
		ld	e,(ix+14)	; get number of unused sectors
		ld	(hl),e		; set reserved sectors (0x0C)
		inc	hl
		ld	d,(ix+15)	; "
		ld	(hl),d		; "
		inc	hl
		push	de
		ld	b,(ix+16)	; get number of FAT's
		ld	(hl),b		; set number of FAT's (0x0E)
		inc	hl
		ld	e,(ix+17)	; get first directory sector
		ld	d,(ix+18)
		ld	a,e
		and	$0f
		ld	(hl),a		; set remainder of directory entries (no whole sector) (0x0F)
		ld	a,4
_updrvtab2:	srl	d
		rr	e
		dec	a
		jr	nz,_updrvtab2
		inc	hl
		ld	(hl),e		; set number of directory sectors (whole sectors) (0x10)
		inc	hl
		jr	nc,_updrvtab3
		inc	de
_updrvtab3:	ld	a,(ix+22)	; get size of FAT
		ld	(hl),a		; set number of sectors per FAT (0x11)
		inc	hl
		ex	(sp),hl
		push	de
		ld	e,a
		xor	a
		ld	d,a
_updrvtab4:	add	a,e
	
		; FAT16 FSIZE3
		jr	nc,FSIZ_3
		inc	d
FSIZ_3:		inc	e
		dec	e
		jr	nz,_updrvtab5

		inc	d
_updrvtab5:	djnz	_updrvtab4
		ld	e,a
		add	hl,de
		ex	de,hl
		pop	hl
		add	hl,de
		ex	(sp),hl
		ld	(hl),e		; set first sector of rootdirectory (0x12)
		inc	hl
		ld	(hl),d		; "
		inc	hl
		pop	de
		ld	(hl),e		; set first sector of data area (0x14)
		inc	hl
		ld	(hl),d		; "
		inc	hl
		push	hl
		ld	l,(ix+19)	; get total number of sectors
		ld	h,(ix+20)

		; FAT16 TALCLS
		; Total of cluster
		ld	a,l		; HL=BOOT +13h,14h
		or	h
		jr	z,WINFMT	; Format with Windows95
		sbc	hl,de
		jr	_updrvtab6	; 12bitFAT
WINFMT:		ld	l,(ix+$20)	; cluster size
		ld	h,(ix+$21)
		ld	a,(ix+$22)
		or	a
		sbc	hl,de
		sbc	a,0
WINFM_:		dec	c
		jr	z,_updrvtab7
		srl	a
		rr	h
		rr	l
		jr	WINFM_
		; FAT16 END

_updrvtab6:	dec	c
		jr	z,_updrvtab7
		srl	h
		rr	l
		jr	_updrvtab6
_updrvtab7:	inc	hl
		ex	de,hl
		pop	hl
		ld	(hl),e		; set number of clusters + 1 on disk (0x16)
		inc	hl
		ld	(hl),d

		; FAT16 DPBSET
		push	de		; total clusters + 1
		ld	bc,6		; skip dirty bit
		add	hl,bc
		ld	a,(ix+21)	; get Media ID
		ld	(hl),a		; set Media ID (0x1D)

		; FAT16 DPBSET
		pop	de
		push	hl
		ld	hl,4085		; if total clusters < 4085 then FAT12 else FAT16
		sbc	hl,de
		pop	hl
		jr	nc,DPB_1
		res	7,(hl)		; clear FAT12 flag in Media ID (0x1D)

DPB_1:		pop	hl		; HL=pointer to begin of drivetable
		push	hl
		inc	hl
		inc	hl
		ld	e,(hl)		; DE=pointer to DPB of drive
		inc	hl
		ld	d,(hl)		; "
		ex	de,hl
		inc	hl
		ld	(hl),a		; set MEDIA
		inc	hl
		ld	(hl),$00	; set SECSIZ
		inc	hl
		ld	(hl),$02	; "
		inc	hl
		ld	(hl),$0f	; set DIRMSK
		inc	hl
		ld	(hl),$04	; set DIRSHFT
		inc	hl
		ex	de,hl
		ld	bc,7
		add	hl,bc
		ld	bc,5
		ldir			; set CLUSMSK,CLUSSFT,FIRFAT,FATCNT
		ld	c,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		or	c
		ld	(de),a		; set MAXENT
		inc	de
		push	hl
		inc	hl
		inc	hl
		inc	hl
		ld	bc,4
		ldir			; set FIRREC,MAXCLUS
		pop	hl
		ld	bc,3		; set FATSIZ,FIRDIR
		ldir
		pop	hl

; Subroutine: invalidate current directory (zap cluster)
; Input:  HL = pointer to drive table
dskZapDir:	push	hl
		push	bc
		ld	bc,31
		add	hl,bc
		pop	bc
		ld	a,(hl)
		call	CHK_A		; FAT16 (CLST_A)
		jr	nz,_zapdir1
		ld	(hl),a
		dec	hl
		ld	(hl),a
_zapdir1:	pop	hl
		ret

; Subroutine execute disk driver function
; Input:  a  = function (see function table below)
;         b  = number of sectors
;         c  = segment (not used)
;         de = transfer address
;         hl = pointer to drive table
;         ix = parameter hl for driver
dskCallDriver:	cp	$03			; valid function number?
		jr	nc,_calldriver2		; nc=no
	
		; FAT16 DSKROM
		cp	$02			; DSKIO read or write ?
		jr	nc,Call16Driver		; nc=no
		ld	(iy-$02),$00		; varRW16
Call16Driver:	; FAT16 END

		push	iy
		push	hl
		push	hl
		ex	(sp),ix
		pop	hl
		push	hl
		exx
		push	hl
		push	de
		push	bc
		ex	af,af'
		push	af
		ld	a,(ix+8)
		dec	a
		ld	(TARGET),a
		ld	a,(ix+6)
		ex	af,af'
		ld	c,a			; driver function
		cp	$01
		ld	a,$ff
		jr	z,_calldriver1
		inc	a
_calldriver1:	ld	(varBBEA),a
		ld	hl,DrvFunTab
		ld	b,0
		add	hl,bc
		add	hl,bc
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ld	a,(P2_TPA)
		ex	af,af'
		call	JPHL
		ex	af,af'
		pop	af
		ex	af,af'
		exx
		pop	bc
		pop	de
		pop	hl
		exx
		pop	ix
		pop	hl
		pop	iy
		or	a
		ret
_calldriver2:	ld	a,_INTER		; internal error
		or	a
		ret

; Subroutine execute
JPHL:		jp	(hl)

; Driver function table
DrvFunTab:	dw	DrvRead			; 0 = DSKIO read
		dw	DrvWrite			; 1 = DSKIO write
		dw	DrvDiskChange			; 2 = DSKCHG

; Subroutine function DSKIO read
DrvRead:	or	a
		jr	_drvrw1

; Subroutine function DSKIO write
DrvWrite:	scf
_drvrw1:	ex	af,af'
		ld	hl,$4000
		exx
		ld	a,c

		; FAT16 SETNUM
		; Set to bit16-23 of sector number at registerC
		push	af
		ld	a,(ix+dub_media)
		bit	7,a
		jr	nz,MED_ID		; FAT12
		ld	a,(varRW16)
		ld	c,a			; FAT16 C=bit16-23
		pop	af
		jr	SETN1
MED_ID:		ld	c,a			; C=MEDIA ID
		pop	af

SETN1:		push	bc
		call	GoDriver
		jr	c,_drvrw2
		pop	bc
		xor	a
		ret

_drvrw2:	ex	af,af'
		pop	af
		sub	b
		ld	b,a
		ex	af,af'
		jr	_dskchg2

; Subroutine function DSKCHG
DrvDiskChange:	ex	af,af'
		ld	hl,$4003
		exx
		ld	b,$00
		ld	c,(ix+dub_media)
		ld	l,(ix+dub_dpb+0)
		ld	h,(ix+dub_dpb+1)
		call	GoDriver
		jr	c,_dskchg1
		xor	a
		ret
_dskchg1:	ld	b,$00

_dskchg2:	ld	hl,DrvErrTab
		rrca
		cp	(hl)
		jr	nc,_dskchg3
		inc	hl
		ld	e,a
		ld	d,0
		add	hl,de
		ld	a,(hl)
		ret
_dskchg3:	ld	A,_DISK
		ret

DrvErrTab:	db	13+1
		db	_WPROT
		db	_NRDY
		db	_DATA
		db	_SEEK
		db	_RNF
		db	_WRERR
		db	_DISK
		db	_DISK
		db	_DISK
		db	_NDOS
		db	_NCOMP
		db	_UFORM
		db	_NOUPB

; Subroutine execute disk driver function
GoDriver:	exx
		ld	e,(ix+dub_offset)	; driver routines jumptable offset to 0x4000
		ld	d,0
		add	hl,de			; set address of driver routine
		ld	b,(ix+dub_slot)		; driver slot
		push	bc
		pop	iy
		push	hl
		pop	ix
		ex	af,af'
		push	af
		ld	a,(MASTER)
		cp	b
		jr	z,go_master
		pop	af
		exx
		call	CALSLT
		ei
		ret

go_master:	pop	af
		exx
		jp	(ix)

; ---------------------------------------------------------

; Subroutine translate drive assigment and get pointer to drive tabel
; Input:  A = logical drive number (0=use current)
; Output: HL = pointer to entry in drive table
;	   A  = physical drive id
;	   C  = error number if z-flag set
dskPhysDrive:	push	de
		or	a			; use current drive?
		jr	nz,_physdrive1		; nz=no
		ld	a,(CUR_DRV)		; load current drive
_physdrive1:	ld	c,_IDRV			; set error to invalid drive
		cp	$09			; drive number <= max drives
		jr	c,_physdrive2		; c=yes
		xor	a			; set drive number to 0
_physdrive2:	ld	hl,varBA1A
		ld	d,$00
		ld	e,a
		add	hl,de
		ld	a,(hl)			; get physical drive
		ld	e,a
		ld	hl,varBA23
		add	hl,de
		add	hl,de			; set pointer in drive table
		pop	de
		or	a			; set flags
		ret

; Subroutine update tick counter of all drives
; UD_TICK:
UpdCountAll:	push	af
		push	hl
		ld	hl,(SSECBUF)
		dec	hl
		ld	a,(hl)
		inc	a
		jr	nz,_updcountall1

		; FAT flag invalid, verify physical drive
		ld	(hl),$00
		ld	a,(TARGET)
		push	de
		ld	e,a
		ld	d,0
		ld	hl,varBA23+2
		add	hl,de
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ld	hl,9
		add	hl,de
		pop	de
		ld	a,(hl)
		or	a
		jr	z,_updcountall1
		ld	(hl),$01

		; update tick counter of all drives
_updcountall1:	ld	hl,TIM_TI
		di
		ld	a,(hl)
		ei
		ld	(hl),$00
		or	a
		jr	z,_updcountall5		; z=no new ticks, quit
		push	bc
		push	de
		push	ix
		ld	c,a
		ld	hl,varBA23+2
		ld	b,8			; maximum drives
_updcountall2:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,d
		or	e
		jr	z,_updcountall4		; z=invalid drive, next
		push	de
		pop	ix
		ld	a,(ix+9)		; get timer
		SUB	$02
		jr	c,_updcountall4		; if timer <= 2 then no change
		sub	c			; decrement by number of ticks
		jr	nc,_updcountall3	; result should not be less then 0
		xor	a
_updcountall3:		add	a,2
		ld	(ix+9),a		; update timer
_updcountall4:	djnz	_updcountall2		; next drive
		pop	ix
		pop	de
		pop	bc
_updcountall5:	pop	hl
		pop	af
		ret

; Subroutine FAT error
ErrorFAT:	ld	a,_IFAT			; bad file allocation table
		ld	de,$ffff

; Subroutine handle error (ignore is valid)
HandleIgnErr:	or	a
		jr	HandleErrARI

; Subroutine handle error (ignore not recommended)
HandleError:	scf

; Subroutine handle error
; Input:  HL = pointer to drive table
; Output: A = result (0=retry,1=ignore), Zx set if retry
HandleErrARI:	push	bc
		push	de
		push	hl
		exx
		push	hl
		push	de
		push	bc
		exx
		ex	af,af'
		push	af
		ex	af,af'
		push	ix
		push	af
		ld	c,$00
		jr	nc,_errhandle1
		set	1,c
_errhandle1:	ld	a,(varBBEA)
		or	a
		jr	z,_errhandle2
		set	0,c
_errhandle2:	ld	a,d
		and	e
		inc	a
		jr	z,_errhandle3
		set	3,c
_errhandle3:	push	hl
		push	bc
		ld	bc,8
		add	hl,bc
		pop	bc
		ld	b,(hl)
		inc	hl
		ld	a,(hl)
		or	a
		jr	z,_errhandle4
		ld	(hl),$01
_errhandle4:	pop	hl
		pop	af
		cp	_STOP
		jr	z,_errhandle13
		cp	$f1
		jr	z,_errhandle10
		cp	$f2
		jr	nz,_errhandle5
		set	2,c
_errhandle5:	push	af
		ld	hl,ErrTab-1
_errhandle6:	inc	hl
		cp	(hl)
		inc	hl
		jr	z,_errhandle7
		bit	7,(hl)
		jr	z,_errhandle6
_errhandle7:	ld	a,(hl)
		add	a,a
		bit	0,c
		jr	z,_errhandle8
		inc	a
_errhandle8:	bit	2,c
		jr	z,_errhandle9
		set	7,a
_errhandle9:	ex	af,af'
		pop	af
		ld	h,(iy+9)
		push	hl
		ld	(iy+9),$00
		push	af
		push	iy
		ld	hl,(KDSK_V)
		call	sysTpaCall
		pop	iy
		pop	bc
		pop	hl
		ld	(iy+9),h
		dec	a
		cp	$03
		jr	c,_errhandle11
		xor	a
		jr	_errhandle11

_errhandle10:	ld	a,1
_errhandle11:	pop	ix
		ex	af,af'
		pop	af
		ex	af,af'
		exx
		pop	bc
		pop	de
		pop	hl
		exx
		pop	hl
		or	a
		jr	z,_errhandle12
		pop	de
		pop	bc
		dec	a
		ret

_errhandle12:	ld	a,_ABORT
_errhandle13:	push	af
		call	dskUnflag
		pop	af
		jp	sysTpaAbort

ErrTab:		db	_WPROT,0
		db	_NRDY,1
		db	_DATA,2
		db	_SEEK,3
		db	_RNF,4
		db	_WRERR,5
		db	_DISK,6
		db	_DISK,7
		db	_DISK,8
		db	_NDOS,9
		db	_NCOMP,10
		db	_UFORM,11
		db	_NOUPB,12
		db	0,6+128

; ------------------------------------------------------------------------------
; *** FAT16 subroutines ***
; ------------------------------------------------------------------------------

;Check cluster number
CHK_C:		ld	a,d
		and	e
CHK_A:		inc	a
		jr	z,chk_setflag
		xor	a
		ret
chk_setflag:	dec	a
		ret

; ---------------------------------------------------------
;Get sector number of FAT & read FAT sector in disk buffer
;Get address of FAT
FATADR:		push	ix			; ix=#B9DA
		push	bc
		push	hl			; HL=DPB address
		push	hl
		pop	ix
		res	0,(iy+$29)
		ld	l,(ix+dub_ncluster+0)
		ld	h,(ix+dub_ncluster+1)
		xor	a
		sbc	hl,de
		jr	c,FATAD_		; ERROR
		ld	h,d			; DE=cluster number
		ld	l,e
		add	hl,hl			; cluster number * 2(16bit)
		push	hl
		ld	e,d			; cluster * 2 / 200hbytes = sector
		ld	d,0
		ld	l,(ix+dub_reserved+0)	; sector number of FAT top
		ld	h,(ix+dub_reserved+1)
		add	hl,de			; get sector number of FAT
		ex	de,hl
		call	dskFatSector		; get address of disk buffer
		ld	bc,$0b
		add	hl,bc			; start address of FAT in disk buffer
		pop	bc
		ld	a,b
		and	$01			; get leave from FATaddress/200h
		ld	b,a
		add	hl,bc			; get address
		ex	de,hl
		cp	a			; z=1 right
FATAD_:		pop	hl
		pop	bc
		pop	ix
		ret

; ---------------------------------------------------------
; Output: zx=1: FAT16 drive, zx=0: FAT12 drive
CHKDRV:		push	hl
		push	de
		ld	de,$001d
		add	hl,de
		bit	7,(hl)
		pop	de
		pop	hl
		ret

; ---------------------------------------------------------
BUF_4F:		ld	bc,(varBBE8)		; SUB DIR? ROOT?
		inc	bc
		ld	a,b
		or	c
		ld	b,$01
		jr	z,BUF_3F		; (BBE8)=FFFF
		ld	a,(varSdir1)
		jr	BUF_F

BUF_2F:		ld	a,(varBit16)
		jr	BUF_F

BUF_3F:		xor	a
BUF_F:		ld	(varDskex),a		; FAT
		jp	GetSector

; ---------------------------------------------------------
STOR_7:		call	GetDirEntry		; note: moved this call here
		push	af
		ld	a,(varSdir1)
		ld	(ix+fib_sdir),a
		pop	af
		ex	de,hl
		ldir
		ret

