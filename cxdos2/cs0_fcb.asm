; ------------------------------------------------------------------------------
; cs0_fc.asm
; Disk functions (fcb).
; 
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS0_FCB

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_DSKRST		; 0D Disk reset
		PUBLIC	F_SELDSK		; 0E Select disk
		PUBLIC	F_FOPEN			; 0F Open file (fcb)
		PUBLIC	F_FCLOSE		; 10 Close file (fcb)
		PUBLIC	F_SFIRST		; 11 Search for first entry (fcb)
		PUBLIC	F_SNEXT			; 12 Search for next entry (fcb)
		PUBLIC	F_FDEL			; 13 Delete file (fcb)
		PUBLIC	F_RDSEQ			; 14 Sequential read (fcb)
		PUBLIC	F_WRSEQ			; 15 Sequential write (fcb)
		PUBLIC	F_FMAKE			; 16 Create file (fcb)
		PUBLIC	F_FREN			; 17 Rename file (fcb)
		PUBLIC	F_LOGIN			; 18 Get login vector
		PUBLIC	F_CURDRV		; 19 Get current drive
		PUBLIC	F_SETDTA		; 1A Set disk transfer address
		PUBLIC	F_ALLOC			; 1B Get allocation information
		PUBLIC	F_RDRND			; 21 Random read (fcb)
		PUBLIC	F_WRRND			; 22 Random write (fcb)
		PUBLIC	F_FSIZE			; 23 Get file size (fcb)
		PUBLIC	F_SETRND		; 24 Set random record (fcb)
		PUBLIC	F_WRBLK			; 26 Random block write (fcb)
		PUBLIC	F_RDBLK			; 27 Random block read (fcb)
		PUBLIC	F_WRZER			; 28 Random write with zero fill (fcb)
		PUBLIC	F_VERIFY		; 2E Set/reset verify flag
		PUBLIC	F_RDABS			; 2F Absolute sector read
		PUBLIC	F_WRABS			; 30 Absolute sector write

		EXTERN	sysFarDos
		EXTERN	F_GENV			; env

		; dsk
		EXTERN	F_FLUSH
		EXTERN	dskParsePath
		EXTERN	dskNameZ
		EXTERN	dskUpdateFIB
		EXTERN	dskUseFIB
		EXTERN	dskFindFirst
		EXTERN	dskFindNext
		EXTERN	dskEnsureFIB
		EXTERN	dskSetupFIB
		EXTERN	dskDeleteFIB
		EXTERN	dskRenameFIB
		EXTERN	dskRwSectors
		EXTERN	dskWriteFIB
		EXTERN	dskReadFIB
		EXTERN	dskRwFIB
		EXTERN	dskFatSector
		EXTERN	dskFlushLog
		EXTERN	dskFlushDrives
		EXTERN	dskUnflag
		EXTERN	Drives
		EXTERN	dskFat12Entry
		EXTERN	dskValFIB
		EXTERN	dskPhysDrive
		EXTERN	CHKDRV
		EXTERN	STOR_7

; ---------------------------------------------------------
; Function $0D DSKRST
; Disk reset
;
; Function $1A _SETDTA
; Input:  de = disk transfer address
; ---------------------------------------------------------
F_DSKRST:	ld	b,$ff
		ld	d,$00
		call	F_FLUSH			; flush buffers
		ld	a,1
		ld	(CUR_DRV),a		; set current drive to default A:
		ld	de,DBUF			; default DTA address
F_SETDTA:	ld	(DTA_AD),de		; set DTA address
		jr	ret_ok

; ---------------------------------------------------------
; Function $0E SELDSK
; Select disk
; Input:  e   = drive number (0=A, 1=B, etc.)
; Output: l,a = number of drives available (excluding ramdisk)
; ---------------------------------------------------------
F_SELDSK:	inc	e			; adjust drive number offset for DOS
		ld	a,e
		ld	c,_IDRV			; set error to invalid drive
		call	nz,dskPhysDrive		; translate logical to physical drive
		jr	z,_seldsk1		; z=no valid drive
		ld	a,(hl)
		inc	hl
		or	(hl)			; valid drive?
		jr	z,_seldsk1		; z=no
		ld	a,e
		ld	(CUR_DRV),a		; update current drive
		ld	c,$00			; no error
_seldsk1:	ld	hl,(SNUMDR)
		ld	h,0
		ld	a,c
		ret

; ---------------------------------------------------------
; Function $2E VERIFY
; Set/reset verify flag
; Input:  e = verify flag
; ---------------------------------------------------------
F_VERIFY:	ld      a,e
		ld	(RAWFLG),a
		jr	ret_ok

; ---------------------------------------------------------
; Function $0F FOPEN
; Open file (fcb)
; ---------------------------------------------------------
F_FOPEN:	ld	(iy+47),$04
		ld	a,2
		ld	c,1
fopen1:		call	SearchFirst
		jr	nz,ret_error
		xor	a
		push	de
		call	dskSetupFIB
		pop	de
		ld	hl,(varBB96)
		push	hl
		ld	a,(varBB98)
		or	a
		jr	z,_fopen2
		ld	a,(varB9DA+25)
		ld	(hl),a
_fopen2:	call	FnameToFCB
		inc	hl
		ld	a,(de)
		ld	(hl),a
		inc	hl
		ld	(hl),$00
		pop	ix
		call	UpdateFCB
		xor	a
		call	SetExtRec
		call	UpdRecCount
		ld	a,(varB9DA+25)
		call	dskFlushLog

ret_ok:		xor	a
		ld	h,a
		ld	l,a
		ret

ret_error:	ld	hl,$00ff
		ret

; ---------------------------------------------------------
; Function $10 FCLOSE
; Close file (fcb)
; Input:  de = pointer to FCB
; ---------------------------------------------------------
F_FCLOSE:	call	GetFIB
		jr	nz,ret_error
		ld	de,(varBB96)
		call	GetFname
		call	dskEnsureFIB
		or	a
		jr	nz,ret_error
		ld	a,(ix+25)
		call	dskFlushLog
		jr	ret_ok

; ---------------------------------------------------------
; Function $11 SFIRST
; Search for first entry (fcb)
; ---------------------------------------------------------
F_SFIRST:	ld	hl,12
		add	hl,de
		ld	a,(hl)
		ld	(varBB95),a
		ld	(iy+47),$04
		ld	a,(de)
		add	a,a
		sbc	a,a
		and	$10
		ld	c,$00
		call	SearchFirst
		jr	nz,ret_error
		jr	snext1

; ---------------------------------------------------------
; Function $12 SNEXT
; Search for next entry (fcb)
; ---------------------------------------------------------
F_SNEXT:	ld	de,varB9DA
		ld	hl,varB99A
		ld	bc,64
		ld	a,(hl)
		cp	$ff
		ld	a,_NOFIL
		jr	nz,ret_error
		ldir
		call	SearchNext
		jr	nz,ret_error
snext1:		push	de
		ld	hl,varB9DA
		ld	de,varB99A
		ld	bc,64
		ldir
		pop	de
		ld	hl,varB975
		ld	a,(varB9DA+25)
		ld	(hl),a
		call	FnameToFCB
		ld	a,(varBB95)
		ld	(hl),a
		inc	hl
		ld	a,(de)
		ld	(hl),a
		inc	hl
		inc	de
		inc	de
		ld	(hl),$00
		inc	hl
		inc	de
		ex	de,hl
		ld	bc,18
		ldir
		ld	ix,varB975
		xor	a
		call	SetExtRec
		call	UpdRecCount
		jr	c,F_SNEXT
		ld	hl,varB975
		ld	de,(DTA_AD)
		ld	bc,33
		call	XFER
		jp      ret_ok

; ---------------------------------------------------------
; Function $14 RDSEQ
; Sequential read (fcb)
; Input:  de = pointer to FCB
; ---------------------------------------------------------
F_RDSEQ:	call	GetFIB
		jr	nz,_rdseq4
		ld	ix,(varBB96)
		call	SetRndRec
		ld	a,(varB9DA+fib_mode)
		bit	7,a			; device?
		jr	z,_rdseq1		; z=no
		ld	bc,128
		ld	de,(DTA_AD)
		ld	ix,varB9DA
		xor	a			; segment type TPA
		call	dskReadFIB
		jr	nz,_rdseq4
		ld	ix,(varBB96)
		jr	_rdseq3

_rdseq1:	call	BufRecord
		jr	z,_rdseq2
		call	BufRead
		jr	nz,_rdseq4
		ld	hl,varB2D4
_rdseq2:	call	ReadFromBuf
_rdseq3:	call	IncRecord
		call	UpdateFCB
		jp	ret_ok
_rdseq4:	ld	hl,1
		ret

; ---------------------------------------------------------
; Function $15 WRSEQ
; Sequential write (fcb)
; Input:  de = pointer to FCB
; ---------------------------------------------------------
F_WRSEQ:	call	GetFIB
		jr	nz,_rdseq4
		ld	ix,(varBB96)
		call	SetRndRec
		call	BufPurge
		ld	ix,varB9DA
		ld	bc,128
		ld	de,(DTA_AD)
		xor	a
		call	dskWriteFIB
		jr	nz,_rdseq4
		ld	ix,(varBB96)
		call	IncRecord
		call	UpdateFCB
		ld	a,(ix+32)
		ld	hl,(varBB96)
		ld	bc,15
		add	hl,bc
		cp	(hl)
		jr	c,_wrseq1
		ld	(hl),a
_wrseq1:	jp	ret_ok

; ---------------------------------------------------------
; Function $16 FMAKE
; Create file (fcb)
; ---------------------------------------------------------
F_FMAKE:	ld	hl,12
		add	hl,de
		ld	a,(hl)
		or	a
		jr	z,_fmake1
		call	F_FOPEN
		or	a
		jp	z,ret_ok
_fmake1:	ld	(iy+47),$00
		xor	a
		ld	c,a
		jp	fopen1

; ---------------------------------------------------------
; Function $13 FDEL
; Delete file (fcb)
; ---------------------------------------------------------
F_FDEL:		ld	bc,$00ff
		ld	(iy+$10),c
		push	bc
		ld	(iy+47),$04
		xor	a
		ld	c,a
		call	SearchFirst
		jr	 _fdel3
_fdel1:		pop	bc
		ld	c,$00
		push	bc
_fdel2:		call	SearchNext
_fdel3:		jr	nz,DelRenEnd
		bit	0,(ix+14)
		jr	nz,_fdel2
		ld	a,1
		call	dskDeleteFIB
		or	a
		jr	z,_fdel1
		jr	DelRenEnd

; ---------------------------------------------------------
; Function $17 FREN
; Rename file (fcb)
; ---------------------------------------------------------
F_FREN:		ld	bc,$00ff
		push	bc
		push	de
		ld	ix,varB9DA
		ld	(ix+31),$00
		ld	hl,17
		add     hl,de
		ld	de,varB975
		ex	de,hl
		ld	a,(de)
		call	dskNameZ
		pop	de
		xor	a
		ld	c,a
		call	SearchFirst
		jr	_fren2
_fren1:		pop	bc
		ld	c,$00
		push	bc
		call	SearchNext
_fren2:		jr	nz,DelRenEnd
		ld	bc,varB975
		call	dskRenameFIB
		or	a
		jr	z,_fren1

DelRenEnd:	ld	b,a
		ld	a,(varB9DA+25)
		call	dskFlushLog
		pop	hl
		ld	a,l
		or	a
		ret	z
		ld	a,b
		ret

; ---------------------------------------------------------
; Function $18 LOGIN
; Get login vector
; Output: hl = login vector
; ---------------------------------------------------------
F_LOGIN:	ld	b,$08			; maximum number of drives
		ld	hl,$0000		; clear login vector
_login1:	add	hl,hl			; shift bits to the left
		push	hl
		ld	a,b			; drive number
		call	dskPhysDrive
		ld	a,(hl)
		inc	hl
		or	(hl)			; valid drive?
		pop	hl
		jr	z,_login2		; z=no
		inc	hl			; set drive bit
_login2:	djnz	_login1			; next drive
		xor	a			; no error
		ret

; ---------------------------------------------------------
; Function $19 CURDRV
; Get current drive
; Output: l = current drive
; ---------------------------------------------------------
F_CURDRV:	ld	hl,(CUR_DRV)
		dec	l
		xor	a
		ld	h,a
		ret

; ---------------------------------------------------------
; Function $1B ALLOC
; Get allocation information
; Input:  e  = drive number (0=current, 1=A: etc)
; Output: a  = sectors per cluster
;         bc = sector size (always 512)
;         de = total clusters on disk
;         hl = free clusters on disk
;         ix = pointer to DPB
;         iy = pointer to first FAT sector
; ---------------------------------------------------------
F_ALLOC:	ld	c,e
		ld	b,$00
		ld	ix,varB9DA
		call	dskValFIB		; validate drive
		or	a
		ld	c,$ff
		ret	nz
		push	hl
		pop	ix
		ld	e,(ix+dub_ncluster+0)	; de = total clusters on disk (max cluster number+1)
		ld	d,(ix+dub_ncluster+1)
		push	de

		; FAT16
		call	CHKDRV
		jp	z,ALLOC16

		ld	de,2			; start with cluster 2
		ld	b,d			; bc = 0: free cluster count
		ld	c,d
_alloc1:	push	de
		call	dskFat12Entry
		ld	a,d
		or	e
		jr	nz,_alloc2
		inc	bc			; increase free cluster counter
_alloc2:	pop	de
		ex	(sp),hl
		sbc	hl,de			; cluster number = max cluster number?
		add	hl,de
		ex	(sp),hl
		inc	de			; next cluster number
		jr	nz,_alloc1		; loop until end of fat
		push	bc
ALLOC_RET:	ld	e,(ix+dub_reserved+0)
		ld	d,(ix+dub_reserved+1)
		call	dskFatSector
		ld	de,11
		add	hl,de
		ld	de,(SSECBUF)
		push    de
		ld      bc,512
		ldir
		pop     iy
		pop     hl
		pop     de
		dec     de
		ld      c,(ix+dub_dpb+0)
		ld      b,(ix+dub_dpb+1)
		push    bc
		ld      c,(ix+dub_clmask)
		inc     c
		pop     ix
		xor     a
		ret

; ---------------------------------------------------------
; Function $21 RDRND
; Random read (fcb)
; ---------------------------------------------------------
F_RDRND:	ld	a,1
		call	rwRandomRec
		jr	nz,rnd_error
		ld	a,c
		neg
		and	$7f
		ld	c,a
		ld	a,$00
		call	nz,ClearRecPart
		jr	rnd_ret

; ---------------------------------------------------------
; Function $22 WRRND
; Random write (fcb)
; ---------------------------------------------------------
F_WRRND:	xor	a
		db	$21		; opcode 'ld hl,nn'

; ---------------------------------------------------------
; Function $28 WRZER
; Random write with zero fill (fcb)
; ---------------------------------------------------------
F_WRZER:	ld	a,2
		call	rwRandomRec
		jr	nz,rnd_error
rnd_ret:	ld	ix,(varBB96)
		call	UpdRecCount
		call	UpdateFCB
		jp	ret_ok

rnd_error:	ld	hl,1
		ret

; ---------------------------------------------------------
; Function $23 FSIZE
; Get file size (fcb)
; ---------------------------------------------------------
F_FSIZE:	ld      (iy+47),$04
		ld      a,2
		ld	c,$00
		call	SearchFirst
		jp	nz,ret_error
		ld	c,(ix+fib_fsize+0)
		ld	b,(ix+fib_fsize+1)
		ld	e,(ix+fib_fsize+2)
		ld	d,(ix+fib_fsize+3)
		xor	a
		ld	h,a
		sub	c
		and	$7f
		ld	l,a
		add	hl,bc
		jr	nc,_fsize1
		inc	de
_fsize1:	add	hl,hl
		ld	a,h
		ex	de,hl
		adc	hl,hl
		ld	ix,(varBB96)
		ld	(ix+fcb_random+0),a
		ld	(ix+fcb_random+1),l
		ld	(ix+fcb_random+2),h
		jp	ret_ok

; ---------------------------------------------------------
; Function $24 SETRND
; Set random record (fcb)
; ---------------------------------------------------------
F_SETRND:	push	de
		pop	ix
		call	SetRndRec
		jp	ret_ok

; ---------------------------------------------------------
; Function $27 RDBLK
; Random block read (fcb)
; Input:  de = pointer to FCB
;         hl = number of records
; ---------------------------------------------------------
F_RDBLK:	ld	a,1
		jr      rwBlock

; ---------------------------------------------------------
; Function $26 WRBLK
; Random block write (fcb)
; Input:  DE = pointer to FCB
;         HL = number of records
; ---------------------------------------------------------
F_WRBLK:	xor	a
rwBlock:	ex	af,af'
		ld	a,_OV64K
		ld	(varB976),a
		push	hl
		call	GetFIB
		jp	nz,_rwblock6
		ld	ix,(varBB96)
		call	BufPurge
		ld	c,(ix+14)
		ld	b,(ix+15)
		ld	hl,64-1
		xor	a
		sbc	hl,bc
		ld	e,(ix+35)
		ld	d,(ix+36)
		jr	nc,_rwblock1
		ld	d,a
_rwblock1:	call	Multiply
		ld	a,h
		or	l
		jp	nz,_rwblock7
		push	de
		ld	e,(ix+33)
		ld	d,(ix+34)
		call	Multiply
		pop	bc
		add	hl,bc
		jp	c,_rwblock7
		ld	(varB9DA+45),de
		ld	(varB9DA+47),hl
		pop	de
		push	de
		ld	c,(ix+14)
		ld	b,(ix+15)
		call	Multiply
		ld	a,h
		or	l
		jp	nz,_rwblock7
		ld	c,e
		ld	b,d
		push	bc
		ex	af,af'
		push	af
		ld	ix,varB9DA
		ld	de,(DTA_AD)
		bit	0,a
		jr	nz,_rwblock2
		ld	a,b
		or	c
		ld	a,$00
		jr	nz,_rwblock2
		set	4,a
_rwblock2:	call	dskRwFIB
		ld	(varB976),a
		ld	ix,(varBB96)
		pop	af
		ex	af,af'
		xor	a
		pop	hl
		sbc	hl,bc
		jr	z,_rwblock4
		ld	(varB977),de
		ld	e,(ix+14)
		ld	d,(ix+15)
		CALL	Divide
		ld	a,h
		or	l
		push	bc
		jr	z,_rwblock3
		pop	bc
		inc	bc
		push	bc
		ex	de,hl
		sbc	hl,de
		ld	b,h
		ld	c,l
		ld	de,(varB977)
		ex	af,af'
		bit	0,a
		call	nz,ClearRecPart
_rwblock3:	pop	bc
		pop	hl
		push	bc
		xor	a
		sbc	hl,bc
		jr	z,_rwblock4
		inc	a
_rwblock4:	ex	af,af'
		call	UpdateFCB
		pop	de
		ld	l,(ix+33)
		ld	h,(ix+34)
		add	hl,de
		ld	(ix+33),l
		ld	(ix+34),h
		jr	nc,_rwblock5
		inc	(ix+35)
		jr	nz,_rwblock5
		inc	(ix+36)
_rwblock5:	ex	af,af'
		jr	_rwblock8
_rwblock6:	ld	(varB976),a
_rwblock7:	pop	hl
		xor	a
		ld	d,a
		ld	e,a
		inc	a
_rwblock8:	ld	l,a
		ld	h,$00
		or	a
		ret	z
		ld	a,(varB976)
		ret

; Subroutine divide
Divide:		xor	a
		ld	h,a
		ld	l,a
		ld	a,$10
_divide1:	ccf
_divide2:	rl	c
		rl	b
		dec	a
		ret	m
		adc	hl,hl
		sbc	hl,de
		jr	nc,_divide1
		add	hl,de
		or	a
		jr	_divide2

; Subroutine multiply
Multiply:	push	bc
		ld	a,b
		ld	hl,0
		ld	b,$10
_multiply1:	add	hl,hl
		rl	c
		rla
		jr	nc,_multiply2
		add	hl,de
		jr	nc,_multiply2
		inc	c
		jr	nz,_multiply2
		inc	a
_multiply2:	djnz	_multiply1
		ex	de,hl
		ld	l,c
		ld	h,a
		pop	bc
		ret

; ---------------------------------------------------------
; Function $2F RDABS
; Absolute sector read
; ---------------------------------------------------------
F_RDABS:	ld	a,1
		jr	absrw1

; ---------------------------------------------------------
; Function $30 WRABS
; Absolute sector write
; ---------------------------------------------------------
F_WRABS:	xor	a
absrw1:		ld	(varBBC4),a
		ld	(varBBB4),de
		ld	bc,(DTA_AD)
		ld	(varBBC2),bc
		ld	a,b
		add	a,h
		jr	c,absrw2
		add	a,h
absrw2:		ld	a,_OV64K
		ret	c
		push	hl
		ld	ix,varB9DA
		ld	b,2
		ld	c,l
		inc	c
		call	dskValFIB
		pop	bc
		or	a
		ret	nz
		call	dskFlushDrives
		call	dskUnflag

		; FAT16 ABSSEC
		; SECTOR READ/WRITE
		xor	a
		ld	(varBit16),a

		call	dskRwSectors
		call	dskFlushDrives
		call	dskUnflag
		ld	de,9
		add	hl,de
		ld	(hl),$01
		xor	a
		ret

; ---------------------------------------------------------
; *** Subroutines: fcb ***
; ---------------------------------------------------------

; Subroutine rebuild FIB from FCB
; Input:  de = pointer to FCB
; Output: ix = pointer to FIB
GetFIB:		ex	de,hl
		ld	(varBB96),hl
		ld	ix,varB9DA
		ld	(ix+0),$ff
		ld	a,(hl)
		and	$0f
		ld	(ix+25),a
		ld	de,varB9DA+21
		ld	bc,16
		add     hl,bc
		ld      c,4
		ldir
		ld      de,varB9DA+26
		ld      c,4
		ldir
		ld      de,varB9DA+37
		ld      c,8
		ldir

		; FAT16 CLUST
		ld	hl,(varB9DA+32)
		call	CHKDRV
		ret	z			; z=FAT16

		ld	a,(ix+42)
		ld	b,$00
		bit	6,a
		jr	z,_getfib1
		ld	b,$01
_getfib1:	ld	(ix+14),b
		ld	b,$00
		bit	5,a
		jr	z,_getfib2
		ld	b,$a4
_getfib2:	bit	4,a
		jr	z,_getfib3
		set	6,b
_getfib3:	ld	(ix+30),b
		ld	b,$00
		bit	7,a
		jr	z,_getfib4
		ld	b,$80
_getfib4:	ld	(ix+49),b
		and	$0f
		ld	(ix+42),a
		xor	a
		bit	7,(ix+30)
		ret	z
		ld	l,(ix+26)
		ld	h,(ix+27)
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ld	l,(ix+28)
		ld	h,(ix+29)
		sbc	hl,de
		ret	z
		ld	a,$b7
		ret

; Subroutine update FIB with file name from FCB
GetFname:	ld	ix,varB9DA
		ld	hl,varB9DA+1
		inc	de
		ld	a,(de)
		ld	(ix+31),$02
		jp	dskNameZ

; Subroutine transfer record from sequential read buffer
ReadFromBuf:	ld      de,(DTA_AD)
		ld      bc,128
		call	XFER
		ret

; Subroutine get directory entry and setup FIB
; Input:  DE = pointer to FCB
SearchFirst:	ld	(iy+$18),$00
		ld	b,a
		push	bc
		ld	(varBB96),de
		ld	ix,varB9DA
		ld	(ix+31),a
		push	af
		push	de
		inc	de
		ld	hl,varB8F4
		ld	a,(de)
		call	dskNameZ
		pop	de
		pop	af
		ld	b,a
		ld	a,(de)
		and	$0f
		ld	de,varB8F4
searchfirst1:	ld	c,$08
		call	dskParsePath
		jr	nz,_searchfirst3
		or	c
		ld	a,_IPATH
		jr	nz,_searchfirst3
		push	hl
		ld	hl,varB926
		ld	de,varB9DA+32
		ld	bc,11
		push	de
		ldir
		pop	de
		pop	hl
		call	dskFindFirst
searchfirst2:	or	a
		jr	nz,_searchfirst3
		push	de
		push	hl
		call	dskUpdateFIB
		call	STOR_7
		pop	hl
		pop	de
		pop	bc
		xor	a
		ret

_searchfirst3:	pop	bc
		bit	0,c
		jr	nz,_searchfirst4
		ld	(iy+$18),$00
		or	a
		ret

_searchfirst4:	ld	(varB2D4+300),a
		push	bc
		ld	hl,_append
		ld	de,varB2D4
		ld	b,$ff
		ld	(iy+$10),b
		push	ix
		ld	ix,F_GENV		; Environment functions are in code segment 1
		call	sysFarDos
		pop	ix
		or	a
		pop	bc
		ret	nz
		push	bc
		ld	c,4
		ld	de,varB2D4
		ld	ix,varB9DA
		xor	a
		call	dskParsePath
		pop	de
		ret	nz
		ld	a,b
		and	$05
		jr      z,_searchfirst7
		ld	(iy+$18),$ff
		push	de
		ld	hl,(varBB9E)
		ld	a,b
		and	$18
		jr	z,_searchfirst5
		ld	(hl),'\\'
		inc	hl
_searchfirst5:	ld	de,varB8F4
_searchfirst6:	ld	a,(de)
		ld	(hl),a
		inc	hl
		inc	de
		or	a
		jr	nz,_searchfirst6
		pop	bc
		xor	a
		ld	c,$00
		push	bc
		ld	de,varB2D4
		jp	searchfirst1

_searchfirst7:	ld	a,(varB2D4+300)
		or	a
		ret

_append:	db	"APPEND",0

; Subroutine try to get next directory entry
SearchNext:	ld	ix,varB9DA
		ld	(iy+47),$04
		call	dskUseFIB
		ret	nz
		ld	de,varB9DA+32
		call	dskFindNext
		ld	c,$00
		push	bc
		jp	searchfirst2

; Subroutine do random record operation
; Input:  a  = operation code (0:write_zero 1:read 2:write)
;         de = pointer to FCB
rwRandomRec:	ex	af,af'
		call	GetFIB
		ret	nz
		ld	ix,(varBB96)
		ld	a,(ix+33)
		ld	c,(ix+34)
		ld	b,(ix+35)
		push	af
		push	bc
		call	UpdFilePtr
		call	BufPurge
		pop	hl
		pop	af
		add	a,a
		adc	hl,hl
		ld	(ix+12),l
		ld	(ix+14),h
		srl	a
		ld	(ix+32),a
		xor	a
		ex	af,af'
		ld	bc,128
		ld	ix,varB9DA
		ld	de,(DTA_AD)
		jp	dskRwFIB

; Subroutine get pointer to record if it is in the sequential read buffer
; Output: hl = pointer to record in buffer
;         zx = set if record is in the buffer
BufRecord:	call	CheckBufFile
		ret	nz
		ld	b,(iy+$12)
		ld	de,(varBB92+1)
		ld	a,(ix+33)
		ld	l,(ix+34)
		ld	h,(ix+35)
		sub	b
		sbc	hl,de
		ret	nz
		ld	b,a
		ld	a,(varBB91)
		sub	$01
		ret	c
		cp	b
		ret	c
		xor	a
		srl	b
		rra
		ld	c,a
		ld	hl,varB2D4
		add	hl,bc
		xor	a
		ret

; Subroutine if random record is in sequential read buffer then invalidate sequential read buffer
BufPurge:	call	CheckBufFile
		ret	nz
		ld	(iy+$10),$ff
		ret

; Subroutine check sequential read buffer has drive and startcluster of file
CheckBufFile:	LD      A,(IX+0)
		LD      B,A
		LD      A,(varBB90)
		CP      B
		RET     NZ
		LD      L,(IX+26)
		LD      H,(IX+27)
		LD      DE,(varBB8E)
		SBC     HL,DE
		RET

; Subroutine fill sequential read buffer
; Input:  ix = pointer to FCB
; Output: a  = 0:read ok  1:nothin was read (eof)
BufRead:	ld	(iy+$10),$ff
		ld	a,(ix+33)
		and	$03
		ld	b,a
		ld	a,4			; cp/m buffer is 512 bytes (old: 1024 bytes / ld a,8)
		sub	b
		ld	b,a
		xor	a
		srl	b
		rr	a
		ld	c,a			; bc = number of bytes to read
		push	ix
		ld	ix,varB9DA
		ld	de,varB2D4
		ld	a,$ff			; segment type
		call	dskReadFIB
		pop	ix
		jr	z,_bufread1
		cp	_EOF
		jr	nz,_bufread2
_bufread1:	ld	a,b
		or	c
		jr	z,_bufread2
		ld	hl,127
		add	hl,bc
		add	hl,hl
		ld	(iy+$11),h
		xor	a
		ld	b,a
		sub	c
		and	$7f
		ld	c,a
		ld	a,$ff			; segment type
		call    nz,ClearRecPart
		push	ix
		pop	hl
		ld	bc,33
		add	hl,bc
		ld	de,varBB92
		ld	c,3
		ldir
		ld	l,(ix+26)
		ld	h,(ix+27)
		ld	(varBB8E),hl
		ld	a,(ix+0)
		ld	(varBB90),a
		xor	a
		ret
_bufread2:	xor	a
		ld	b,a
		inc	a
		ret

; Subroutine clear part of record
; Input:  a  = segment type (b2 set BDOS, b2 reset DOS)
;         de = address
;         bc = number of bytes to clear
ClearRecPart:	push	af
		push	de
_clearrecpart1:	xor	a
		ld	(de),a
		inc	de
		dec	bc
		ld	a,b
		or	c
		jr	z,_clearrecpart2
		bit	6,d
		jr	z,_clearrecpart1
		pop	af
		and	$c0
		add	a,$40
		ld	d,a
		ld	e,$00
		pop	af
		jr	ClearRecPart
_clearrecpart2:	pop	af
		pop	af
		ret

; Subroutine increase record number FCB
IncRecord:	inc	(ix+32)
		ret	p
		ld	(ix+32),$00
		inc	(ix+12)
		jr	nz,UpdRecCount
		inc	(ix+14)

; Subroutine update record count in current extent FCB
; Input: ix = pointer to FCB
;        FIB values
UpdRecCount:	ld	hl,(varB9DA+21)
		xor	a
		ld 	b,a
		sub	l
		and	$7f
		ld	c,a
		add	hl,bc
		ld	bc,(varB9DA+23)
		jr	nc,_updreccount1
		inc	bc
_updreccount1:	ld	a,(varB9DA+46)
		and	$c0
		ld	d,a
		xor	a
		ld	e,a
		sbc	hl,de
		push	bc
		ex	(sp),hl
		ld	bc,(varB9DA+47)
		sbc	hl,bc
		pop	hl
		ld	b,a
		jr	c,_updreccount2
		ld	b,$80
		jr	nz,_updreccount2
		ld	a,h
		and	$c0
		jr	nz,_updreccount2
		add	hl,hl
		ld	b,h
_updreccount2:	ld	(ix+15),b
		ret

; Subroutine setup random record from current extent FCB
SetRndRec:	ld	a,(ix+32)

; Subroutine setup random record from current extent FCB
; Input:  a = record in extent
SetExtRec:	ld	b,(ix+14)
		ld	c,(ix+12)
		add	a,a
		srl	b
		rr	c
		rra
		ld	(ix+33),a
		ld	(ix+34),c
		ld	(ix+35),b

; Subroutine update current file position from random record number
; Input:  bc,a = random record number
UpdFilePtr:	ld	hl,varB9DA+45
		ld	(hl),$00
		srl	b
		rr	c
		rra
		rr	(hl)
		inc	hl
		ld	(hl),a
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		ret

; Subroutine update FCB fields from FIB
; Input:  ix = pointer to FCB
;         FIB values
UpdateFCB:	push	ix
		pop	hl
		ld	bc,16
		add	hl,bc
		ex	de,hl
		ld	hl,varB9DA+21
		ld	c,4
		ldir
		ld	hl,varB9DA+26
		ld	c,4
		ldir
		ld	hl,varB9DA+37
		ld	c,8
		ldir

		; FAT16 CLUST2
		ld	hl,(varB9DA+32)
		call	CHKDRV
		ret	z			; z=FAT16

		ld	a,(ix+29)
		and	$0f
		ld	b,a
		ld	a,(varB9DA+14)
		bit	0,a
		jr	z,_updatefcb1
		set	6,b
_updatefcb1:	ld	a,(varB9DA+30)
		bit	7,a
		jr	z,_updatefcb2
		set	5,b
_updatefcb2:	ld	a,(varB9DA+30)
		bit	6,a
		jr	z,_updatefcb3
		set	4,b
_updatefcb3:	ld	a,(varB9DA+49)
		bit	7,a
		jr	z,_updatefcb4
		set	7,b
_updatefcb4:	ld	(ix+29),b
		ret

; Subroutine copy file name back to FCB
; Input:  de = directory entry
;         hl = pointer to FCB
FnameToFCB:	inc	hl
		ld	a,(de)
		cp	$05
		jr	nz,_fnametofcb1
		ld	a,$e5
_fnametofcb1:	ld	(hl),a
		inc	hl
		inc	de
		ld	bc,10
		ex	de,hl
		ldir
		ex	de,hl
		ret

; ------------------------------------------------------------------------------
; *** FAT16 subroutine ***
; ------------------------------------------------------------------------------

; Calculate disk free space for FAT16 drives.
; If FASTALLOC is defined then cut off count if number of free clusters is more than 2K.

ALLOC16:	ld	b,d
		ld	c,e			; bc = total cluster countdown
		inc	bc			; adjust for start cluster 2
		ld	l,(ix+dub_reserved+0)
		ld	h,(ix+dub_reserved+1)	; hl = first fat sector
		ld	e,b
		ld	d,0
		add	hl,de			; start at last sector
		inc	b			; total FAT sector count
		ld	a,c
		or	a			; (Total clusters) MOD 256 = 0 ?
		jr	nz,r01			; nz=no
		dec	hl			; adjust last FAT sector
		dec	b			; adjust FAT sector counter
r01:		ld	e,d			; de=0: free cluster counter

		; b  = FAT sector counter
		; c  = End of FAT in last FAT sector
		; de = Free cluster counter
		; hl = FAT sector address
free_0:		push	hl
		push	bc
		push	de
		ex	de,hl
		xor	a
		call	dskFatSector
		ld	bc,$0b
		add	hl,bc			; hl = pointer to sector data
		pop	de
		pop	bc
		ld	a,c			; end of FAT in last sector
		ld	c,0			; next FAT sectors
		push	bc
		ld	b,a
		call	free_1			; add free clusters in fat sector to counter
		pop	bc			; remaining total cluster count
		pop	hl
		dec	hl			; next FAT sector
	IFDEF FASTALLOC
		ld	a,d
		and	$f8			; free clusters >= 2048? 
		jr	z,free_3		; z=no
		ld	de,$0800		; set free clusters to 2048
		jr	free_4
free_3:
	ENDIF
		djnz	free_0
free_4:		push	de
		jp	ALLOC_RET

; count free clusters in fat16 sector
free_1:		ld	a,(hl)
		inc	hl
		or	(hl)
		inc	hl
		jr	nz,free_2
		inc	de
free_2:		djnz	free_1
		ret

