; ------------------------------------------------------------------------------
; cs0_ram.asm
; RAM functions.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS0_RAM

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_BUFFER		; sys
		PUBLIC	K_ALLSEG		; sys
		PUBLIC	K_FRESEG		; sys
		PUBLIC	ramVarAlloc		; sys,fhs,env
		PUBLIC	ramVarFree		; sys,fhs
		PUBLIC	ramFreeUserSeg		; sys

		; dsk
		EXTERN	F_ASSIGN
		EXTERN	dskFlushBuf

; ---------------------------------------------------------
; Function $69 _BUFFER
; ---------------------------------------------------------
F_BUFFER:	ld	a,b
		cp	$01			; old:2
		jr	c,r003
r001:		ld	a,b
		cp	(iy+122)
		jr	z,r003
		jr	nc,r002
		ld	hl,(varBBF8)
		call	dskFlushBuf
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		dec	hl
		ld	(varBBF8),de
		call	ramVarFree
		ld	hl,$0000
		ld	(varBBF6),hl
		dec	(iy+122)
		jr	r001

r002:		ld	hl,512+11
		call	ramVarAlloc
		jr	nz,r003
		ld	de,(varBBF8)
		ld	(varBBF8),hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		ld	hl,varBBFA
		inc	(hl)
		jr	r001

r003:		ld	b,(iy+122)
		xor	a
		ret

; ---------------------------------------------------------
; *** K_ALLSEG ***
; ---------------------------------------------------------
K_ALLSEG:	or	a
		ld	a,(varBBFE)
		jr	z,r011
		ld	a,$ff
r011:		ex	af,af'
		ld	c,b
		ld	a,c
		and	$8f
		jr	nz,r012
		ld	a,(RAMAD3)
		or	c
		ld	c,a
r012:		ld	a,c
		and	$70
		jr	nz,r013
		jr	AllocSlotSeg

r013:		ld	b,c
		cp	$20
		jr	nz,r014
		call	AllocSlotSeg
		jr	nc,r019
r014:		xor	a
		ld	hl,EXPTBL
r015:		bit	7,(hl)
		jr	z,r016
		set	7,a
r016:		ld	c,a
		xor	b
		and	$8f
		jr	z,r017
		push	hl
		call	AllocSlotSeg
		pop	hl
		jr	nc,r019
r017:		ld	a,c
		bit	7,a
		jr	z,r018
		add	a,4
		bit	4,a
		jr	z,r016
r018:		inc	hl
		inc	a
		and	$03
		jr	nz,r015
		ld	a,b
		and	$70
		cp	$30
		scf
		jr	nz,r019
		ld	c,b
		call	AllocSlotSeg
r019:		push	af
		ld	a,c
		and	$8f
		ld	b,a
		pop	af
		ret

; Subroutine allocate segment of the specified slot
; ref: C1206
AllocSlotSeg:	push	bc
		ld	a,c
		and	$0f
		add	a,a
		add	a,a
		ld	e,a
		ld	d,$00
		ld	hl,varRamTab
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		or	h
		jr	z,r026
		ld	a,(de)
		inc	de
		ld	c,a
		ex	af,af'
		ld	b,a
		ex	af,af'
		inc	b
		jr	z,r022
		ld	b,$00
r020:		ld	a,(hl)
		or	a
		jr	z,r021
		inc	b
		inc	hl
		dec	c
		jr	nz,r020
		jr	r026

r021:		ex	de,hl
		dec	(hl)
		inc	hl
		inc	hl
		inc	(hl)
		jr	r025

r022:		add	hl,bc
r023:		dec	hl
		ld	a,(hl)
		or	a
		jr	z,r024
		dec	c
		jr	nz,r023
		jr	r026

r024:		ld	b,c
		dec	b
		ex	de,hl
		dec	(hl)
		inc	hl
		inc	(hl)
r025:		ex	af,af'
		ld	(de),a
		ex	af,af'
		ld	a,b
		pop	bc
		or	a
		ret

r026:		pop	bc
		scf
		ret

; ---------------------------------------------------------
; *** K_FRESEG ***
; ---------------------------------------------------------
K_FRESEG:	ld	c,a
		ld	a,b
		and	$8f
		jr	nz,r031
		ld	a,(RAMAD3)
r031:		and	$0f
		add	a,a
		add	a,a
		ld	e,a
		ld	d,$00
		ld	hl,varRamTab
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		or	h
		jr	z,r033
		ld	a,(de)
		cp	c
		jr	c,r033
		jr	z,r033
		ld	b,$00
		add	hl,bc
		ld	a,(hl)
		or	a
		jr	z,r033
		ld	(hl),b
		ex	de,hl
		inc	hl
		inc	(hl)
		inc	hl
		inc	a
		jr	z,r032
		inc	hl
r032:		dec	(hl)
		or	a
		ret

r033:		scf
		ret

; ---------------------------------------------------------
; Subroutine free user segments
; Input:  B = proces id
; ref: C1290
; ---------------------------------------------------------
ramFreeUserSeg:	ld	c,$10
		ld	hl,varRamTab
r041:		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	(sp),hl
		ld	a,h
		or	l
		jr	z,r044
		push	bc
		ld	c,(hl)
r042:		ld	a,(de)
		inc	a
		jr	z,r043
		dec	a
		jr	z,r043
		dec	a
		cp	b
		jr	c,r043
		push	hl
		xor	a
		ld	(de),a
		inc	hl
		inc	(hl)
		inc	hl
		inc	hl
		dec	(hl)
		pop	hl
r043:		inc	de
		dec	c
		jr	nz,r042
		pop	bc
r044:		pop	hl
		dec	c
		jr	nz,r041
		ret

; ---------------------------------------------------------
; Subroutine allocate BDOS data block
; Inputs  HL = size
; ---------------------------------------------------------
ramVarAlloc:	push	de
		push	bc
		inc	hl
		res	0,l
		ld	b,h
		ld	c,l
		ld	hl,(VARMEMPTR)
r051:		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,d
		or	e
		jr	z,r055
		bit	0,e
		jr	nz,r052
		ex	de,hl
		sbc	hl,bc
		jr	nc,r053
		add	hl,bc
		ex	de,hl
r052:		res	0,e
		add	hl,de
		jr	r051

r053:		ex	de,hl
		dec	hl
		dec	hl
		jr	z,r056
		dec	de
		dec	de
		ld	a,d
		or	e
		jr	z,r054
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		add	hl,de
		jr	r056

r054:		inc	bc
		inc	bc
		jr	r056

r055:		ld	a,0deh
		inc	bc
		inc	bc
		ld	hl,(VARMEMPTR)
		or	a
		sbc	hl,bc
		jr	c,r058

		; below bottom of reserved memory?
		push	de
		ld	de,(VARLOMEM)
		sbc	hl,de
		add	hl,de
		pop	de
		jr	c,r058

		ld	(VARMEMPTR),hl
		dec	bc
		dec	bc
r056:		ld	(hl),c
		set	0,(hl)
		inc	hl
		ld	(hl),b
		inc	hl
		push	hl
r057:		ld	(hl),$00
		inc	hl
		dec	bc
		ld	a,b
		or	c
		jr	nz,r057
		pop	hl
r058:		pop	bc
		pop	de
		or	a
		ret

; ---------------------------------------------------------
; Subroutine free BDOS data block
; Input:  HL = address of block
; ---------------------------------------------------------
ramVarFree:	dec	hl
		dec	hl
		res	0,(hl)
		push	de
		push	bc
		ld	hl,(VARMEMPTR)
r061:		ld	c,(hl)
		bit	0,c
		jr	nz,r062
		inc	hl
		ld	b,(hl)
		inc	hl
		add	hl,bc
		jr	r061

r062:		ld	(VARMEMPTR),hl
r063:		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,d
		or	e
		jr	z,r067
		bit	0,e
		jr	nz,r066
r064:		push	hl
		add	hl,de
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		pop	hl
		bit	0,c
		jr	nz,r065
		inc	bc
		inc	bc
		ex	de,hl
		add	hl,bc
		ex	de,hl
		jr	r064

r065:		dec	hl
		ld	(hl),d
		dec	hl
		ld	(hl),e
		inc	hl
		inc	hl
r066:		res	0,e
		add	hl,de
		jr	r063

r067:		pop	bc
		pop	de
		ret

