; ------------------------------------------------------------------------------
; cs1_ram.asm
; RAM mapper routines.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS1_RAM

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	ramInitMapDos
		PUBLIC	ramInitMapTab
		PUBLIC	InitRamMapper
		PUBLIC	ramAllocMem
		PUBLIC	ramMapper		; sys

		EXTERN	sysGetSlotP3

; ------------------------------------------------------------------------------
; Part 1 of subroutine to initialize and allocate memory mapper segments for dos
; This part must run before initialization of the paging helper routines
; ------------------------------------------------------------------------------
ramInitMapDos:	call	InitRamMapper
		jr	nc,r001			; nc=minimum 4 RAM mapper segments available

		; No RAM mapper
		xor	a
		ld	(VARMAPPER),a		; reset RAM Mapper flags
		; assume that all system RAM is in the same slot 
		; optimize: use MSXDOS1 method to detect system ram in slots 0 to 2
		call	sysGetSlotP3		
		LD	(RAMAD3),A
		LD	(RAMAD2),A
		LD	(RAMAD1),A
		LD	(RAMAD0),A
		ret

		; Use RAM Mapper
r001:		ld	a,1			; set bit 0: use RAM Mapper
		ld	(VARMAPPER),a		; set RAM Mapper flags
		ex	de,hl
		ld	hl,KBUF+32
		sbc	hl,de
		add	hl,hl
		add	hl,hl
		inc	hl
		push	de
		call	ramAllocMem
		pop	de
		ret	c
		ld	(MAP_TAB),hl
		ex	de,hl
r002:		ld	a,(hl)
		or	a
		jr	z,r004
		inc	hl
		ldi
		ld	(de),a
		inc	de
		ld	(de),a
		inc	de
		xor	a
		ld	b,5
r003:	 	ld	(de),a
		inc	de
		djnz	r003
		jr	r002
r004:		ld	(de),a
		ld	hl,(MAP_TAB)
		ld	a,(hl)			; slotid primary memory mapper
		push	hl
		ld	(RAMAD3),a
		ld	(RAMAD2),a
		ld	(RAMAD1),a
		ld	(RAMAD0),a
		pop	hl
		inc	hl
		ld	a,(hl)			; number of segments in primary memory mapper
		sub	4			; four segments for tpa
		inc	hl
		ld	(hl),a			; free segments primary memory mapper
		inc	hl
		ld	(hl),4			; 4 reserved segments primary memory mapper
		ld	de,P0_64K
		ld	hl,P0_SEG
		ld	a,4-1
r005:		ld	(de),a
		inc	de
		ld	(hl),a
		inc	hl
		dec	a
		jp	p,r005
		or	a			; reset c-flag
		ret

; ------------------------------------------------------------------------------
; Part 2 of subroutine to initialize and allocate memory mapper segments for dos
; This part must run after initialization of the paging helper routines
; ------------------------------------------------------------------------------
ramInitMapTab:	ld	hl,(MAP_TAB)
		ld	a,(hl)			; slotid primary memory mapper
		inc	hl
		push	hl
		call	AllocSeg
		;dec	hl
		;dec	(hl)
		;dec	hl
		;dec	(hl)
		ex	de,hl
		ld	b,4
r011:		dec	(hl)
		inc	hl
		djnz	r011
		pop	hl
		ld	bc,7
		add	hl,bc
r012:		ld	a,(hl)
		or	a
		ret	z
		inc	hl
		push	hl
		call	AllocSeg
		pop	hl
		ld	bc,7
		add	hl,bc
		jr	r012

AllocSeg:	ex	de,hl
		and	$0f
		add	a,a
		add	a,a
		ld	c,a
		ld	b,$00
		ld	hl,varRamTab
		add	hl,bc
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ex	de,hl
		ld	c,(hl)
		ld	b,$00
		ld	hl,(VARMEMPTR)
		or	a
		sbc	hl,bc
		ld	(VARMEMPTR),hl
		inc	hl
		inc	hl
		ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		ld	h,d
		ld	l,e
		add	hl,bc
		ret

; ------------------------------------------------------------------------------
; Subroutine initialize memory mapper
; ------------------------------------------------------------------------------
InitRamMapper:	di
		push	af
		ld	hl,KBUF+32
		xor	a
		ld	(hl),a			; empty memory mapper list
		push	hl
		ld	hl,EXPTBL+0		; start with slot 0
r021:		bit	7,(hl)			; expanded slot?
		jr	z,r022			; z=no
		set	7,a
r022:		push	hl
		push	af
		ld	h,$40
		call	ENASLT			; enable slot on page 1
		call	TestForMapper
		or	a
		jr	z,r024			; no memory mapper, next
		pop	bc
		pop	hl
		ex	(sp),hl
		push	bc
		ld	c,a
		ld	a,(hl)
		cp	c			; this memory mapper bigger then sofar ?
		jr	c,r023			; c=no
		ld	(hl),c			; put this memory mapper last on the list
		ld	c,a
		inc	hl
		ld	a,(hl)
		ld	(hl),b
		ld	b,a
		dec	hl			; put this memory mapper on the list and keep bigest last on the list
r023:		dec	hl
		ld	(hl),b
		dec	hl
		ld	(hl),c
		pop	bc
		ex	(sp),hl
		push	hl
		push	bc
r024:		pop	af
		pop	hl
		bit	7,a
		jr	z,r025
		add	a,4
		bit	4,a
		jr	z,r022
r025:		inc	hl
		inc	a
		and	$03
		jr	nz,r021
		pop	hl
		pop	af
		push	af			; save page 1 slot
		push	hl
		ld	h,$40
		call	ENASLT			; restore disk rom slot on page 1
		pop	hl
		pop	bc			; load page 1 slot..
		ld	a,(hl)
		cp	4			; minimum 4 segments for system?
		ret	c			; c=no
		push	hl
		push	bc			; .. and save page 1 slot
		inc	hl
		call	SetRamP3		; Set page 3 RAM to primary mapper
		pop	af			; load page 1 slot again
		ld	h,$40
		call	ENASLT			; restore disk rom slot on page 1 again
		pop	hl
		xor	a
		ret

; ------------------------------------------------------------------------------
; Subroutine test if there is a RAM mapper in the selected slot
; Note: unlike DOS2, the test uses page 1 because this ROM routine is executed in page 2.
; Output: A = number of RAM mapper segments, 0 if no mapper in slot
; ------------------------------------------------------------------------------
TestForMapper:	ld	hl,$4000
		; pass 1: test write/read segment 0
		ld	a,1
		out	($fd),a			; set page 1 to segment 1
		ld	b,(hl)			; save byte 1
		ld	(hl),$aa		; write test value AA in segment 1
		xor	a
		out	($fd),a			; set page 1 to segment 0
		ld	c,(hl)			; save byte 0
		ld	(hl),$55		; write test value 55 in segment 0
		inc	a
		out	($fd),a			; set page 1 to segment 1
		ld	e,(hl)			; read test byte 1 in E
		xor	a
		out	($fd),a			; set page 1 to segment 0
		ld	(hl),c			; restore byte 0
		inc	a
		out	($fd),a			; set page 1 to segment 1
		ld	(hl),b			; restore byte 1
		ld	a,e			; AA=mapper 55=no mapper
		cp	$aa			; is mapper?
		ld	b,$00			; set RAM segments to 0
		jr	nz,_restore2		; nz=no mapper
		; pass 2: write test byte to all segments
_testpass2:	ld	a,b
		out	($fd),a
		ld	a,(hl)
		push	af			; save byte on stack
		inc	sp			; "
		ld	(hl),$aa
		inc	b
		jr	nz,_testpass2
		; pass 3: determine number of valid ram segments
_testpass3:	ld	a,b
		out	($fd),a
		ld	a,(hl)
		cp	$aa			; valid ram segment?
		jr	nz,_restore		; nz=no
		ld	a,$55
		ld	(hl),a
		cp	(hl)			; 2nd test ok?
		jr	nz,_restore		; nz=no
		inc	b
		jr	nz,_testpass3
		dec	b			; maximum segment number is 255
		; restore data saved on stack for each segment
_restore:	ld	c,$00
_restore1:	ld	a,c
		dec	a
		out	($fd),a
		dec	sp			; load byte on stack
		pop	af			; "
		ld	(hl),a
		dec	c
		jr	nz,_restore1
		; restore/set ram segment 2 in page 1
_restore2:	ld	a,$02
		out	($fd),a
		ld	a,b
		ret

; ------------------------------------------------------------------------------
; Subroutine select memory mapper segment on page 3
; Input: (hl) = mapper RAM slot
; ------------------------------------------------------------------------------
SetRamP3:	ld	a,(hl)
		push	af
		ld	h,$40
		call	ENASLT		; enable ram mapper in page 1
		xor	a
		out	($fd),a		; set page 1 to ram segment 0

		; copy current page 3 content between sp and 0xffff to the new ram segment
		ld	hl,$0000
		sbc	hl,sp
		ld	c,l
		ld	b,h		; bc = size of area above stackpointer
		ld	hl,$0000
		add	hl,sp		; hl = start of area above stackpointer
		ld	e,l
		ld	d,h
		res	7,d		; de = hl page 1 based
		ldir

		; initialize ram segments on all pages
		out	($ff),a		; page 3 segment 0
		inc	a
		out	($fe),a		; page 2 segment 1
		inc	a
		out	($fd),a		; page 1 segment 2
		inc	a
		out	($fc),a		; page 0 segment 3

		; set page 3 to new mapper slot
		pop	af
		ld	hl,$0000
		add	hl,sp		; save current stack pointer
		ld	sp,$8000	; set stackpointer to page 1
		push	af
		push	hl
		call	ENASLT		; enable slot on page where stackpointer was in (page 3)
		pop	hl
		pop	af
		ld	sp,hl		; restore stackpointer
		bit	7,a
		ret	z
		and	$03
		ld	c,a
		ld	b,$00
		ld	hl,SLTTBL
		add	hl,bc
		ld	a,(AFFFF)
		cpl
		ld	(hl),a
		ret

; ------------------------------------------------------------------------------
; *** Allocate RAM memory ***
; ------------------------------------------------------------------------------
ramAllocMem:	ld	a,l
		or	h
		ret	z
		ex	de,hl
		ld	hl,$0000
		sbc	hl,de
		ld	c,l
		ld	b,h
		add	hl,sp
		ccf
		ret	c
		ld	a,h
		cp	$c2
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
		ld	de,-534
		add	hl,de
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
		jr	r061
		ld	a,1
		ld	(MAXFIL),a
		ld	hl,(HIMEM)
		ld	de,-534
		add	hl,de
		ld	(FILTAB),hl
		ld	e,l
		ld	d,h
		dec	hl
		dec	hl
		ld	(MEMSIZ),hl
		ld	bc,200
		or	a
		sbc	hl,bc
		push	hl
		ld	hl,13
		add	hl,de
		ld	(NULBUF),hl
		pop	hl
r061:		ld	(STKTOP),hl
		dec	hl
		dec	hl
		ld	(SAVSTK),hl
		ld	l,e
		ld	h,d
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a,$02
r062:		ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ex	de,hl
		ld	bc,7
		ld	(hl),b
		add	hl,bc
		ld	(hl),b
		ld	bc,$0102
		add	hl,bc
		dec	a
		jr	nz,r062
		ret

; ------------------------------------------------------------------------------
; *** EXTBIO handler RAM mapper  ***
; ------------------------------------------------------------------------------
ramMapper:	push	de
		push	af
		call	Mapper
		pop	af
		pop	de
		ret

Mapper:		ld	a,d
		or	e			; broadcast + function: build device name table ?
		jr	nz,r071			; nope,
		ld	a,4
		call	WriteSlot		; memory mapper device id
		jp	WriteSlot		; reserved byte
r071:		ld	a,d
		cp	4			; memory mapper device ?
		ret	nz			; nope, quit
		ld	a,e
		or	a			; function 0 ?
		jr	z,Mapper0		; z=yes
		dec	a			; function 1 ?
		jr	z,Mapper1		; z=yes
		dec	a			; function 2 ?
		jr	z,Mapper2		; z=yes
		ret

; Function 0: Build
Mapper0:	push	hl
		ld	hl,(MAP_TAB)
		ld	c,(hl)			; slotid primary memory mapper
		inc	hl
		ld	d,(hl)			; number of segments in primary memory mapper
		inc	hl
		ld	e,(hl)			; number of free segments in primary memory mapper
		pop	hl
		ld	a,c
		call	WriteSlot		; slotid
		ld	a,MAP_VE % 256		; rem: LOW MAP_VE
		call	WriteSlot
		ld	a,MAP_VE / 256		; rem: HIGH MAP_VE
		call	WriteSlot		; memory mapper jump table
		ld	a,e
		CALL	WriteSlot		; number of free segments in primary memory mapper
		ld	a,d
		call	WriteSlot		; number of segments in primary memory mapper
		call	WriteSlot		; reserved byte
		call	WriteSlot		; reserved byte
		jp	WriteSlot		; reserved byte

; Function 1: Get mapper variable table
Mapper1:	pop	de
		pop	af
		ld	hl,(MAP_TAB)
		ld	a,(hl)			; slotid primary memory mapper
		push	af
		push	de
		ret

; Function 2: Get mapper support routine address
Mapper2:	pop	de
		pop	af
		ld	hl,(MAP_TAB)
		ld	b,(hl)			; slotid primary memory mapper
		inc	hl
		ld	a,(hl)			; number of segments in primary memory mapper
		inc	hl
		ld	c,(hl)			; number of free segments in primary memory mapper
		ld	hl,MAP_VE		; memory mapper jump table
		push	af
		push	de
		ret

; Subroutine write value to slot
; Input:  a  = value
;         b  = slot
;         hl = address
; Output: hl = hl+1
WriteSlot:	push	bc
		push	de
		ld	e,a
		ld	a,b
		call	WRSLT			; (a:hl)=e
		pop	de
		pop	bc
		inc	hl			; increase address
		xor	a			; set default value to 0 for next call to this routine
		ret
