; ------------------------------------------------------------------------------
; cs1_pag.asm
; Paging helper routines and static variables in RAM page 3.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION CS1_PAG

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	pagBegin		; I4418
		PUBLIC	pagTable		; I485F
		PUBLIC	pagVectors		; I43F6

		EXTERN	K_BDOS			; sys
		EXTERN	K_ALLSEG		; sys
		EXTERN	K_FRESEG		; sys

; ------------------------------------------------------------------------------
; *** Paging helper routines loaded in RAM P3 ***
; ------------------------------------------------------------------------------
pagBegin:
		PHASE	PAGCODE

; ---------------------------------------------------------
; Subroutine SDOSON: enable disk system rom on page 1
; Note: interrupts are disabled after call (same as DOS 1)
; ---------------------------------------------------------
PH_SDOSON:	push	af
		ld	a,(MASTER)
		jr	SetPage1

; ---------------------------------------------------------
; Subroutine SDOSOF: enable dos ram on page 1
; Note: interrupts are disabled after call (same as DOS 1)
; ---------------------------------------------------------
PH_SDOSOF:	push	af
		ld	a,(RAMAD1)

		; Set slot in page 1
SetPage1:	push	hl
		push	de
		push	bc
		ld	h,$40
		call	PH_ENASLT
		pop	bc
		pop	de
		pop	hl

		pop	af
		ret

; ---------------------------------------------------------
; Subroutine XFER: transfer to/from ram in page 1
; ---------------------------------------------------------
PH_XFER:	push    af
		push    hl
		push    de
		push    bc

		; Get SlotId page 1
		in	a,($a8)
		rrca
		rrca
		call	GetSlotExp
		jr	z,r003			; z=slot not expanded
		and	$0c			; mask for secondary slot bits
		or	$80			; set expanded slot flag
		or	c			; add primary slot bits

r003:		push    af			; save current slot in page 1
		ld	a,(RAMAD1)
		ld	h,$40
		call	PH_ENASLT		; enable TPA RAM in page 1
		pop	af
		pop	bc
		pop	de
		pop	hl
		ldir
		jr	SetPage1		; restore slot in page 1

; Get secondary slot register (if slot is expanded)
GetSlotExp:	and	$03
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

; ---------------------------------------------------------
; Subroutine BDOS entry point for disk BASIC
; ---------------------------------------------------------
PH_BDOS:	ld	(IX_BDOS),ix
		ld	iy,(MASTER-1)
		ld	ix,(SBDOS)
		jp	CALSLT

; ---------------------------------------------------------
; Subroutine jump to address on pointer (F1E2)
; ---------------------------------------------------------
PH_JPVEC:	ei
		push	de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		pop	de
		jp	(hl)

; ---------------------------------------------------------
; Subroutine call routine in BDOS code segment
; ---------------------------------------------------------
PH_GOBIOS:	call 	SDOSON
		call 	CLPRM1+1	; call (hl)
		jp	SDOSOF

; ---------------------------------------------------------
; Subroutine call BDOS handler in BDOS code segment
; ---------------------------------------------------------
PH_GOBDOS:	call	SDOSON
		call	K_BDOS
		jp	SDOSOF

; ---------------------------------------------------------
; Subroutine interrupt handler (F1E5)
; ---------------------------------------------------------
PH_SIRQ:	di
		push	af
		push	hl
		push	bc
		ld	hl,(IRQ_ST)
		or	a
		sbc	hl,sp
		jr	c,_sirq0
		ld	bc,200
		sbc	hl,bc
		jr	c,_sirq1
_sirq0:		ld	(SP_IRQ),sp
		ld	sp,(IRQ_ST)
		call	_sirq3
		ld	sp,(SP_IRQ)
		jr	_sirq2

_sirq1:		call	_sirq3
_sirq2:		pop	bc
		pop	hl
		pop	af
		ei
		ret

_sirq3:		push	de
		push	ix
		push	iy
		exx
		ex	af,af'
		ld	ix,KEYINT
		ld	iy,(EXPTBL-1)
		call	CALSLT
		di
		ex	af,af'
		exx
		pop	iy
		pop	ix
		pop	de
		ret

; ---------------------------------------------------------
; Subroutines read/write to ram underneath disk ROM
; ---------------------------------------------------------
PH_RDRAM:	call	SDOSOF
		ld	a,(hl)
		jp	SDOSON

PH_WRRAM:	call	SDOSOF
		ld	(hl),a
		jp	SDOSON

; ---------------------------------------------------------
; Subroutine BDOS function print string
; ---------------------------------------------------------
PH_PRTBUF:	ld	a,(de)
		inc	de
		cp	'$'
		jr	z,_prtbufend
		push	de
		ld	e,a
		ld	c,$02
		call	GO_BDOS
		pop	de
		jr	PH_PRTBUF
_prtbufend:	xor	a
		ld	b,a
		ld	l,a
		ld	h,a
		ret

; ---------------------------------------------------------
; ** Obsolete paging helper routines **
; ---------------------------------------------------------

PH_RDLDI:			; transfer to/from slot --> used by ramdisk only
PH_GODRV:			; BDOS call with prompt --> used by floppy driver only
PH_PUTUS:			; restore DOS TPA segments --> SDOSOF
PH_PUTBD:			; enable BDOS segments --> SDOSON
PH_P0LDIR:			; transfer to/from segment --> alternative rom code
PH_P0CALL:			; interslot call to main BIOS --> CALSLT
PH_P0RAM:			; enable DOS ram on page 0 -->  alternative rom code
PH_SFLUSH:	ret		; print string via BIOS CHPUT routine --> alternative rom code

; ---------------------------------------------------------
; ** RAM Mapper functions **
; ---------------------------------------------------------

PH_ALLSEG:	push	hl
		ld	hl,K_ALLSEG
		jr	r011

PH_FRESEG:	push	hl
		ld	hl,K_FRESEG
r011:		push	de
		call	GO_BIOS
		pop	de
		pop	hl
		ret

PH_RDSEG:	di
		push	hl
		push	bc
		ld	b,a
		call	PH_GETP2
		ld	c,a
		ld	a,b
		call	PH_PUTP2
		res	6,h
		set	7,h
		ld	b,(hl)
		ld	a,c
		call	PH_PUTP2
		ld	a,b
		pop	bc
		pop	hl
		ret

PH_WRSEG:	di
		push	hl
		push	bc
		ld	b,a
		call	PH_GETP2
		ld	c,a
		ld	a,b
		call	PH_PUTP2
		res	6,h
		set	7,h
		ld	(hl),e
		ld	a,c
		call	PH_PUTP2
		pop	bc
		pop	hl
		ret

PH_CALLS:	exx
		ex	(sp),hl
		ld	d,(hl)
		inc	hl
		push	de
		pop	iy
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		push	de
		pop	ix
		ex	(sp),hl
		exx

PH_CALSEG:	exx
		ex	af,af'
		push	ix
		pop	hl
		call	PH_GETPH
		push	af
		push	hl
		push	iy
		pop	af
		call	PH_PUTPH
		ex	af,af'
		exx
		call	CLPRM1
		exx
		ex	af,af'
		pop	hl
		pop	af
		call	PH_PUTPH
		ex	af,af'
		exx
		ret

PH_PUTPH:	bit	7,h
		jr	nz,r012
		bit	6,h
		jr	z,PH_PUTP0
		jr	PH_PUTP1

r012:		bit	6,h
		jr	z,PH_PUTP2
		jr	PH_PUTP3

PH_GETPH:	bit	7,h
		jr	nz,r013
		bit	6,h
		jr	z,PH_GETP0
		jr	PH_GETP1

r013:		bit	6,h
		jr	z,PH_GETP2
		jr	PH_GETP3

PH_PUTP0:	ld	(P0_SEG),a
		out	($fc),A
		ret

PH_GETP0:	ld	a,(P0_SEG)
		ret

PH_PUTP1:	ld	(P1_SEG),A
		out	($fd),a
		ret

PH_GETP1:	ld	a,(P1_SEG)
		ret

PH_PUTP2:	ld	(P2_SEG),a
		out	($fe),a
		ret

PH_GETP2:	ld	a,(P2_SEG)
		ret

PH_GETP3:	ld	a,(P3_SEG)
PH_PUTP3:	ret

; ---------------------------------------------------------
; ** BIOS Subroutines **
; ---------------------------------------------------------

; ---------------------------------------------------------
; Subroutine RDSLT (F1E8)
; ---------------------------------------------------------
PH_RDSLT:	res	6,d
		jr	r021

; ---------------------------------------------------------
; Subroutine WRSLT (F1EB)
; ---------------------------------------------------------
PH_WRSLT:	set	6,d
r021:		di
		ld	(data_hl+1),hl
		ex	de,hl
		ld	(data_de+1),hl
		ld	c,a
		ld	b,d
		call	GetSlotMasks
		bit	7,c
		jr	z,RwPrim
		call	ChgSecSlot
		push	de
		push	bc
		push	af
		call	RwPrim
		ld	c,a
		pop	af
		pop	de
		ld	b,d
		ex	(sp),hl
		jr	nz,r022
		ld	a,e
		ld	(AFFFF),a
r022:		call	nz,SSLOTE
		ld	(hl),e
		pop	hl
		ld	a,c
		ld	e,c
		ret

; Subroutine RDSLT/WRSLT on primary slot
RwPrim:		in	a,($a8)
		ld	b,a
		and	(hl)
		inc	hl
		or	(hl)
		di
data_hl: 	ld	hl,0
data_de: 	ld	de,0
		bit	6,d
		ld	d,b
		jp	nz,WRPRIM
		call	RDPRIM
		ld	a,e
		ret

; ---------------------------------------------------------
; Subroutine CALLF (F1F4)
; ---------------------------------------------------------
PH_CALLF:	exx
		ex	af,af'
		pop	hl
		ld	a,(hl)
		inc	hl
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl
		push	hl
		push	bc
		pop	ix
		jr	r023

; ---------------------------------------------------------
; Subroutine CALSLT (F1EE)
; Note: interrupts are disabled after call (same as BIOS)
; ---------------------------------------------------------
PH_CALSLT:	exx
		ex	af,af'
		push	ix
		pop	bc
		push	iy
		pop	af
r023:		ld	c,a
		call	GetSlotMasks
		bit	7,c
		jr	z,CallPrim
		call	ChgSecSlot
		push	bc
		push	de
		push	af
		call	CallPrim		; disables interrupts
		exx
		ex	af,af'
		pop	bc
		pop	hl
		pop	de
		ld	a,b
		cp	d
		ld	b,d
		jr	nz,r024
		ld	a,e
		ld	(AFFFF),a		; interrupts already disabled
r024:		call	nz,SSLOTE
		ld	(hl),e
		ex	af,af'
		exx
		ret

; Subroutine CALSLT on primary slot
CallPrim:	in	a,($a8)
		push	af
		and	(hl)
		inc	hl
		or	(hl)
		exx
		di
		jp      CLPRIM

; ---------------------------------------------------------
; Subroutine ENASLT (F1F1)
; ---------------------------------------------------------
PH_ENASLT:	push	hl
		ld	c,a
		ld	b,h
		call	GetSlotMasks
		bit	7,c
		call	nz,ChgSecSlot
		in	a,($a8)
		and	(hl)
		inc	hl
		or	(hl)
		di
		out	($a8),a
		pop	hl
		ret

; ---------------------------------------------------------
; Subroutine change secondary slot register
; ---------------------------------------------------------
ChgSecSlot:	push	hl
		and	$03
		ld	e,a
		ld	hl,SLTTBL
		add	hl,de
		push	hl
		ld	a,c
		rrca
		rrca
		ld	c,a
		call	GetSlotMasks
		di
		pop	de
		ld	a,(de)
		ld	(SS_TEMP),a
		and	(hl)
		inc	hl
		or	(hl)
		ld	(de),a
		ld	l,a
		in	a,($a8)
		ld	b,a
		xor	c
		and	$3f
		xor	c
		ld	h,a
		cp	b
		jr	nz,r026
		ld	a,l
		ld	(AFFFF),a
r026:		call	nz,SSLOTL
		ld	a,(SS_TEMP)
		ld	c,a
		ld	a,h
		pop	hl
		ret

; ---------------------------------------------------------
; Subroutine get slot masks
; ---------------------------------------------------------
GetSlotMasks:	and	$03
		ld	d,a
		ld	a,b
		rrca
		rrca
		rrca
		rrca
		and	$0c
		or	d
		ld	e,a
		ld	d,$00
		ld	hl,Masks
		add	hl,de
		add	hl,de
		ret

Masks:		db	$fc,$00,$fc,$01,$fc,$02,$fc,$03
		db	$f3,$00,$f3,$04,$f3,$08,$f3,$0c
		db	$cf,$00,$cf,$10,$cf,$20,$cf,$30
		db	$3f,$00,$3f,$40,$3f,$80,$3f,$c0

		DEPHASE

; ------------------------------------------------------------------------------
; Jump table $F1C9-$F22D indirect entry points
; ref: I485F
; ------------------------------------------------------------------------------
pagTable:	; Add jump to PRTBUF here
		jp	PH_PRTBUF	; F1C9 PRTBUF
		defs	7,$00		; not used

		; Repurpose RD_LDI and P0_LDIR entries
		jp	PH_RDRAM	; F1D3 RD_RAM
		jp	PH_WRRAM	; F1D6 WR_RAM

		; Obsolete
		jp	PH_P0CALL	; F1D9 P0CALL
		jp	PH_SFLUSH	; F1DC SFLUSH
		jp	PH_GODRV	; F1DF GO_DRV

		; BIOS
		jp    PH_JPVEC		; F1E2 JP_VEC	start DOS1 style handler
		jp    PH_SIRQ		; F1E5 SIRQ 	KEYINT handler
		jp    PH_RDSLT		; F1E8 RDSLT
		jp    PH_WRSLT		; F1EB WRSLT
		jp    PH_CALSLT		; F1EE CALSLT
		jp    PH_ENASLT		; F1F1 ENASLT
		jp    PH_CALLF		; F1F4 CALLF

		; Obsolete
		jp    PH_PUTBD		; F1F7 PUT_BD
		jp    PH_PUTUS		; F1FA PUT_US
		jp    PH_P0RAM		; F1FD P0_RAM

		; RAM Mapper
		jp    PH_ALLSEG		; F200 ALL_SEG
		jp    PH_FRESEG		; F203 FRE_SEG
		jp    PH_RDSEG		; F206 RD_SEG
		jp    PH_WRSEG		; F209 WR_SEG
		jp    PH_CALSEG		; F20C CAL_SEG
		jp    PH_CALLS		; F20F CALLS
		jp    PH_PUTPH		; F212 PUT_PH
		jp    PH_GETPH 		; F215 GET_PH
		jp    PH_PUTP0		; F218 PUT_P0
		jp    PH_GETP0		; F21B GET_P0
		jp    PH_PUTP1		; F21E PUT_P1
		jp    PH_GETP1		; F221 GET_P1
		jp    PH_PUTP2		; F224 PUT_P2
		jp    PH_GETP2		; F227 GET_P2
		jp    PH_PUTP3		; F22A PUT_P3
		jp    PH_GETP3		; F22D GET_P3

; ------------------------------------------------------------------------------
; *** Paging vector table ***
; Notes: must be adjacant to pagTable, table size is 8, used in ipl module.
; ref: I43F6
; ------------------------------------------------------------------------------
pagVectors:	dw	SDOSON,PH_SDOSON
		dw	SDOSOF,PH_SDOSOF
		dw	XFER,PH_XFER
		dw	SAUXIN,AUXBOD+0
		dw	SAUXOUT,AUXBOD+5
		dw	GO_BIOS,PH_GOBIOS
		dw	GO_BDOS,PH_GOBDOS
		dw	BDOS,PH_BDOS
