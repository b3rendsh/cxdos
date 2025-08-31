; ------------------------------------------------------------------------------
; cs1_env.asm
; Environment variable functions.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS1_ENV

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_GENV			; sys,fcb
		PUBLIC	F_SENV			; sys
		PUBLIC	F_FENV			; sys

		; routines in code segment 0
		EXTERN	sysCheckChar
		EXTERN	sysChainRmv
		EXTERN	ramVarAlloc

; ---------------------------------------------------------
; Function $6B GENV
; Input:  hl = pointer to ASCIIZ environment name string
;         de = pointer to buffer for value
;         b  = size of buffer
; Output: a  = error
;         de = preserved, buffer filled in if A=0
; ---------------------------------------------------------
F_GENV:		push	bc
		call	ValEnvStr		; validate environment name
		pop	bc
		ret	nz			; nz=invalid name
		push	de
		push	bc
		ld	de,varEnvPtr		; start of environment variables chain
		call	SearchEnv		; search for environment variable
		ld	de,EnvBlank
		jr	nc,r001			; nc=not found
		ld	d,b
		ld	e,c
r001:		pop	bc
		pop	hl
		call	EnvToBuf		; copy environment variable value to buffer
		ex	de,hl
		ret

; ---------------------------------------------------------
; Function $6C SENV
; Input:  hl = pointer to ASCIIZ environment name string
;         de = pointer to ASCIIZ value string
; Output: a  = error
; ---------------------------------------------------------
F_SENV:		call	ValEnvStr		; valid environment name?
		ret	nz			; nz=no
		ld	a,c
		or	a
		ret	z			; z=blank environment name
		ex	af,af'
		ex	de,hl
		ld	a,$01			; don't uppercase
		call	ValEnvStr1		; valid environment value?
		ret	nz			; nz=no
		ld	a,c
		or	a
		jr	z,r002			; z=no value, remove environment variable
		ex	af,af'
		push	hl
		ld	hl,$0004		; pointer (2 bytes) + 2 terminating zero's
		ld	b,h			; b=0 c=length of value
		add	hl,bc			; add length of value
		ld	c,a			; b=0 c=length of name
		add	hl,bc			; add length of name
		call	ramVarAlloc		; allocate memory for environment variable
		pop	bc
		ret	nz			; nz=out of memory
		push	bc
		ld	bc,(varEnvPtr)		; update environment variable chain
		ld	(varEnvPtr),hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ex	de,hl
		xor	a			; uppercase
		call	BufToEnv		; copy variable name
		ex	(sp),hl
		ld	a,$01			; don't uppercase
		call	BufToEnv		; copy variable value
		pop     hl
		ld	de,(varEnvPtr)
		jr	r003
r002:		ex	de,hl
		ld	de,varEnvPtr
r003:		call	SearchEnv		; does environment variable already exist?
		ld	hl,varEnvPtr
		call	c,sysChainRmv		; c=yes, remove old variable element from chain
		xor	a
		ret

; ---------------------------------------------------------
; Function $6D FENV
; Input:  de = environment variable item number
;         hl = pointer to buffer for name string
; Output: a  = error
;         hl = preserved, buffer filled in
; ---------------------------------------------------------
F_FENV:		push	hl
		push	bc
		ld	b,d			; init item counter
		ld	c,e
		ld	hl,(varEnvPtr)
r004:		ld	a,h
		or	l
		ld	de,EnvBlank
		jr	z,r005			; z=item number doesn't exist
		ld	e,(hl)			; get next item pointer
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	de,hl			; hl=next item pointer, de=item name pointer
		dec	bc			; decrease item counter
		ld	a,b
		or	c
		jr	nz,r004			; nz=get next item
r005:		pop	bc
		pop	hl
		jp	EnvToBuf		; item found, copy value to buffer

EnvBlank:	dw	$0000

; ---------------------------------------------------------

; Subroutine search for environment variable
; Output: cx set if found
SearchEnv:	ex	de,hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		or	h
		ex	de,hl
		ret	z
		push	de
		push	hl
		inc	de
		inc	de
		call	CheckEnv
		ld	b,d
		ld	c,e
		pop	hl
		pop	de
		jr	nz,SearchEnv
		scf
		ret

; Subroutine validate environment variable string
ValEnvStr:	xor	a			; don't uppercase
ValEnvStr1:	ld	c,a			; set checkchar flags
		push	hl
		ld	b,$ff
r006:		call	RD_RAM			; a=(hl)
		inc	hl
		call	sysCheckChar
		jr	z,r008
		bit	0,c
		jr	nz,r007
		bit	4,c
		ld	a,_IENV			; invalid environment string
		jr	nz,r009
r007:		djnz	r006
		ld	a,_ELONG		; environment string too long
		jr	r009
r008:		dec	a
		sub	b
		ld	c,a
		xor	a
r009:		pop	hl
		or	a
		ret

; Subroutine copy from environment variable to buffer
EnvToBuf:	push	hl
		push	de
r010:		ld	a,b
		dec	b
		or	a
		ld	a,_ELONG		; environment string too long
		jr	z,r011
		ld	a,(de)
		call	WR_RAM			; (hl)=a
		inc	hl
		inc	de
		or	a
		jr	nz,r010
r011:		pop	de
		pop	hl
		or	a
		ret

; Subroutine copy from buffer to environment variable
BufToEnv:	push	hl
		ld	c,a			; set checkchar flags
r012:		call	RD_RAM			; a=(hl)
		inc	hl
		call	sysCheckChar
		ld	(de),a
		inc	de
		or	a
		jr	nz,r012
		pop	hl
		ret

; Subroutine check if environment variable
CheckEnv:	ld	c,$00			; set checkchar flags: uppercase
r013:		call	RD_RAM			; a=(hl)
		inc	hl
		call	sysCheckChar
		ld	b,a
		ld	a,(de)
		inc	de
		cp	b
		ret	nz
		or	a
		jr	nz,r013
		ret
