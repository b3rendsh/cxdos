; ------------------------------------------------------------------------------
; cs1_tim.asm
; Time and date functions.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS1_TIM

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_GDATE			; sys
		PUBLIC	F_SDATE			; sys
		PUBLIC	F_GTIME			; sys
		PUBLIC	F_STIME			; sys
		PUBLIC	timInitRTC		; sys
		PUBLIC	timReadRTC		; dsk

; ---------------------------------------------------------
; Function $2A GDATE
; Output: hl = year 1980...2079
;         d  = month (1=Jan...12=Dec)
;         e  = date (1...31)
;         c  = day of week (0=Sun...6=Sat)
; ---------------------------------------------------------
F_GDATE:	call	timReadRTC
		ld	c,d
		ld	b,$00
		ld	e,l
		ld	d,h
		ld	hl,1980			; year base is 1980
		add	hl,bc
		ld	a,d
		cp	$03
		ld	a,c
		sbc	a,$fc
		and	$fc
		rrca
		rrca
		add	a,c
		push	hl
		ld	hl,WeekTab-1
		ld	c,d
		add	hl,bc
		add	a,(hl)
		pop	hl
		add	a,e
r001:		sub	$07
		jr	nc,r001
		add	a,$07
		ld	c,a
		xor	a
		ret

WeekTab:	db	1,4,4,7,9,12,14,17,20,22,25,27

; ---------------------------------------------------------
; Function $2B _SDATE
; Input:  hl = year 1980...2079
;         d  = month (1=Jan...12=Dec)
;         e  = date (1...31)
; Output: a  = $00: valid date $be: invalid date
;         c  = $00: valid date $ff: invalid date
; ---------------------------------------------------------
F_SDATE:	ld	bc,-1980		; year base is 1980
		add	hl,bc
		jr	nc,r013
		ld	a,h
		or	a
		jr	nz,r013
		ld	a,l
		cp	100			; year < 2080 ?
		jr	nc,r013
		ld	b,a
		ld	a,d
		dec	a
		cp	12			; valid month?
		jr	nc,r013
		ld	hl,MonthTab
		add	a,l
		ld	l,a
		jr	nc,r011
		inc	h
r011:		cp	$97
		jr	nz,r012
		ld	a,b
		and	$03
		jr	nz,r012
		ld	hl,LeapTab
r012:		ld	a,e
		dec	a
		cp	(hl)
		jr	nc,r013
		ld	l,e
		ld	h,d
		ld	d,b
		call	WriteDateRTC
		xor	a
		ld	c,a
		ret

r013:		ld	c,$ff			; error
		ld	a,_IDATE		; invalid date
		ret

MonthTab:	db	31,28,31,30,31,30,31,31,30,31,30,31
LeapTab:	db	29

; ---------------------------------------------------------
; Function $2C GTIME
; Output: h   = hours (0...23)
;         c,l = minutes (0...59)
;         d   = seconds (0...59)
;         e   = centiseconds (always zero)
; ---------------------------------------------------------
F_GTIME:	call	timReadRTC
		ld	h,b
		ld	l,c
		ld	d,e
		xor	a
		ld	e,a
		ret

; ---------------------------------------------------------
; Function $2D STIME
; Input:  h = hours (0...23)
;         l = minutes (0...59)
;         d = seconds (0...59)
;         e = centiseconds (ignored)
; Output: a = $00: valid time $bd: invalid time
;         c = $00: valid time $ff: invalid time
; ---------------------------------------------------------
F_STIME:	ld	a,h
		cp	24
		jr	nc,r021
		ld	a,l
		cp	60
		jr	nc,r021
		ld	a,d
		cp	60
		jr	nc,r021
		ld	b,h
		ld	c,l
		ld	e,d
		call	WriteTimeRTC
		xor	a
		ld	c,a
		ret

r021:		ld	c,$ff
		ld	a,_ITIME		; invalid time
		ret

; ---------------------------------------------------------
; Subroutine initialize real time clock
timInitRTC:	ld	a,13
		out	($b4),a
		in	a,($b5)
		and	$04
		ld	b,a
		inc	a
		out	($b5),a
		ld	a,10
		out	($b4),a
		ld	a,1
		out	($b5),a
		ld	a,13
		out	($b4),a
		ld	a,b
		out	($b5),a
		ld	bc,$0d00
r031:		ld	a,c
		out	($b4),a
		in	a,($b5)
		push	af
		inc	c
		djnz	r031
		ld	a,14
		out	($b4),a
		ld	a,00h
		out	($b5),a
		ld	b,13
r032:		dec	c
		pop	de
		ld	a,c
		out	($b4),a
		ld	a,d
		out	($b5),a
		djnz	r032

; Subroutine resume real time clock
ResumeRTC:	ld	a,13
		out	($b4),a
		in	a,($b5)
		or	$08
		out	($b5),a
		ret

; Subroutine pause real time clock
PauseRTC:	ld	a,13
		out	($b4),a
		in	a,($b5)
		and	$04
		out	($b5),a
		ret

; Subroutine read time and date from real time clock
timReadRTC:	call	PauseRTC
		ld	e,13
		call	ReadByteRTC
		ld	d,a
		call	ReadByteRTC
		ld	h,a
		call	ReadByteRTC
		ld	l,a
		dec	e
		call	ReadByteRTC
		ld	b,a
		call	ReadByteRTC
		ld	c,a
		call	ReadByteRTC
		ld	e,a
		jp	ResumeRTC

; Subroutine read byte (BCD) from real time clock
; Input:  E = register+1
; Output: E = register-1
ReadByteRTC:	push	bc
		call	ReadNibbleRTC
		ld	b,a
		add	a,a
		add	a,a
		add	a,b
		add	a,a
		ld	b,a
		call	ReadNibbleRTC
		add	a,b
		pop	bc
		ret

; Subroutine read nibble from real time clock
; Input:  E = register+1
; Output: E = register
ReadNibbleRTC:	dec	e
		ld	a,e
		out	($b4),a
		in	a,($b5)
		and	$0f
		ret

; Subroutine write hour,minute and second to real time clock
WriteTimeRTC:	ld	l,e
		ld	h,c
		ld	d,b
		call	PauseRTC
		ld	a,15
		out	($b4),a
		ld	a,2
		out	($b5),a
		ld	e,$00
		jr	writeRTC

; Subroutine write year,month and day to real time clock
WriteDateRTC:	call	PauseRTC
		or	$01
		out	($b5),a
		ld	a,11
		out	($b4),a
		ld	a,d
		out	($b5),a
		call	PauseRTC
		call	PauseRTC
		ld	e,$07
writeRTC:	ld	a,l
		call	WriteByteRTC
		ld	a,h
		call	WriteByteRTC
		ld	a,d
		call	WriteByteRTC
		jp	ResumeRTC

; Subroutine convert byte to BCD and write to real time clock
WriteByteRTC:	ld	c,a
		xor	a
		ld	b,8
r041:		rlc	c
		adc	a,a
		daa
		djnz	r041
		call	WriteNibbleRTC
		rrca
		rrca
		rrca
		rrca

; Subroutine write nibble to real time clock
WriteNibbleRTC:	ld	b,a
		ld	a,e
		out	($b4),a
		ld	a,b
		out	($b5),a
		inc	e
		ret

