; ------------------------------------------------------------------------------
; cs0_con.asm
; Console I/O routines.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS0_CON

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	F_CONIN			; 01
		PUBLIC	F_CONOUT		; 02
		PUBLIC	F_AUXIN			; 03
		PUBLIC	F_AUXOUT		; 04
		PUBLIC	F_LSTOUT		; 05
		PUBLIC	F_DIRIO			; 06
		PUBLIC	F_DIRIN			; 07
		PUBLIC	F_INNOE			; 08
		PUBLIC	F_BUFIN			; 0A
		PUBLIC	F_CONST			; 0B

		PUBLIC	K_CON_INIT		; sys
		PUBLIC	K_CON_CLEAR		; fhs
		PUBLIC	KB_CHARIN		; xio
		PUBLIC	KB_CHAROUTC		; xio
		PUBLIC	KB_CHARSTAT		; xio
		PUBLIC	KB_LPTOUT		; xio
		PUBLIC	KB_LPTSTAT		; xio
		PUBLIC	KB_AUXOUTC		; xio
		PUBLIC	KB_AUXIN		; xio
		PUBLIC	conClearBuf		; fhs
		PUBLIC	DEV_CON			; sys
		PUBLIC	DEV_AUX			; sys
		PUBLIC	DEV_LST			; sys
		PUBLIC	DEV_NUL			; sys

		EXTERN	sysTpaAbort
		EXTERN	sysTpaCall
		EXTERN	dskReadFIB
		EXTERN	dskWriteFIB
		EXTERN	fhsPtrFIB

; ---------------------------------------------------------
; Subroutine initialize buffered input history buffer
; ---------------------------------------------------------
K_CON_INIT:	ld	hl,varB0D0		; history buffer
		ld	(varBB82),hl		; current line pointer in history buffer
		ld	(varBB80),hl		; last line pointer in history buffer
		xor	a
		ld	(varBB7F),a		; buffer is empty
		ld	b,a			; size of history buffer (256)
_coninit1:	ld	(hl),CR
		inc	hl
		djnz	_coninit1

; ---------------------------------------------------------
; Subroutine clear stored input, disable echo to printer
; ---------------------------------------------------------
K_CON_CLEAR:	xor	a
		ld	(varBB8D),a		; no stored input
		ld	(varBB8A),a		; disable echo to printer
		ret

; ---------------------------------------------------------
; Function $01 CONIN
; ---------------------------------------------------------
F_CONIN:	call	F_INNOE			; get character
		push	hl
		ld	a,l
		call	ValCharType		; is it a printable character?
		call	nc,ConOut1		; nc=yes, echo to console output
		pop	hl
		xor	a
		ret

; ---------------------------------------------------------
; Function $02 CONOUT
; ---------------------------------------------------------
F_CONOUT:	ld	a,e
		call	ConOut
		xor	a
		ld	h,a
		ld	l,a
		ret

; ---------------------------------------------------------
; Function $08 INNOE
; ---------------------------------------------------------
F_INNOE:	bit	0,(iy+9)		; standard input redirected?
		ld	c,$ff
		jr	nz,K_HCONIN		; nz=yes
		call	CharIn
		ld	l,a
		xor	a
		ld	h,a
		ret

; ---------------------------------------------------------
; Function $0B CONST
; ---------------------------------------------------------
F_CONST:	call	ConStat
		ld	l,a
		xor	a
		ld	h,a
		ret

; ---------------------------------------------------------
; Function $06 DIRIO
; ---------------------------------------------------------
F_DIRIO:	ld	a,e
		inc	a			; parameter is 0xff ?
		jr	z,_direct_in		; z=yes, direct input
		bit	1,(iy+9)		; output redirected?
		ld	a,e
		ld	c,$00
		jr	nz,K_HCONOUT		; nz=yes, redirected output
		call	KB_CHAROUT		; send output to BIOS console
		xor	a
		ld	h,a
		ld	l,a
		ret

_direct_in:	bit	0,(iy+9)		; input redirected?
		ld	c,$00
		jr	nz,K_HCONIN		; nz=yes, redirected input
		ld	hl,varBB8D
		cp	(hl)			; character in buffer?
		jr	nz,_direct_in1		; nz=yes
		call	KB_CHARSTAT		; character available via BIOS?
		jr	nz,_direct_in1		; nz=yes
		ld	l,a
		ld	h,a
		ret

; ---------------------------------------------------------
; Function $07 DIRIN
; ---------------------------------------------------------
F_DIRIN:	bit	0,(iy+9)		; input redirected?
		ld	c,00h
		jr	nz,K_HCONIN
_direct_in1:	ld	a,(varBB8D)
		or	a			; character in buffer?
		call	z,KB_CHARIN		; z=no, get character via BIOS
		ld	l,a
		xor	a
		ld	h,a
		ld	(varBB8D),a		; clear character buffer
		ret

; ---------------------------------------------------------
; Subroutine character from console input file handle
; ---------------------------------------------------------
K_HCONIN:	ld	b,$00
		push	bc
		call	ReadCharH		; read character from file handle
		pop	de
		or	a
		jr	nz,InError
		or	e
		jr	z,J0426
		ld	a,b
		sub	03h			; CTRL-C ?
		jr	z,InError		; z=yes
J0426:		ld	l,b
		xor	a
		ld	h,a
		ret

; ---------------------------------------------------------
; Subroutine character to console output file handle
; ---------------------------------------------------------
K_HCONOUT:	ld	b,1
		jr	_out2

; ---------------------------------------------------------
; Function $03 _AUXIN
; ---------------------------------------------------------
F_AUXIN:	ld	b,3
		ld	c,0ffh
		call	ReadCharH
		or	a
		ld	l,b
		ld	h,a
		ret	z
InError:	ld	c,_INERR
		jr	ConAbort

; ---------------------------------------------------------
; Subroutine $04 _AUXOUT
; ---------------------------------------------------------
F_AUXOUT:	ld	b,3
		jr	_out1

; ---------------------------------------------------------
; Function $05 _LSTOUT
; ---------------------------------------------------------
F_LSTOUT:	ld	b,4
_out1:		ld	c,$ff
		ld	a,e
_out2:		call	WriteCharH
		or	a
		ld	l,a
		ld	h,a
		ret	z
		ld	c,_OUTERR
ConAbort:	ld	b,a
		ld	a,c
		call	sysTpaAbort		; abort program
		jr	$			; loop forever

; ---------------------------------------------------------
; Function $0A _BUFIN
; ---------------------------------------------------------
F_BUFIN:	push	de
		bit	0,(iy+9)		; redirected console input?
		jr	nz,K_HBUFIN		; nz=yes
		xor	a
		call	K_CON_BUFIN
		JR	_hbufin5

; ---------------------------------------------------------
; console line buffered input (file handle)
; ---------------------------------------------------------
K_HBUFIN:	ex	de,hl
		ld	b,(hl)
		ld	c,$00
		inc	hl
		push	hl
_hbufin1:	push	hl
		push	bc
		ld	c,$ff
		call	K_HCONIN		; read character
		ld	a,l
		pop	bc
		pop	hl
		or	a			; NULL character?
		jr	z,_hbufin1		; z=yes, read again
		cp	LF			; LF character?
		jr	z,_hbufin1		; z=yes, read again
		cp	CR			; CR character?
		jr	z,_hbufin4		; z=yes, stop reading
		ld	e,a
		ld	a,b
		cp	c			; buffer full?
		jr	z,_hbufin2		; z=yes, ping the bell
		inc	c
		inc	hl
		ld	(hl),e			; put character in buffer
		ld	a,e
		push	hl
		push	bc
		call	ConOut			; echo to screen
		jr	_hbufin3

_hbufin2:	push	hl
		push	bc
		ld	a,BELL
		call	KB_CHAROUT
_hbufin3:	pop	bc
		pop	hl
		jr	_hbufin1

_hbufin4:	pop	hl
		ld	(hl),c			; store number of characters in buffer
		call	ConOut			; echo CR to screen
_hbufin5:	pop	hl
		push	hl
		ld	a,(hl)
		inc	hl
		cp	(hl)			; is there room for a CR in the buffer?
		jr	z,_hbufin6		; z=no
		ld	e,(hl)
		ld	d,0
		add	hl,de
		inc	hl
		ld	(hl),CR			; add a CR as last character in the buffer
_hbufin6:	pop	de
		xor	a
		ld	l,a
		ld	h,a
		ret

; ---------------------------------------------------------
; Subroutine console line buffered input (keyboard)
; Inputs  A = force console output to screen flag
; ---------------------------------------------------------
K_CON_BUFIN:	ld	(varBB7A),a		; save output redirection flag
		inc	de
		xor	a
		ld	(de),a			; init buffer character counter
		dec	de
		ld	(varBB7C),a		; no up/down key

		; loop console line input
loop_line_in:	push	de
		call	K_EDIT_LINE		; get a line (exits on CR, Up or Down)
		pop	de
		dec	a
		jr	z,go_up
		dec	a
		jr	z,go_down
		inc	de
		ld	a,(de)
		or	a			; empty line entered?
		ret	z			; z=yes, exit
		ld	b,a
		ld	(varBB7F),a
		ld	a,(varBB7C)
		or	a
		jr	z,_conbufin4

		; compare line with current line in buffer
		push	de
		push	bc
		ld	hl,(varBB82)
_conbufin2:	inc	de
		ld	a,(de)
		cp	(hl)
		jr	nz,_conbufin3
		call	K_CBUF_INC
		DJNZ	_conbufin2
		ld	a,(hl)
		cp	CR
_conbufin3:	pop	bc
		pop	de
		jr	z,_conbufin6		; z=line not changed, don't change buffer

		; copy line to buffer
_conbufin4:	ld	hl,(varBB80)		; end of buffer
_conbufin5:	inc	de
		ld	a,(de)
		ld	(hl),a
		call	K_CBUF_INC
		djnz	_conbufin5
		ld	a,(hl)
		ld	(hl),CR
		call	K_CBUF_INC
		ld	(varBB80),hl

		; partial line / pad buffer with CR's
_conbufin6:	ld	(varBB82),HL		; current line start
_conbufin7:	cp	CR
		ret	z
		ld	a,(hl)
		ld	(hl),CR
		call	K_CBUF_INC
		jr	_conbufin7

		; previous line
go_up:		ld	a,(varBB7F)
		or	a
		jr	z,loop_line_in
		ld	hl,(varBB82)
_go_up1:	call	K_CBUF_DEC
		ld	a,(hl)
		cp	CR
		jr	z,_go_up1
_go_up2:	call	K_CBUF_DEC
		ld	a,(hl)
_go_up3:	cp	CR
		jr	nz,_go_up2
		call	K_CBUF_INC
		jr	update_line

		; next line
go_down:	ld	a,(varBB7F)
		or	a
		jr	z,loop_line_in
		ld	hl,(varBB82)
_go_down1:	ld	a,(hl)
		cp	CR
		call	K_CBUF_INC
		jr	nz,_go_down1
		scf
_go_down2:	call	NC,K_CBUF_INC
		ld	a,(hl)
		cp	CR
		jr	z,_go_down2

		; update current line
update_line:	ld	(varBB82),hl
		push	de
		ld	a,(de)
		ld	b,a
		inc	de
		inc	de
		ld	c,$ff
update_line1:	ld	a,(hl)
		ld	(de),a
		inc	c
		call	K_CBUF_INC
		cp	CR
		inc	de
		jr	z,update_line2
		djnz	update_line1
		inc	c
update_line2:	pop	de
		inc	de
		ld	a,c
		ld	(de),a
		dec	de
		ld	(varBB7C),a
		jp	loop_line_in

; ---------------------------------------------------------
; Subroutine next position in history buffer
; Input:  HL = pointer in history buffer
; Output: HL = updated pointer in history buffer
; ---------------------------------------------------------
K_CBUF_INC:	push	af
		push	de
		ld	de,varB1CF
		or	a
		sbc	hl,de
		add	hl,de
		inc	hl
		jr	nz,_cbufinc1
		ld	hl,varB0D0
_cbufinc1:	pop	de
		pop	af
		ret

; ---------------------------------------------------------
; Subroutine previous position in history buffer
; Input:  HL = pointer in history buffer
; Output: HL = updated pointer in history buffer
; ---------------------------------------------------------
K_CBUF_DEC:	push	af
		push	de
		ld	de,varB0D0
		or	a
		sbc	hl,de
		add	hl,de
		dec	hl
		jr	nz,_cbufdec1
		ld	hl,varB1CF
_cbufdec1:	pop	de
		pop	af
		ret

; ---------------------------------------------------------
; Subroutine edit line
; Input:  DE = pointer to buffer
; ---------------------------------------------------------
K_EDIT_LINE:	ld	hl,(varBB8B)			; current column
		ld	(varBB87),hl
		ld	(varBB7D),hl			; max curpos
		ex	de,hl
_editline1:	ld	c,(hl)				; buffer size
		inc	hl
		ld	(varBB84),hl			; character counter
		ld	a,(hl)
		or	a				; is buffer prefilled with text?
		ld	b,a
		jr	z,_editline2			; z=no
		inc	hl
		call	TextOut
		dec	hl
		ld	a,b				; character counter
_editline2:	ld	(varBB86),A
		xor	a				; set overwrite mode
		call	InsMode
edit_loop:	ld	de,edit_loop
		push	de				; set edit_loop as return address
		push	hl
		ld	hl,varBB86
		ld	a,(hl)
		cp	b				; buffer counter > total chars in buffer?
		jr	nc,J05B9			; nc=yes, skip update total buffer counter
		ld	(hl),b
J05B9:		pop	hl
		call	CharIn				; get character from keyboard
		cp	LF
		ret	z
		cp	CR
		jp	z,edit_end
		cp	$1d				; joystick left
		jp	z,joy_left
		cp	$1c				; joystick right
		jp	z,joy_right
		cp	DEL
		jp	z,key_del
		cp	BS
		jp	z,key_bs
		cp	INS
		jp	z,key_ins
		cp	ESC
		jr	z,jp_clear_line
		cp	$18				; ctrl-x
		jr	z,jp_clear_line
		cp	$15				; ctrl-u
jp_clear_line:	jp	z,clear_line
		cp	$1e				; up
		jp	z,key_up
		cp	$1f				; down
		jp	z,key_down
		cp	$0b				; home
		jp	z,key_home
		ld	e,a				; save character
		ld	a,(varBB7B)			; insert flag
		or	a
		jp	nz,insert_char
		ld	a,(varBB86)
		cp	b				; add character at end?
		jr	z,add_char			; z=yes
		inc	hl
new_char:	ld	a,(hl)
		cp	SPACE
		jr	c,insert_char6
		ld	a,e
		cp	SPACE
		jr	c,insert_char6
		jr	add_char1

add_char:	cp	c
		jr	nc,line_full
		inc	hl
add_char1:	ld	(hl),e
		ld	a,e
		inc	b
		call	CharConOut
		jp	UpdMaxColumn

insert_char:	ld	a,(varBB86)
		cp	c
		jr	nc,line_full
		inc	a
		ld	(varBB86),a
		dec	a
		sub	b
		jr	z,insert_char5
		push	de
		push	bc
		ld	c,a
		ld	b,0
		add	hl,bc
		ld	d,h
		ld	e,l
		inc	de
		lddr
		pop	bc
		pop	de
insert_char5:	inc	hl
insert_char6:	ld	(hl),e
		call	line_refresh
		inc	b
		jp	CurPos

line_full:	ld	a,BELL
		push	bc
		push	hl
		call	KB_CHAROUT
		pop	hl
		pop	bc
		ret

; Subroutine: INS key, toggle insert mode and update cursor shape
key_ins:	ld	a,(varBB7B)
		cpl

; Subroutine: set insert mode and update cursor shape
InsMode:	ld	(varBB7B),a
		or	a
		ld	a,$79
		jr	nz,_insmode1
		dec	a
_insmode1:	push	bc
		push	hl
		push	de
		push	af
		ld	a,$1b
		call	KB_CHAROUT
		pop	af
		call	KB_CHAROUT
		ld	a,$34
		call	KB_CHAROUT
		pop	de
		pop	hl
		pop	bc
		ret

joy_right:	ld	a,(varBB86)
		cp	b
		ret	z
		inc	hl
		inc	b
		ld	a,(hl)
		jp	CharConOut

key_home:	ld	a,b
		or	a
		ret	z
		ld	b,$00
		jr	CurPos

joy_left:	ld	a,b
		or	a
		ret	z
		dec	b

; Subroutine cursor to position
; Input:  B = position
CurPos:		ld	hl,(varBB84)
		ld	de,(varBB87)
		push	bc
		inc	b
		jr	CurPos6

CurPos1:	inc	hl
		ld	a,(hl)
		cp	TAB
		jr	nz,CurPos3
		ld	a,e
		or	$07
		ld	e,a
		jr	CurPos5

CurPos3:	cp	SPACE
		jr	nc,CurPos5
CurPos4:	inc	de
CurPos5:	inc	de
CurPos6:	djnz	CurPos1
		push	hl
		ld	hl,(varBB8B)
		or	a
		sbc	hl,de
		jr	z,CurPos9
CurPos8:	ld	a,BS
		call	CharConOut
		dec	hl
		ld	a,h
		or	l
		jr	nz,CurPos8
CurPos9:	pop	hl
		pop	bc
		ret

key_bs:		ld	a,b
		or	a
		ret	z
		dec	b
		call	CurPos
key_del:	ld	a,(varBB86)
		cp	b
		ret	z
		dec	a
		ld	(varBB86),a
		sub	b
		jr	z,key_del2
		push	bc
		push	hl
		ld	c,a
		ld	b,0
		inc	hl
		ld	d,h
		ld	e,l
		inc	hl
		ldir
		pop	hl
		pop	bc
key_del2:	inc	hl
		call	line_refresh
		dec	hl
		jp	CurPos

; Subroutine: delete line on screen
line_delete:	xor	a
		cp	b
		ld	b,a
		call	nz,CurPos
		call	clear_to_end
		ld	b,$00
		call	CurPos
		ld	hl,(varBB84)
		ld	(hl),$00
		dec	hl
		ret

clear_line:	call	line_delete
		pop	de
		jp	_editline1

edit_end:	inc	hl
		call	line_refresh
		ld	hl,(varBB84)
		ld	a,(varBB86)
		ld	(hl),a
		xor	a
		call	InsMode
		ld	a,CR
		call	CharConOut
		pop	hl
		xor	a
		ret

key_up:		pop	hl
		call	line_delete
		ld	a,1
		ret

key_down:	pop	hl
		call	line_delete
		ld	a,2
		ret

; Subroutine clear rest of line on screen
clear_to_end:	push	bc
		push	de
		push	hl
		jr	_refresh1

; Subroutine refresh line on screen
; Input:  B = current position
line_refresh:	push	bc
		push	de
		push	hl
		ld	a,(varBB86)
		sub	b
		ld	b,a
		call	TextOut
_refresh1:	ld	de,(varBB8B)
		ld	hl,(varBB7D)
		or	a
		sbc	hl,de
		jr	z,_refresh3
		jr	c,_refresh3
_refresh2:	ld	a,SPACE
		call	CharConOut
		dec	hl
		ld	a,h
		or	l
		jr	nz,_refresh2
		ld	(varBB7D),de
_refresh3:	ld	a,ESC
		call	KB_CHAROUT
		ld	a,$4b
		call	KB_CHAROUT
		pop	hl
		pop	de
		pop	bc
		ret

; Subroutine: text to console, update maximum column position
; Input:  HL = pointer to text
;         B  = size of text
TextOut:	push	bc
		inc	b
		jr	_textout2
_textout1:	ld	a,(hl)
		call	CharConOut
		call	UpdMaxColumn
		inc	hl
_textout2:	djnz	_textout1
		pop	bc
		ret

; Subroutine update maximum column position
UpdMaxColumn:	push	hl
		push	bc
		ld	hl,(varBB7D)
		ld	bc,(varBB8B)
		or	a
		sbc	hl,bc
		jr	nc,_maxcolumn1
		ld	(varBB7D),bc
_maxcolumn1:	pop	bc
		pop	hl
		ret

; Subroutine character to console (make control character printable, allow force to screen)
CharConOut:	push	bc
		push	de
		push	hl
		call	ValCharType		; control character?
		jr	nc,CharConOut1		; nc=no
		push	af
		ld	a,'^'
		call	CharConOut2
		pop	af
		add	a,$40			; convert ctrl code to character
CharConOut1:	call	CharConOut2
		pop	hl
		pop	de
		pop	bc
		ret

; Subroutine character to console (allow force to screen)
CharConOut2:	ld	b,a
		ld	a,(varBB7A)
		or	a
		ld	a,b
		jp	z,ConOut1
		jp	CharOut

; Subroutine: validate type of character
; Input:  a  = Character
; Output: zx = set: CR,LF,TAB,BS,DEL
;         cx  = set: control character not set: printable character
ValCharType:	cp	CR
		ret	z
		cp	LF
		ret	z
		cp	TAB
		ret	z
		cp	BS
		ret	z
		cp	DEL
		ret	z
		cp	SPACE
		ret

; Subroutine character to console (with console status, redirection support and TAB translation)
ConOut:		push	af
		call	ConStat
		pop	af

; Subroutine character to console (with redirection support and TAB translation)
ConOut1:	cp	TAB
		jr	nz,ConOut2
_tab1:		ld	a,SPACE
		call	ConOut2
		ld	a,(varBB8B)		; current column
		and	$07			; multiple of 8?
		jr	nz,_tab1		; nz=no
		ret

; Subroutine character to console (with redirection support)
ConOut2:	ld	hl,(varBB8B)
		call	UpdColumn
		ld	(varBB8B),hl
		bit	1,(iy+9)		; is standard output redirected?
		jp	z,CharOut2		; z=no, send char to screen
		ld	c,$ff
		jp	K_HCONOUT

; Subroutine get console status
ConStat:	call	KB_CHARSTAT
		ld	b,a
		ld	a,(varBB8D)
		or	a
		jr	nz,_constat1
		ld	a,b
		or	a
		ret	z
		call	KB_CHARIN
		call	ValInput
		or	a
		ret	z
		ld	(varBB8D),a
_constat1:	xor	a
		dec	a
		ret

; Subroutine: get input from keyboard
CharIn:		ld	a,(varBB8D)		; character buffer
		ld	(iy+13),$00		; clear character buffer (varBB8D)
		or	a			; use character in buffer?
		ret	nz			; nz=yes
_charin1:	call	KB_CHARIN		; get character from BIOS
		call	ValInput		; validate input
		or	a			; is control character?
		jr	z,_charin1		; z=yes
		ret

; Subroutine: handle special keys
ValInput:	cp	$10			; CTRL-P ?
		jr	z,ctrlp_in		; z=yes, enable printer echo
		cp	$0e			; CTRL-N ?
		jr	z,ctrln_in		; z=yes, disable printer echo
		cp	$03			; CTRL-C ?
		jr	z,ctrlc_in		; z=yes, abort program
		cp	$13			; CTRL-S ?
		ret	nz			; nz=no
		call	KB_CHARIN		; wait for another character from the BIOS
		cp	$03			; CTRL-C ?
		ld	a,$00
		ret	nz
ctrlc_in:	ld	a,_CTRLC
		ld	b,$00
		call	sysTpaAbort		; abort program (don't return)
		jr	$			; loop forever

ctrlp_in:	ld	a,$ff
		db	$fe			; opcode 'cp nn'
ctrln_in:	xor	a
		ld	(varBB8A),a		; update printer echo flag
		xor	a
		ret

; Subroutine character to screen (with TAB translation)
CharOut:	cp	TAB
		jr	nz,CharOut1
_chartab1:	ld	a,SPACE
		call	CharOut1
		ld	a,(varBB8B)
		and	$07
		jr	nz,_chartab1
		ret

; Subroutine character to screen
CharOut1:	ld	hl,(varBB8B)
		call	UpdColumn
		ld	(varBB8B),hl
CharOut2:	call	KB_CHAROUT
		ld	hl,varBB8A
		bit	0,(hl)			; echo to printer enabled?
		ret	z			; z=no
		ld	e,a
		jp	F_LSTOUT

; Subroutine update console column position
UpdColumn:	inc	hl
		cp	DEL
		jr	z,_updcolumn1
		cp	SPACE			; control character?
		ret	nc			; nc=no
_updcolumn1:	dec	hl
		ld	b,a
		ld	a,h
		or	l
		ld	a,b
		ret	z
		dec	hl
		cp	BS
		ret	z
		cp	DEL
		ret	z
		inc	hl
		cp	CR
		ret	nz
		ld	hl,0
		ret

; ---------------------------------------------------------
; *** Character devices ***
; ---------------------------------------------------------

DEV_CON:	jp	ConReadChar
		jp	ConWriteChar
		jp	ConReadStat
		jp	NulStat
		jp	ConScreenSize

; CON device input handler
ConReadChar:	bit	5,c			; binary mode?
		jr	nz,_conreadchar1	; nz=no, read in char in ascii mode
		call	KB_CHARIN
		ld	b,a
		xor	a
		ret

_conreadchar1:	ld	hl,(varBB78)
		ld	a,(hl)
		or	a
		jr	nz,_conreadchar2
		ld	de,varB1D0
		ld	a,$ff
		ld	(de),a
		ld	a,$ff
		call	K_CON_BUFIN
		ld	a,LF
		call	CharOut
		ld	hl,varB1D1
		ld	e,(hl)
		ld	d,$00
		inc	hl
		ex	de,hl
		add	hl,de
		ld	(hl),CR
		inc	hl
		ld	(hl),LF
		inc	hl
		ld	(hl),$00
		ex	de,hl
		ld	a,(hl)
		cp	CTRL_Z
		jr	z,_conreadchar3
_conreadchar2:	inc	hl
		ld	(varBB78),hl
		ld	b,a
		cp	LF
		ld	a,_EOL			; end of line
		ret	z
		xor	a
		ret

_conreadchar3:	ld	b,a
		ld	(hl),$00
		ld	(varBB78),hl
		ld	a,_EOF			; end of file
		ret

; CON device output handler
ConWriteChar:	bit	5,c
		jr	nz,_conwritechar1
		call	KB_CHAROUT
		xor	a
		ret

_conwritechar1:	push	af
		call	ConStat
		pop	af
		call	CharOut
		xor	a
		ret

; CON device check if input ready handler
ConReadStat:	bit	5,c				; binary mode?
		jr	nz,_conreadstat1		; nz=no
		call	KB_CHARSTAT
		ld	e,a
		xor	a
		ret

_conreadstat1:	call	ConStat
		ld	e,a
		xor	a
		ret

; CON device get screen size handler
ConScreenSize:	call	KB_SCREENSIZE
		xor	a
		ret

; Subroutine clear line input buffer
conClearBuf:	ld	hl,varB1D2
		ld	(varBB78),hl
		ld	(hl),$00
		ret

; ---------------------------------------------------------
; AUX device jumptable
; ---------------------------------------------------------
DEV_AUX:	jp	AuxReadChar
		jp	AuxWriteChar
		jp	NulStat
		jp	NulStat
		jp	NulScreenSize

; AUX device input handler
AuxReadChar:	call	KB_AUXIN
		ld	b,a
		cp	CTRL_Z
		jr	z,read_eof
		cp	CR
		ld	a,_EOL
		ret	z
		xor	a
		ret

; AUX device output handler
AuxWriteChar:	call	KB_AUXOUT
		xor	a
		ret

; ---------------------------------------------------------
; LST/PRN device jumptable
; ---------------------------------------------------------
DEV_LST:	jp	NulReadChar
		jp	LstWriteChar
		jp	NulStat
		jp	LstStat
		jp	NulScreenSize

; LST/PRN device output handler
LstWriteChar:  	call	KB_LPTOUT
		jr	nc,NulWriteChar
		res	0,(iy+10)
		ld	a,_STOP
		ret

; LST/PRN device check if output ready handler
LstStat:	call	KB_LPTSTAT
		ld	e,a
		xor	a
		ret

; ---------------------------------------------------------
; NUL device jumptable
; ---------------------------------------------------------
DEV_NUL:	jp	NulReadChar
		jp	NulWriteChar
		jp	NulStat
		jp	NulStat
		jp	NulScreenSize

NulReadChar:	ld	b,CTRL_Z
read_eof:	ld	a,_EOF
		ret

NulScreenSize:	ld	de,0
NulWriteChar:	xor	a
		ret

NulStat:	ld	e,$ff
		xor	a
		ret

; ---------------------------------------------------------
; *** BIOS calls ***
; ---------------------------------------------------------

; Subroutine get screen size
KB_SCREENSIZE:	ld	a,(LINLEN)
		ld	e,a
		ld	a,(CRTCNT)
		ld	hl,CNSDFG
		add	a,(hl)
		ld	d,a
		ret

; Subroutine get character from keyboard
KB_CHARIN:	push	ix
		ld	ix,CHGET
		call	K_BIOS
		call	KB_CHECK_STOP
		pop	ix
		ret

; Subroutine get status from keyboard
KB_CHARSTAT:	ld	hl,ST_COU
		dec	(hl)
		jr	nz,_charstat1
		inc	(hl)
		ld	a,(CH_COU)
		dec	a
		push	ix
		ld	ix,CHSNS
		call	K_BIOS
		call	KB_CHECK_STOP
		pop	ix
		ld	a,$ff
		ret	nz
		ld	a,$65
		ld	(ST_COU),a
_charstat1:	xor	a
		ret

KB_CHAROUTC:	ld	a,c

; Subroutine output character to screen
KB_CHAROUT:	push	ix
		ld	ix,CHPUT
		call	K_BIOS
		pop	ix
		ret

; Subroutine output character to printer
KB_LPTOUT:	push	ix
		ld	ix,LPTOUT
		call	K_BIOS
		pop	ix
		ret

; Subroutine get printer status
KB_LPTSTAT:	push	ix
		ld	ix,LPTSTT
		call	K_BIOS
		pop	ix
		ret

; Subroutine get character from AUX device
KB_AUXIN:	ld	hl,SAUXIN
		jp	sysTpaCall

KB_AUXOUTC:	ld	a,c

; Subroutine output character to AUX device
KB_AUXOUT:	ld	hl,SAUXOUT
		jp	sysTpaCall

; Subroutine check and handle CTRL-STOP
KB_CHECK_STOP:	push	af
		ld	a,(INTFLG)
		sub	$03
		jr	z,_checkstop1
		pop	af
		ret

_checkstop1:	ld	(INTFLG),a
		ld	ix,KILBUF
		call	K_BIOS
		ld	a,_STOP
		ld	b,$00
		call	sysTpaAbort
		jr	$			; loop forever

; ---------------------------------------------------------
; Subroutine call main-bios
; ---------------------------------------------------------
K_BIOS:		ex	af,af'
		exx
		push	af
		push	bc
		push	de
		push	hl
		push	iy
		exx
		ex	af,af'
		ld	iy,(EXPTBL-1)
		call	CALSLT
		ei
		ex	af,af'
		exx
		pop	iy
		pop	hl
		pop	de
		pop	bc
		pop	af
		exx
		ex	af,af'
		ret

; ---------------------------------------------------------
; Subroutine: write character to file handle
; ---------------------------------------------------------
WriteCharH:	ex	af,af'
		call	fhsPtrFIB
		ret	nc
		ret	z
		bit	7,(ix+30)
		jr	z,_writechar1
		ld	l,(ix+28)
		ld	h,(ix+29)
		inc	hl
		inc	hl
		inc	hl
		ex	af,af'
jphl:		jp	(hl)

_writechar1:	ex	af,af'
		ld	de,varBBC5
		ld	(de),a
		ld	bc,$0001
		ld	a,$ff
		jp	dskWriteFIB

; ---------------------------------------------------------
; Subroutine: read character from file handle
; ---------------------------------------------------------
ReadCharH:	call	fhsPtrFIB
		ret	nc
		ret	z
		bit	7,(ix+30)
		jr	z,_readchar2
		res	6,(ix+30)
		ld	l,(ix+28)
		ld	h,(ix+29)
		push	bc
		call	jphl
		pop	de
		cp	$b9
		jr	z,_readchar1
		bit	5,e
		ret	nz
		cp	$c7
		ret	nz
_readchar1:	xor	a
		ret

_readchar2:	push	bc
		ld	de,varBBC5
		ld	bc,$0001
		ld	a,$ff
		call	dskReadFIB
		ld	hl,varBBC5
		ld	b,(hl)
		pop	de
		or	a
		ret	nz
		or	e
		ret	z
		ld	a,b
		cp	1ah
		ld	a,$c7
		ret	z
		xor	a
		ret
