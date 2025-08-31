; ------------------------------------------------------------------------------
; cs1_msg.asm
; DOS system and error messages.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION	CS1_MSG

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	msgGetError		; sys
		PUBLIC	msgGetMessage		; sys,ipl

; ------------------------------------------------------------------------------
; MACRO

DOSST1		MACRO	X,Y
		LOCAL	_D1
		db	X
		db	_D1-$
		db	Y
_D1:		db	0
		ENDM

; ------------------------------------------------------------------------------
msgGetError:	ld	hl,ErrorMsg
		call	GetMsg
		ret	z
		push	af
		cp	64
		ld	a,11			; system error
		jr	nc,_geterror1
		ld	a,12			; user error
_geterror1:	call	msgGetMessage
		pop	af
		ret

; Subroutine
; ref: C4C1E
msgGetMessage:	ld	hl,SystemMsg

; Subroutine
GetMsg:		push	bc
		push	de
		ld	e,a
_getmsg1:	ld	a,(hl)
		or	a
		ld	a,e
		jr	z,_getmsg3
		sub	(hl)
		inc	hl
		ld	c,(hl)
		ld	b,$00
		inc	hl
		jr	z,_getmsg2
		add	hl,bc
		jr	_getmsg1
_getmsg2:	pop	de
		push	de
		ldir
		ex	de,hl
_getmsg3:	pop	de
		pop	bc
		or	a
		ret

; ------------------------------------------------------------------------------
; *** Messages ***
; ------------------------------------------------------------------------------

; ref: I4C4A
SystemMsg:	DOSST1	1,"Not enough memory error "
		DOSST1	2,"Drive name? ("
		DOSST1	3,") "
		DOSST1	4,"Strike a key when ready "
		DOSST1	5,"Aborted"
		DOSST1	6,"Format complete"
		DOSST1	7,"Insert disk for drive "
		DOSST1	8,":"
		DOSST1	9,"and strike a key when ready "
		DOSST1	10,"*** "
		DOSST1	11,"System error "
		DOSST1	12,"User error "
		db	0

; ref: I4D0E
ErrorMsg:	DOSST1	$ff,"Incompatible disk"
		DOSST1	$fe,"Write error"
		DOSST1	$fd,"Disk error"
		DOSST1	$fc,"Not ready"
		DOSST1	$fb,"Verify error"
		DOSST1	$fa,"Data error"
		DOSST1	$f9,"Sector not found"
		DOSST1	$f8,"Write protected disk"
		DOSST1	$f7,"Unformatted disk"
		DOSST1	$f6,"Not a DOS disk"
		DOSST1	$f3,"Seek error"
		DOSST1	$f2,"Bad file allocation table"
		DOSST1	$df,"Internal error"
		DOSST1	$de,"Not enough memory"
		DOSST1	$dc,"Invalid DOS call"
		DOSST1	$db,"Invalid drive"
		DOSST1	$da,"Invalid filename"
		DOSST1	$d9,"Invalid pathname"
		DOSST1	$d8,"Pathname too long"
		DOSST1	$d7,"File not found"
		DOSST1	$d6,"Directory not found"
		DOSST1	$d5,"Root directory full"
		DOSST1	$d4,"Disk full"
		DOSST1	$d3,"Duplicate filename"
		DOSST1	$d2,"Invalid directory move"
		DOSST1	$d1,"Read only file"
		DOSST1	$d0,"Directory not empty"
		DOSST1	$cf,"Invalid attributes"
		DOSST1	$ce,"Invalid . or .. operation"
		DOSST1	$cd,"System file exists"
		DOSST1	$cc,"Directory exists"
		DOSST1	$cb,"File exists"
		DOSST1	$ca,"File is already in use"
		DOSST1	$c9,"Cannot transfer above 64k"
		DOSST1	$c8,"File allocation error"
		DOSST1	$c7,"End of file"
		DOSST1	$c6,"File access violation"
		DOSST1	$c5,"Invalid process id"
		DOSST1	$c4,"No spare file handles"
		DOSST1	$c3,"Invalid file handle"
		DOSST1	$c2,"File handle not open"
		DOSST1	$c1,"Invalid device operation"
		DOSST1	$c0,"Invalid environment string"
		DOSST1	$bf,"Environment string too long"
		DOSST1	$be,"Invalid date"
		DOSST1	$bd,"Invalid time"
		DOSST1	$ba,"File handle has been deleted"
		DOSST1	$b8,"Invalid sub-function number"
		DOSST1	$b7,"Invalid File Control Block"
		DOSST1	$9f,"Ctrl-STOP pressed"
		DOSST1	$9e,"Ctrl-C pressed"
		DOSST1	$9d,"Disk operation aborted"
		DOSST1	$9c,"Error on standard output"
		DOSST1	$9b,"Error on standard input"
		DOSST1	$8f,"Wrong version of command"
		DOSST1	$8e,"Unrecognized command"
		DOSST1	$8d,"Command too long"
		DOSST1	$8b,"Invalid parameter"
		DOSST1	$8a,"Too many parameters"
		DOSST1	$89,"Missing parameter"
		DOSST1	$88,"Invalid option"
		DOSST1	$87,"Invalid number"
		DOSST1	$86,"File for HELP not found"
		DOSST1	$85,"Wrong version of DOS"
		DOSST1	$84,"Cannot concatenate destination file"
		DOSST1	$83,"Cannot create destination file"
		DOSST1	$82,"File cannot be copied onto itself"
		DOSST1	$81,"Cannot overwrite previous destination file"
		DOSST1	$7f,"Insert DOS disk in drive \x07:"
		DOSST1	$7e,"Press any key to continue... "
		db	0
