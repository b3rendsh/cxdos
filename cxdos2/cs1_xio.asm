; ------------------------------------------------------------------------------
; cs1_xio.asm
; System i/o module, replaces msxdos2.sys.
;
; (c) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		SECTION CS1_XIO

		INCLUDE	"cxdos.inc"		; CXDOS constants and definitions

		PUBLIC	xioBegin
		PUBLIC	xioEnd

		EXTERN	KB_CHARIN
		EXTERN	KB_CHAROUTC
		EXTERN	KB_CHARSTAT
		EXTERN	KB_LPTOUT
		EXTERN	KB_LPTSTAT
		EXTERN	KB_AUXOUTC
		EXTERN	KB_AUXIN

xioBegin:
		PHASE	XIOBASE		; This must be at a 256 byte page boundary

; ------------------------------------------------------------------------------
; XIO variable space (256 bytes)
; ------------------------------------------------------------------------------

JBDOS:		defs	5,0
		db	22		; cp/m version number (decimal value!)
		jp	XBDOS
DSK_ERRV:	dw	0
BRK_ERRV:	dw	0
ERROR2:		db	0
ERROR1:		db	0
DISK_VECT:	dw	0
BIOS_SP:	dw	0
BDOS_SP:	dw	0
ENTERS:		db	0
SP_SAVE:	dw	0
		db	$c3		; opcode 'jp ####'
ABORT_VECT:	dw	0

; CP/M compatibility vectors
CPMVEC:		jp	BiosTab+3	; reboot entry
		db	0		; i/o byte (not used)
		db	0		; current drive
		jp	JBDOS+6		; BDOS entry

; A: is patched to the boot drive.
AutoCmd:	db	18,"A:\\AUTOEXEC.BAT "
AutoPat:	db	"A:",0
RebootCmd:	db	16,"A:\\REBOOT.BAT "
RebootPat:	db	"A:",0
CommandM:	db	"A:\\COMMAND2.COM",0

; short version info (signon message is moved to ipl module)
	IFDEF DEBUG
VerTxt:		db	"CXDOS version 2.00",MOD1,MOD2," (TEST)",0
	ELSE
VerTxt:		db	"CXDOS version 2.00",MOD1,MOD2,0
	ENDIF

		defs	XIOBASE+$100-$,0	; space for stack (at least 128 bytes)
BIOS_STACK:

; ------------------------------------------------------------------------------
; BIOS jump table
; ------------------------------------------------------------------------------
BiosTab:	jp	Startup		; initial start-up entry point
JpReboot:	jp	Reboot		; reload COMMAND2.COM
		jp	ConStat		; console status
		jp	ConIn		; console input
		jp	ConOut		; console output
		jp	LstOut		; printer output
		jp	AuxOut		; auxiliary output
		jp	AuxIn		; auxiliary input
		jp	DiskRet		; not implemented, return error if possible
		jp	DiskRet
		jp	DiskRet
		jp	DiskRet
		jp	DiskRet
		jp	DiskRet
		jp	DiskRet
		jp	LstStat		; printer status
		jp	DiskRet
		dw	VerTxt		; pointer to version message

Startup:	ld	hl,$0000		; zero main and secondary errors returned by process
		ld	(ERROR2),hl
		ld	a,(BOOT_D)		; set the correct drive letter in the boot command strings
		add	a,'A'-1
		ld	(AutoPat),a
		ld	(RebootPat),a
		ld	(AutoCmd+1),a
		ld	(RebootCmd+1),a
		ld	(CommandM),a

		ld	de,$0080		; command line buffer
		ld	a,(de)
		or	a			; are there any commandline parameters?
		jr	nz,Boot			; nz=yes (e.g. call system from BASIC)

		ld	hl,NOTFIR		; not the first boot flag
		ld	a,(hl)
		ld	(hl),h			; set the flag (not zero)

		ld	hl,AutoCmd
		or	a			; first boot?
		jr	z,CopyCmd		; z=yes

		; Copy to commandline buffer: $12,"A:\AUTOEXEC.BAT A:",$00
		ld	hl,RebootCmd
CopyCmd:	ld	c,(hl)			; get length of command
		inc	c			; include length byte
		inc	c			; include null on end
		ld	b,$00			; make sure that B=0
		ldir
		jr	Boot

Reboot:		xor	a			; reset command buffer
		ld	($0080),a

Boot:		call	SDOSOF			; disable disk rom / enable ram on page 1
		ld	a,(RAMAD2)
		ld	h,$80
		call	ENASLT			; enable ram on page 2

		ld	hl,DISK_ERROR		; set disk error vector
		ld	(KDSK_V),hl
		ld	hl,ABORT		; set abort vector
		ld	(KAB_VE),hl

		ld	de,$0000		; reset user error/abort routines
		call	XDEFER
		call	XDEFAB

		ld	hl,Reboot		; set Reboot entry in BIOS jump table to initial value
		ld	(JpReboot+1),hl
		ld	a,$ff			; set BDOS re-entrancy count
		ld	(ENTERS),a

		; set jump vectors in cp/m scratch area (page 0)
		ld	hl,CPMVEC
		ld	de,$0000
		ld	bc,$0008
		ldir

		ld	sp,BIOS_STACK		; use own stack in page 3
		ld	c,FCURDRV		; get the current drive and put it at address $0004 for CP/M compatibility
		call	XBDOS
		ld	($0004),a

		; check COMMAND error conditions which don't require reload
		ld	a,(ERROR1)
		cp	_BADCM
		jr	z,LoadOk
		cp	_OKCMD
		jr	z,LoadOk
		cp	_BATEND
		jr	z,LoadOk

Rejoin:		ld	bc,0*256+FJOIN
		call	XBDOS
		jr	nz,Rejoin		; join may fail the first time

		or	b			; any error from previous process?
		jr	z,r001			; z=no

		call	ErrorMsg
		call	PrintCRLF
r001:		ld	de,CommandM		; 'A:\COMMAND2.COM'
		ld	c,FOPEN
		xor	a
		call	XBDOS
		jr	nz,LoadErr		; nz=error
		ld	de,$0100		; Load program here
		ld	hl,JBDOS-$100		; maximum load size
		push	hl
		push	bc			; save file handle
		ld	c,FREAD
		call	XBDOS
		pop	bc			; restore file handle.
		pop	de			; restore maximum number of bytes usable. ($C300)
		jr	nz,LoadErr		; nz=error
		sbc	hl,de			; maximum load size used? (should be no)
		ld	a,_NORAM
		jr	z,LoadErr		; z=load error
		ld	c,FCLOSE
		call	XBDOS
		jr	nz,LoadErr		; nz=error
		scf				; set LOAD_FLAG to $ff later

; COMMAND2.COM is loaded at $0100
LoadOk:		push	af			; Save LOAD_FLAG
		ld	hl,$5c			; Clear CP/M FCB
		ld	b,h			; set B=0
Zloop:		ld	(hl),h
		inc	l
		jp	p,Zloop			; zero until $0080, which is the command line
		ex	de,hl			; set default DMA address to $0080
		ld	c,FSETDTA
		call	XBDOS
		pop	af			; set LOAD_FLAG to $00 or $ff
		sbc	a,a
		ld	(LOAD_FLAG),a
		xor	a			; Put address $0000 on the stack as the
		ld	l,a			; return address for COMMAND2.COM
		ld	h,a
		push	hl
		ld	de,BiosTab-1		; set registers for cp/m compatibility
		ld	c,$ff
		ld	b,a
		jp	$0100			; Start COMMAND2.COM

		; ** end of startup **

; Reload of COMMAND2.COM failed
; Input: A = error code
LoadErr:	cp	_NOFIL			; 'file not found'
		ld	b,a
		call	nz,ErrorMsg		; nz=if other error than 'file not found' then print error message
		ld	c,FCURDRV		; get current drive
		call	XBDOS
		add	a,'A'			; convert to drive
		ld	(msgDrive+7),a		; insert in the message
		ld	de,msgInsert
		call	PrintStr
		ld	de,msgPause
		call	PrintStr
		call	InKey			; wait for a keypress..
		jp	Reboot			; ..and try again

;------------------------------------------------------------------------------
; BIOS character I/O routines
;------------------------------------------------------------------------------

ConStat:	ld	hl,KB_CHARSTAT
		jr	BIOS

ConIn:		ld	hl,KB_CHARIN
		jr	BIOS

ConOut:		ld	hl,KB_CHAROUTC
		jr	BIOS

LstOut:		ld	hl,KB_LPTOUT
		jr	BIOS

AuxOut:		ld	hl,KB_AUXOUTC
		jr	BIOS

AuxIn:		ld	hl,KB_AUXIN
		jr	BIOS

LstStat:	ld	hl,KB_LPTSTAT

BIOS:		ld	(BIOS_SP),sp		; save user's stack
		ld	sp,BIOS_STACK		; switch to BIOS stack
		call	GO_BIOS
		ld	sp,(BIOS_SP)		; restore user's stack.
		ret

;------------------------------------------------------------------------------
; Routine for disk related CP/M BIOS functions, which  are not  supported.
; They return errors where possible but otherwise just return.

DiskRet:	xor	a
		ld	l,a			; hl=0: error from seldsk function
		ld	h,a
		dec	a			; a=ff: error from read/write
		ret

;------------------------------------------------------------------------------
; Abort Routine

ABORT:		push	af
		ld	a,(ENTERS)
		inc	a
		jr	z,ABIOS

		ld	a,$ff
		ld	(ENTERS),a
		pop	af

		ld	sp,(SP_SAVE)		; restore stack pointer
		exx
		ex	af,af'
		pop	iy
		pop	ix
		pop	hl
		ld	(SP_SAVE),hl
		pop	bc
		pop	de
		pop	hl
		exx
		pop	af
		ex	af,af'
		jr	Abort2

ABIOS:		pop	af
		ld	sp,(BIOS_SP)

; Abort user's BDOS or BIOS call
Abort2:		push	hl
		push	af			; save error code
		ld	hl,(ABORT_VECT)		; get user's abort routine address
		ld	a,h
		or	l
		pop	hl			; h=error code
		ld	a,h			; af=error code.
		pop	hl
		call	nz,ABORT_VECT-1
		ld	(ERROR1),a
		ld	a,b
		ld	(ERROR2),a
		ld	a,(ERROR1)
		or	a
		jr	z,GoReboot
		ld	hl,(BREAKV)
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		jp	(hl)
GoReboot:	jp	$0000

;------------------------------------------------------------------------------
; Disk Error Handler
; Input: A  = error code.
;        B  = logical drive number.
;        C  = flags: b0 => read/write error.
;                   b1 => ignore OK/ ignore dangerous.
;                   b2 => normal/auto-abort error (eg. .IFAT)
;                   b3 => sector number in DE is valid.
;       DE = logical sector number or error (if bit 3,C=1).
;
; Output: A=1, 2 or 3 for Abort, Retry, Ignore (any other aborts).
;         A=0 => use the default error handling. (if user routine)
;------------------------------------------------------------------------------
DISK_ERROR:	push	af
		ex	af,af'
		push	af
		push	bc
		ld	hl,(DISK_VECT)
		ld	a,h
		or	l
		call	nz,UserDiskErr
		pop	bc
		ex	af,af'
		pop	af
		ex	af,af'
		pop	hl
		or	a
		ret	nz

; Pass control through old DISKVECT interface.
; Input: A' = Old error code (passed to DISKVECT in C)
;        H  = New error code (passed to DISKVECT in A')
;        B  = Drive number (decremented & passed to DISKVECT in A)
;        C  = New Flags (passed to DISKVECT in B)
;        DE = Sector number (passed to DISKVECT in DE)

		ld	a,h
		ex	af,af'
		push	bc
		ld	b,c
		ld	c,a
		ld	hl,(DISKVE)
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		pop	af
		dec	a
		call	JPHL
		ld	a,3
		sub	c
		ret

;------------------------------------------------------------------------------
; ** Subroutines **
;------------------------------------------------------------------------------

; Copies string or FIB at DE to BUF_1.
CopyDE:		ld	a,(de)
		inc	a
		jr	nz,Copy100

Copy64:		push	hl
		push	bc
		ex	de,hl
		ld	de,(BUF_1)
		ld	bc,64
		ldir
		pop	bc
		pop	hl
		scf
		ret

Copy100:	push	hl
		push	bc
		ex	de,hl
		ld	de,(BUF_1)
		ld	b,100
		call	CopyString
		pop	bc
		pop	hl
		ret

; Copies the string at HL to BUF_3
CopyHL:		push	de
		push	bc
		ld	de,(BUF_3)
		ld	b,100
		call	CopyString
		pop	bc
		pop	de
		ret

CopyString:	ld	a,(hl)
		inc	hl
		ld	(de),a
		inc	de
		or	a
		ret	z
		djnz	CopyString
		ld	a,_PLONG	; pathname too long
		ret

;------------------------------------------------------------------------------
; ** BDOS handler (jump from $0005) **
;------------------------------------------------------------------------------
XBDOS:		ld	(VAR_A),a		; save A register parameter for function
		ld	a,(ENTERS)		; get re-entrancy count
		inc	a			; first entry?
		ld	(ENTERS),a
		jr	nz,xbdos1		; nz=no
		ld	(LOAD_FLAG),a
		ld	(BDOS_SP),sp
		ld	sp,(ST_BDOS)
xbdos1:		ex	af,af'			; save alternate register because KBDOS doesn't
		push	af
		exx				; save BDOS parameters in alternate registers
		push	hl
		push	de
		push	bc
		ld	hl,(SP_SAVE)		; save SP_SAVE for re-entrancy reasons
		push	hl
		push	ix
		push	iy
		ld	(SP_SAVE),sp
		ld	hl,XBDOS_DONE		; set return address for KBDOS
		push	hl

		; dispatcher
		exx
		push	hl
		ld	a,c			; get function number
	IFNDEF OLD_XIO
		cp	$67			; function number in table?
		jr	c,xbdos2		; c=yes
		ld	a,$5d			; no wrapper / unknown function
xbdos2:		ld	hl,FNTAB		; function wrapper routine table
		add	l
		ld	l,a
		ld	l,(hl)
		inc	h
		ld	a,c
		cp	$59			; function wrappers span two 256-byte pages
		jr	c,xbdos2a
		inc	h
xbdos2a:
	ELSE
		cp	$67			; function number in table?
		jr	c,xbdos2		; c=yes
		xor	a			; no wrapper / unknown function
xbdos2:		ld	hl,FNTAB		; function wrapper routine table
		add	a,a
		add	a,l
		ld	l,a
		jr	nc,xbdos3		; nc=no crossing of 256-byte page boundary
		inc	h
xbdos3:		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
	ENDIF
		ex	(sp),hl
		ld	a,(VAR_A)
		ret				; jump to routine for this function

; Returning point from BDOS routine
XBDOS_DONE:	ld	sp,(SP_SAVE)		; restore stack pointer
		exx
		ex	af,af'
		pop	iy
		pop	ix
		pop	hl
		ld	(SP_SAVE),hl
		pop	bc
		pop	de
		pop	hl
		exx
		pop	af
		ex	af,af'
		push	af
		ld	a,(ENTERS)
		dec	a
		ld	(ENTERS),a
		JP	P,xbdos4
		pop	af
		ld	sp,(BDOS_SP)
		push	af
xbdos4:		pop	af
		or	a
		ret

	IFNDEF OLD_XIO

		ALIGN	XIOBASE+$400-$03-$67	; function alignment to 256 byte page boundary

		; 103 bytes (see assembler map)
FNTAB:		db	$07					; XIOBDOS
		db	$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a		; XIOCPM
		db	$21					; XSTROUT
		db	$24					; XBUFIN
		db	$0a,$0a					; XIOCPM
		db	$10					; XDSKRST
		db	$12					; XSELDSK
		db	$59,$59,$59				; XIOFCB33
		db	$0a					; XIOCPM
		db	$59,$59,$59,$59,$59			; XIOFCB33
		db	$0a,$0a,$0a				; XIOCPM
		db	$40					; XALLOC
		db	$0a,$0a,$0a,$0a,$0a			; XIOCPM
		db	$5c,$5c,$5c,$5c				; XIOFCB36
		db	$07					; XIOBDOS
		db	$7d,$7d					; XIOBLK
		db	$5c					; XIOFCB36
		db	$07					; XIOBDOS
		db	$98,$98,$98,$98				; XIOTIME
		db	$0a					; XIOCPM
		db	$07,$07					; XIOBDOS
		db	$9d					; XDPARM
		db	$07,$07,$07,$07,$07,$07,$07,$07		; XIOBDOS
		db	$07,$07,$07,$07,$07,$07
		db	$af					; XFFIRST
		db	$b6					; XFNEXT
		db	$af					; XFNEW
		db	$d8,$d8					; XIOFHS
		db	$07,$07,$07,$07,$07,$07,$07		; XIOBDOS
		db	$d8,$d8					; XIOFHS
		db	$d2,$d2					; XRENAME/XMOVE
		db	$d8,$d8					; XIOFHS
		db	$07					; XIOBDOS
		db	$f3,$f3					; XHRENAME/XHMOVE
		db	$07,$07,$07,$07				; XIOBDOS
		;
		db	$0b					; XGETCD
		db	$56					; XCHDIR
		db	$06					; XPARSE
		db	$2b					; XPFILE
		db	$03					; XIOKBDOS
		db	$0b					; XWPATH
		db	$03					; XIOKBDOS
		db	$4e					; XFORK
		db	$5b					; XJOIN
		db	$03					; XIOKBDOS
		db	$7d					; XDEFAB
		db	$8f					; XDEFER
		db	$03					; XIOKBDOS
		db	$69					; XEXPLAIN

	ELSE
;------------------------------------------------------------------------------
;Table of routine addresses for the functions.
FNTAB:		dw	XIOBDOS		; 00 Terminate program with no error
		dw	XIOCPM		; 01 Console input
		dw	XIOCPM		; 02 Console output
		dw	XIOCPM		; 03 Auxiliary input
		dw	XIOCPM		; 04 Auxiliary output
		dw	XIOCPM		; 05 List output
		dw	XIOCPM		; 06 Direct console I/O
		dw	XIOCPM		; 07 Direct console input, no echo
		dw	XIOCPM		; 08 Console input, no echo
		dw	XSTROUT		; 09 String output
		dw	XBUFIN		; 0A Buffered line input
		dw	XIOCPM		; 0B Console status
		dw	XIOCPM		; 0C Return CP/M version number
		dw	XDSKRST		; 0D Disk reset
		dw	XSELDSK		; 0E Select disk
		dw	XIOFCB33	; 0F Open file (FCB)
		dw	XIOFCB33	; 10 Close file (FCB)
		dw	XIOFCB33	; 11 Search for first (FCB)
		dw	XIOCPM		; 12 Search fir next (FCB)
		dw	XIOFCB33	; 13 Delete file (FCB)
		dw	XIOFCB33	; 14 Read sequential (FCB)
		dw	XIOFCB33	; 15 Write sequential (FCB)
		dw	XIOFCB33	; 16 Create file (FCB)
		dw	XIOFCB33	; 17 Rename file (FCB)
		dw	XIOCPM		; 18 Get login vector
		dw	XIOCPM		; 19 Get current drive
		dw	XIOCPM		; 1A Set disk transfer address
		dw	XALLOC		; 1B Get allocation information
		dw	XIOCPM		; 1C Write protect disk in CP/M
		dw	XIOCPM		; 1D Get read only vector in CP/M
		dw	XIOCPM		; 1E Set file attributes in CP/M
		dw	XIOCPM		; 1F Get Disk Parameter Block in CP/M
		dw	XIOCPM		; 20 Get/set user code in CP/M
		dw	XIOFCB36	; 21 Read random (FCB)
		dw	XIOFCB36	; 22 Write random (FCB)
		dw	XIOFCB36	; 23 Get file size (FCB)
		dw	XIOFCB36	; 24 Set random record (FCB)
		dw	XIOBDOS		; 25 Undefined function in CP/M
		dw	XIOBLK		; 26 Write random block (FCB)
		dw	XIOBLK		; 27 Read random block (FCB)
		dw	XIOFCB36	; 28 Write random with zero fill (FCB)
		dw	XIOBDOS		; 29 Unused
		dw	XIOTIME		; 2A Get date
		dw	XIOTIME		; 2B Set date
		dw	XIOTIME		; 2C Get time
		dw	XIOTIME		; 2D Set time
		dw	XIOCPM		; 2E Set/reset verify flag
		dw	XIOBDOS		; 2F Absolute sector read
		dw	XIOBDOS		; 30 Absolute sector write
		dw	XDPARM		; 31 Get disk parameters
		dw	XIOBDOS		; 32 Not implemented
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XIOBDOS
		dw	XFFIRST		; 40 Find first entry
		dw	XFNEXT		; 41 Find next entry
		dw	XFNEW		; 42 Find new entry
		dw	XIOFHS		; 43 Open file handle
		dw	XIOFHS		; 44 Create file and open file handle
		dw	XIOBDOS		; 45 Close file handle
		dw	XIOBDOS		; 46 Ensure file handle
		dw	XIOBDOS		; 47 Duplicate file handle
		dw	XIOBDOS		; 48 Read from file handle
		dw	XIOBDOS		; 49 Write to file handle
		dw	XIOBDOS		; 4A Seek (position file pointer)
		dw	XIOBDOS		; 4B I/O control for devices
		dw	XIOFHS		; 4C Test file handle
		dw	XIOFHS		; 4D Delete file or subdirectory
		dw	XRENAME		; 4E Rename file or subdirectory
		dw	XMOVE		; 4F Move file or subdirectory
		dw	XIOFHS		; 50 Get/set file attributes
		dw	XIOFHS		; 51 Get/set file time
		dw	XIOBDOS		; 52 Delete file handle
		dw	XHRENAME	; 53 Rename file handle
		dw	XHMOVE		; 54 Move file handle
		dw	XIOBDOS		; 55 Get/set file handle attributes
		dw	XIOBDOS		; 56 Get/set file handle time
		dw	XIOBDOS		; 57 Get disk transfer address
		dw	XIOBDOS		; 58 Get verify flag setting
		dw	XGETCD		; 59 Get current directory
		dw	XCHDIR		; 5A Set current directory
		dw	XPARSE		; 5B Parse path-name string
		dw	XPFILE		; 5C Parse filename
		dw	XIOKBDOS	; 5D Check character
		dw	XWPATH		; 5E Get whole path string
		dw	XIOKBDOS	; 5F Flush disk buffers
		dw	XFORK		; 60 Fork to process
		dw	XJOIN		; 61 Join to process
		dw	XIOKBDOS	; 62 Terminate with error code
		dw	XDEFAB		; 63 Define abort exit routine
		dw	XDEFER		; 64 Define critical error handler routine
		dw	XIOKBDOS	; 65 Get previous error code
		dw	XEXPLAIN	; 66 Explain error code
; Following functions have no wrapper
;		dw	XIOBDOS		; 67 Format a disk
;		dw	XIOBDOS		; 68 Create or destroy RAM-disk
;		dw	XIOBDOS		; 69 Allocate sector buffers
;		dw	XIOBDOS		; 6A Logical drive assignment
;		dw	XIOBDOS		; 6B Get environment string
;		dw	XIOBDOS		; 6C Set environment string
;		dw	XIOBDOS		; 6D Find environment name
;		dw	XIOBDOS		; 6E Get/set disk check status
;		dw	XIOBDOS		; 6F Get version number
;		dw	XIOBDOS		; 70 Get/set redirection status
	ENDIF

;------------------------------------------------------------------------------
; Allowed keys

keyAbort:	db	"Aa",$03,0	; $03=CTRL+C
keyRetry:	db	"Rr",0
keyIgnore:	db	"Ii",0

;------------------------------------------------------------------------------
; BDOS function calls

XIOBDOS:	jp	GO_BDOS

XIOCPM:		call	GO_BDOS
		ld	a,l
		ld	b,h
		ret

XDSKRST:	ld	e,0
XSELDSK:	push	de		; save drive
		call	GO_BDOS
		pop	de		; restore drive
		or	a
		ld	a,e
		jr	nz,bad_drive
		ld	(4),a		; store drive for cp/m compatibility
bad_drive:	ld	a,l
		ld	b,h
		ret

XSTROUT:	jp	SPRTBUF		; same as print buffer routine

XBUFIN:		push	de
		ld	a,(de)
		ld	de,(BUF_1)
		ld	(de),a
		call	GO_BDOS
		pop	de
		ld	a,(de)
		inc	de
		ld	hl,(BUF_1)
		inc	hl
		ld	c,a
		ld	b,$00
		inc	bc
		ldir
		xor	a
		ld	b,a
		ld	l,a
		ld	h,a
		ret

XALLOC:		exx
		pop	hl
		pop	de
		pop	de
		push	hl
		exx
		call	GO_BDOS
		exx
		pop	hl
		push	ix
		push	iy
		push	hl
		exx
		ld	a,c		; return sectors/cluster in a, not c
		ld	c,(IX+2)	; return sector size
		ld	b,(IX+3)
		ret

XIOFCB33:	ld	a,33		; ordinary FCBs are 33 bytes long
		db	$21		; opcode 'ld h,##' to skip next instruction

XIOFCB36:	ld	a,36		; random FCBs are 36 bytes long

		push	de
		exx
		pop	hl
		ld	c,a
		ld	b,0		; bc = number of bytes in FCB

RW:		push	hl
		push	bc
		ld	de,(BUF_1)
		push	de
		ldir
		exx
		pop	de
		push	de
		call	GO_BDOS
		exx
		pop	hl
		pop	bc
		pop	de
		ldir
		exx
		ld	a,l
		ld	b,h
		ret

XIOBLK:		push	de
		exx
		pop	hl
		push	hl
		ld	bc,14+1
		add	hl,bc
		ld	c,36
		ld	a,(hl)
		or	a
		jr	nz,xrdblk1
		dec	hl
		ld	a,(hl)
		cp	64
		jr	nc,xrdblk1
		inc	c
xrdblk1:	pop	hl
		call	RW
		ex	de,hl
		ret

XIOTIME:	call	GO_BDOS
		ld	a,c		; return day in A
		ret

XDPARM:		push	de
		ld	de,(BUF_1)
		call	GO_BDOS
		ex	de,hl
		pop	de
		push	de
		ld	bc,32
		ldir
		pop	de
		ret

XFFIRST:
XFNEW:		call	CopyDE
		call	c,CopyHL
		ret	nz

XFNEXT:		push	ix
		pop	hl
		push	hl
		ld	de,(BUF_2)
		push	bc
		ld	bc,64
		ldir
		pop	bc

		call	XKBDOS
		pop	de
		ld	hl,(BUF_2)
		ld	bc,64
		ldir
		ret

XRENAME:
XMOVE:		call	CopyHL
		ld	hl,(BUF_3)

XIOFHS:		ex	af,af'
		push	de
		call	CopyDE
		push	af
		ex	af,af'
		call	XKBSDE
		ex	af,af'
		exx
		pop	af
		pop	de
		jr	nc,XRET
		ld	hl,(BUF_1)
		ld	bc,64
		ldir
XRET:		ex	af,af'
		exx
		ret

XHRENAME:
XHMOVE:		ex	af,af'
		call	CopyHL
		ex	af,af'

XKBDOS:		ld	ix,(BUF_2)
		ld	hl,(BUF_3)
XKBSDE:		ld	de,(BUF_1)
XIOKBDOS:	jp	GO_BDOS

XPARSE:		push	de
		call	Copy64
		pop	de
XGETCD:
XWPATH:		ex	de,hl
		push	hl
		ld	de,(BUF_1)
		or	a
		sbc	hl,de
		push	hl
		push	de
		call	GO_BDOS
		exx
		pop	hl
		pop	bc
		pop	de
		push	bc
		ld	bc,64
		ldir
		exx
		ex	(sp),hl
		ex	de,hl
		add	hl,de
		ex	(sp),hl
		add	hl,de
		pop	de
		ret

XPFILE:		push	hl
		push	hl
		ld	l,e
		ld	h,d
		call	Copy64
		ld	de,(BUF_1)
		or	a
		sbc	hl,de
		ex	(sp),hl
		push	hl
		call	XKBDOS
		exx
		pop	de
		ld	bc,11
		ld	hl,(BUF_3)
		ldir
		exx
		pop	hl
		add	hl,de
		ex	de,hl
		pop	hl
		ret

XFORK:		ld	hl,0
		ld	(ERROR2),HL
		jr	XIOKBDOS

XCHDIR:		call	Copy64
		jr	XKBSDE

XJOIN:		CALL	GO_BDOS
		ld	bc,(ERROR2)
		ld	hl,0
		ld	(ERROR2),hl
		ret

XEXPLAIN:	push	de
		ld	de,(ERR_BUF)
		call	GO_BDOS
		ex	de,hl
		pop	de
		push	de
		push	bc
		ld	bc,64
		ldir
		pop	bc
		pop	de
		ret

XDEFAB:		ld	(ABORT_VECT),de
		xor	a

		ld	hl,GoReboot
		ld	(BRK_ERRV),hl
		ld	hl,BRK_ERRV
		ld	(BREAKV),hl
		ret

XDEFER:		ld	(DISK_VECT),de
		xor	a
		ld	hl,DSK_ERR
		ld	(DSK_ERRV),hl
		ld	hl,DSK_ERRV
		ld	(DISKVE),hl
		ret

;------------------------------------------------------------------------------
; Disk Error routine
; Input: A  = Drive number (0=A:,1=B: etc)
;        C  = Old error code
;        A' = New error code
;        B  = New Flags - b0 set => writing
;                         b1 set => ignore not recommended
;                         b2 set => auto-abort error
;                         b3 set => sector number is valid
;       DE = Sector number
;
; Output:  C = 0 => Ignore
;              1 => Retry
;              2 => Abort
;------------------------------------------------------------------------------
DSK_ERR:	ld	c,b
		inc	a
		ld	b,a
		push	bc
		ex	af,af'
		ld	b,a
		ex	af,af'
		ld	de,(ERR_BUF)
		ld	c,FEXPLAIN
		call	XBDOS
		call	PrintCRLF
		call	PrintStr
		pop	bc

		ld	de,msgReading
		bit	0,c		; reading or writing error?
		jr	z,rw_err	; z=reading
		ld	de,msgWriting
rw_err:		call	PrintStr
		ld	a,b
		add	a,'A'-1		; convert to upper case letter
		ld	(msgDrive+7),a	; insert in message
		ld	de,msgDrive
		call	PrintStr
		bit	2,c		; If an 'auto abort' error (eg. .IFAT) then
		ld	l,1		;   return A=1 for Abort without giving an
		jr	nz,ErrorReturn	;   Abort, Retry prompt.

ErrorLoop:	ld	de,msgAR
		bit	1,c
		jr	nz,MsgChoice
		ld	de,msgARI
MsgChoice:	call	PrintStr
		call	InKey		; Get a keypress in H.
		ld	l,0		; L=0 initially => abort.
		ld	de,keyAbort
		call	TestARI		; Test for abort character.
		ld	de,keyRetry
		call	nc,TestARI	; Test for retry character.
		ld	de,keyIgnore
		call	nc,TestARI	; Test for ignore character.
		jr	nc,ErrorLoop	; If invalid character, then re-print message.

ErrorReturn:	ld	a,3
		sub	l
		ld	c,a		; Return with the DOS1 compatible response in register c.
		ret

UserDiskErr:	ex	af,af'		; restore error code
JPHL:		jp	(hl)		; jump to user's disk error routine

TestARI:	inc	l		; Return with L incremented.
RepeatARI:	ld	a,(de)
		or	a
		ret	z		; Return with C clear if end of string.
		inc	de
		cp	h		; Character in string.
		jr	nz,RepeatARI
		scf
		ret

;------------------------------------------------------------------------------
; ** I/O Routines **
;------------------------------------------------------------------------------

ErrorMsg:	ld	de,(ERR_BUF)
		ld	c,FEXPLAIN
		call	XBDOS

PrintError:	push	de
		ld	de,msgSplats
		call	PrintStr
		pop	de

PrintStr:	ld	a,(de)
		inc	de
		or	a
		ret	z
		call	OutChar
		jr	PrintStr

InKey:		push	bc
rptinkey:	ld	c,FCONST	; see if a key has already been pressed
		call	XBDOS
		push	af

		ld	c,FINNOE	; get a keypress anyway
		call	XBDOS
		ld	h,a

		pop	af		; if a character was ready then read another
		jr	nz,rptinkey	;   (this flushes the keyboard buffer)

		ld	a,h
		cp	' '		; echo if a printing character
		call	nc,OutChar
		pop	bc		; end with a CRLF

PrintCRLF:	ld	a,CR
		call	OutChar
		ld	a,LF

OutChar:	push	hl
		push	de
		push	bc
		ld	e,a
		ld	c,$02		; CONOUT
		call	XBDOS
		pop	bc
		pop	de
		pop	hl
		ret

;------------------------------------------------------------------------------
; ** Messages **
;------------------------------------------------------------------------------

msgReading:	db	" reading",0
msgWriting:	db	" writing",0
msgAR:		db	"Abort or Retry? ",0
msgARI:		db	"Abort, Retry or Ignore? ",0
msgSplats:	db	CR,LF,"*** ",0
msgInsert:	db	CR,LF,"Insert COMMAND2.COM in"
msgDrive:	db	" drive A:",CR,LF,0			; A: is patched to current drive
msgPause:	db	"Press any key... ",0

		DEPHASE

xioEnd:
