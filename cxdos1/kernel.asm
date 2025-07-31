; ------------------------------------------------------------------------------
; kernel.asm
; CXDOS 1 kernel functions
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		INCLUDE	"cxdos1.inc"
		SECTION	DISK

		; Used by main module
		PUBLIC	DosBegin
		PUBLIC	DosSize
		PUBLIC	DosMultiply
		PUBLIC	DosConout
		PUBLIC	DosValFCB
		PUBLIC	DosGetSlotP3
		PUBLIC	DosGetSlotP2
		PUBLIC	DosDiskIO
		PUBLIC	DosInvalidFbuf
		PUBLIC	ClockRunMode
		PUBLIC	PatchTPA
		PUBLIC	PH_SIRQ
		PUBLIC	PH_XFER
		PUBLIC	PH_DOSON
		PUBLIC	PH_DOSOF
		PUBLIC	PH_RDSLT
		PUBLIC	PH_WRSLT
		PUBLIC	PH_CALLF
		PUBLIC	PH_CALSLT
		PUBLIC	PH_ENASLT

		; sys entry points
		PUBLIC	DOS_SSBIOS
		PUBLIC	DOS_SIN
		PUBLIC	DOS_SOUT
		PUBLIC	DOS_CPMVER
		PUBLIC	DOS_DELETE
		PUBLIC	DOS_RENAME
		PUBLIC	DOS_OPEN
		PUBLIC	DOS_CLOSE
		PUBLIC	DOS_CREATE
		PUBLIC	DOS_ABSREA
		PUBLIC	DOS_ABSWRI
		PUBLIC	DOS_SEQRD
		PUBLIC	DOS_SEQWRT
		PUBLIC	DOS_RNDRD
		PUBLIC	DOS_RNDWRT
		PUBLIC	DOS_BLKRD
		PUBLIC	DOS_BLKWRT
		PUBLIC	DOS_ZWRITE
		PUBLIC	DOS_SRCHFR
		PUBLIC	DOS_SRCHNX
		PUBLIC	DOS_FILESI
		PUBLIC	DOS_LOGIN
		PUBLIC	DOS_SETDMA
		PUBLIC	DOS_GETEFA
		PUBLIC	DOS_DSKRES
		PUBLIC	DOS_WRTFAT
		PUBLIC	DOS_GETDRV
		PUBLIC	DOS_SETRND
		PUBLIC	DOS_SELDSK
		PUBLIC	DOS_BUFIN
		PUBLIC	DOS_CRLF
		PUBLIC	DOS_BUFOUT
		PUBLIC	DOS_CONOUT
		PUBLIC	DOS_CONSTA
		PUBLIC	DOS_CONIN
		PUBLIC	DOS_IN
		PUBLIC	DOS_RAWIO
		PUBLIC	DOS_RAWINP
		PUBLIC	DOS_LIST
		PUBLIC	DOS_READER
		PUBLIC	DOS_PUNCH
		PUBLIC	DOS_GETDAT
		PUBLIC	DOS_SETDAT
		PUBLIC	DOS_GETTIM
		PUBLIC	DOS_SETTIM
		PUBLIC	DOS_SETRAW

		; Used by driver
		PUBLIC	GETSLT			; Get disk driver's slot address.
		PUBLIC	GETWRK			; Get address of disk driver's work area.
		PUBLIC	DIV16			; BC:=BC/DE, remainder in HL.

		; Defined by main module
		EXTERN	X0046

		; Defined by driver
		EXTERN	DSKIO			; Sector read and write
		EXTERN	DSKCHG			; Get disk change status
		EXTERN	GETDPB			; Get disk parameter block (DPB)

DosBegin:
		PHASE	DOSBASE

; ------------------------------------------------------------------------------
; *** Kernel functions ***
; ------------------------------------------------------------------------------

; Subroutine: check if keyboard input available (get status)
; Output: Zx set if no input, Zx reset if input, a = input
DOS_SSBIOS:	push	ix
		ld	ix,BREAKX
		call	K_BIOS			; BREAKX BIOS call
		pop	ix			; CTRL-STOP pressed ?
		jr	nc,_ssbios1		; nc=no
		ld	a,$03
		ld	(KEYVLD),a		; saved input available
		ld	(KEYCHR),a		; CTRL-C
		and	a			; flag NZ
		ret

_ssbios1:	ld	a,(KEYVLD)
		and	a			; saved input available ?
		ld	a,(KEYCHR)
		ret	nz			; nz=yes, return it
		push	ix
		ld	ix,CHSNS
		call	K_BIOS			; CHSNS BIOS call
		pop	ix			; any chars in the keyboard buffer ?
		ret	z			; z=no, quit (flag Z)
		ld	a,$ff
		ld	(KEYVLD),a		; flag saved input available
		push	ix
		ld	ix,CHGET
		call	K_BIOS			; CHGET BIOS call
		pop	ix			; get char from keyboard buffer
		ld	(KEYCHR),a		; save char
		push	bc
		ld	b,$00
		inc	b
		pop	bc			; flag NZ
		ret

; Subroutine get character from keyboard
; Output: a = character
DOS_SIN:	push	hl
		ld	hl,KEYVLD
		xor	a
		cp	(hl)			; saved input available ?
		ld	(hl),a			; not anymore!
		inc	hl
		ld	a,(hl)
		pop	hl
		ret	nz			; nz=yes, return saved input
		push	ix
		ld	ix,CHGET
		call	K_BIOS
		pop	ix			; get char
		ret

; Subroutine output character to screen
; Input: a = character
DOS_SOUT:	push	ix
		ld	ix,CHPUT
		call	K_BIOS
		pop	ix
		ret

; Subroutine output character to printer
; Input: a = character
K_LPTOUT:	push	ix
		ld	ix,LPTOUT
		call	K_BIOS
		pop	ix
		ret

; ---------------------------------------------------------
; Subroutine call main-bios
; Input:  ix = BIOS routine
; ---------------------------------------------------------
K_BIOS:		push	iy
		ld	iy,(EXPTBL-1)
		call	CALSLT
		ei
		pop	iy
		ret

; ---------------------------------------------------------

; Subroutine store date
StoreDate:	ld	(CURDAT),hl
		ld	a,(TIMFLG)
		and	a			; use clockchip ?
		ret	z			; no, quit
		ld	a,(YEAR)
		ld	b,a
		ld	a,(MONTH)
		ld	c,a
		ld	a,(DAY)
		ld	d,a			; current day
		ld	e,$07			; nibble 7 (date)
		call	ClockPause		; pause clock, select bank 0
		jr	ClockWrite

; Subroutine store time
StoreTime:	ld	a,(TIMFLG)
		and	a			; use clockchip ?
		ret	z			; no, quit
		ld	e,$00			; nibble 0 (time)
		call	ClockPause		; pause clock, select bank 0
		ld	a,$0f
		out	($b4),a
		ld	a,$02
		out	($b5),a			; time reset
ClockWrite:	ld	h,d
		call	ClockWriteByte
		ld	h,c
		call	ClockWriteByte
		ld	h,b
		call	ClockWriteByte

; Subroutine put clockchip in running mode
ClockRunMode:	ld	a,13
		out	($b4),a
		in	a,($b5)
		or	$08
ClockMode:	out	($b5),a
		ret

; Subroutine pause clockchip and select bank 0
ClockPause:	call	ClockRunMode		; clock in running mode
		and	$04
		jr	ClockMode		; clock paused, select bank 0

; Subroutine write byte to clockchip
; Input:  h = data
;         e = nibble number
; Output: e = updated nibble number (+2)
ClockWriteByte:	xor	a
		ld	l,8
_tobcd:		rlc	h
		adc	a,a
		daa
		dec	l
		jr	nz,_tobcd
		call	_clockwrbyte1
		rrca
		rrca
		rrca
		rrca
_clockwrbyte1:	push	af
		ld	a,e
		inc	e
		out	($b4),a
		pop	af
		jr	ClockMode

; Subroutine get date and time
; Output: cx set if from clockchip,
GetDateTime:	ld	a,(TIMFLG)
		and	a			; use clockchip ?
		ld	b,a
		ld	c,a
		ld	d,a
		ld	e,a			; 00:00:00
		ld	hl,(CURDAT)		; days since 1-1-1980
		ret	z			; no clockchip, quit
		call	ClockPause		; clock paused, select bank 0
		ld	e,12+1
		call	ClockReadByte
		call	_timestamp6		; setup days in februari
		call	ClockReadByte
		ld	(MONTH),a		; current month
		call	ClockReadByte
		ld	(DAY),a			; current day
		dec	e			; nibble 5
		call	ClockReadByte
		ld	b,a
		call	ClockReadByte
		ld	c,a
		call	ClockReadByte
		call	ClockRunMode		; clock in running mode
		scf
		ret

; Subroutine read byte from clockchip
; Input:  e = nibble number+1
; Output: e = updated nibble number (-2)
;         a = data
ClockReadByte:	xor	a
		call	_clockrdbyte1
		add	a,a
		add	a,a
		add	a,d
		add	a,a
_clockrdbyte1:	ld	d,a
		dec	e
		ld	a,e
		out	($b4),a
		in	a,($b5)
		and	$0f
		add	a,d
		ld	d,a
		ret

; ---------------------------------------------------------
; Function $0C CPMVER
; ---------------------------------------------------------
DOS_CPMVER:	ld	b,$00			; machinetype 8080, plain CP/M
		ld	a,$22			; CP/M version 2.2
		ret

; ---------------------------------------------------------

; Subroutine get FAT entry content
; Input:  hl = cluster number
;         ix = pointer to DPB
; Output: hl = cluster entry content
;         de = pointer to FAT buffer
;         zx set if entry is free
GETFAT:		ld	e,(ix+19)
		ld	d,(ix+20)		; pointer to FAT buffer of drive
		jp	NewGetFAT
OldGetFAT:	push	de			; store pointer to FAT buffer of drive
		ld	e,l
		ld	d,h			; cluster number
		srl	h
		rr	l			; /2
		rra				; remainer in b7
		add	hl,de			; * 1.5
		pop	de			; restore pointer to FAT buffer of drive
		add	hl,de			; pointer in FAT buffer
		rla				; remainer in Cx
		ld	a,(hl)
		inc	hl
		ld	h,(hl)			; FAT entry content
		jr	nc,_getfat1		; even entry, skip shift
		srl	h
		rra
		srl	h
		rra
		srl	h
		rra
		srl	h
		rra				; shift 4 bits right
_getfat1:	ld	l,a
		ld	a,h
		and	$0f			; FAT entry content to 12 bit
		ld	h,a
		or	l			; Zx set if entry is free
		ret

; Subroutine set FAT entry content
; Input:  hl = clusternumber
;         de = pointer to FAT buffer
;         bc = clusterentry content
PutFAT:		jp	F16P02
OldPutFAT:	srl	h
		rr	l
		rra
		add	hl,de
		pop	de
		add	hl,de
		rla
		jr	nc,_putfat1
		sla	c
		rl	b
		sla	c
		rl	b
		sla	c
		rl	b
		sla	c
		rl	b
		ld	a,(hl)
		and	$0f
		or	c
		ld	(hl),a
		inc	hl
		ld	(hl),b
		ret

_putfat1:	ld	(hl),c
		inc	hl
		ld	a,(hl)
		and	$f0
		or	b
		ld	(hl),a
		ret

; Subroutine compare with filename1
; Input:  hl = pointer to buffer
;         b = size
; Output: zx set if equal
CmpName1:	ld	de,NAME1

; Subroutine compare
; Input:  de = pointer to buffer1
;         hl = pointer to buffer2
;         b  = size
; Output: zx set if equal
CompareName:	ld	a,(de)
		cp	(hl)
		inc	hl
		inc	de
		ret	nz
		djnz	CompareName
		ret

; Subroutine check if devicename
ValDevName:	call	H_DEVN
		ld	hl,IONAME		; table with devicenames
		ld	c,5			; 5 devices
_devnam1:	ld	b,4			; check 4 bytes (device names are 4 chars long)
		call	CmpName1		; compare with filename1
		jr	nz,_devnam3		; nz=not this device, try the next
		ld	b,4
_devnam2:	ld	a,(de)
		inc	de
		cp	' '
		jr	nz,_devnam4		; last 4 bytes of filename not spaces, not a device
		djnz	_devnam2
		ld	a,c
		neg
		ld	(DEVDIR+11),a		; device code
		ld	hl,NAME1
		ld	de,DEVDIR
		ld	bc,4
		ldir				; copy of devicename
		call	DirDateTime		; get time and date (dirformat)
		ld	(DEVDIR+24),bc
		ld	(DEVDIR+22),de
		ld	hl,DEVDIR
		push	hl
		pop	iy
		or	$01			; Cx reset, Zx reset
		ret

_devnam3:	dec	b
		ld	a,l
		add	a,b
		ld	l,a
		ld	a,h
		adc	a,$00
		ld	h,a
		dec	c
		jr	nz,_devnam1

_devnam4:	scf
		ret

; Subroutine validate FCB, clear S2 and find direntry
ValFCB:		push	de
		ld	hl,14
		add	hl,de
		ld	(hl),0			; FCB S2 (extent high byte)
		call	_valfcb			; validate FCB drive and filename and find direntry
		pop	de
		ret

_valfcb:	call	ValPath			; validate FCB drive and filename
		ret	c			; invalid, quit

; Subroutine find first directory entry
FindFirst:	call	ValDevName		; is device name?
		ret	nc			; nc=yes, quit with pointer to fake device direntry
		call	ResetDir		; reset direntry search and get latest FAT

; Subroutine find next directory entry
FindNext:	call	H_CONT
		call	FirstNextDir		; get next direntry
		ret	c			; no more, quit
_find1:		ld	a,(hl)
		or	a
		jr	z,_find6		; unused entry,
		cp	$e5
		jr	z,_find6		; deleted entry,
		push	hl
		ld	b,11
		ld	de,NAME1
_find2:		call	CompareName		; compare with fcb filename
		jr	z,_find3		; equal, found!
		cp	'?'
		jr	nz,_find4		; on difference no wildcard, try next
		djnz	_find2			; wildcard pos ignored, check rest
_find3:		pop	hl
		push	hl
		pop	iy
		ld	a,(ATTRIB)
		xor	$80
		bit	7,a
		ret	z			; orginal FCB DR byte had b7 set, ignore direntryattribute
		ld	a,(iy+11)
		and	$1e
		ret	z			; files with archive or read-only bit set are ok, quit
		ld	a,(CREATI)
		or	a			; include special fileattribute flag set ?
		ret	nz			; nz=yes, every direntry is ok, quit
		jr	_find5

_find4:		pop	hl
_find5:		call	NextDir			; get next direntry (while searching)
		jr	nc,_find1		; ok, check it
		ret

_find6:		ld	a,(ENTFRE)
		inc	a			; already found a free direntry ?
		jr	nz,_find7		; nz=yes
		ld	a,(LASTEN)
		ld	(ENTFRE),a		; no, register it
_find7:		ld	a,(hl)
		or	a			; unused direntry ?
		jr	nz,_find5		; nz=no, the search goes on!
		scf
		ret

; Subroutine get next direntry (at start of search)
FirstNextDir:	ld	a,(LASTEN)
		inc	a
		cp	(ix+11)			; last direntry ?
		jr	nc,UpdateDir		; nc=yes, update directory of disk when needed and quit

; Subroutine get direntry
GetDir:		call	H_GETE
		ld	(LASTEN),a
		ld	c,a
		and	(ix+4)			; dirmask
		ld	l,a
		ld	h,0
		add	hl,hl
		add	hl,hl
		add	hl,hl
		add	hl,hl
		add	hl,hl
		ld	de,(SDIRBU)		; dirsector buffer
		add	hl,de
		ld	b,(ix+5)		; dirshift
_getdir1:	srl	c
		djnz	_getdir1
		ld	a,(DIRBFI)
		cp	c			; same as dirsector currently in buffer ?
		jr	nz,_getdir2		; nz=no, go get it
		ld	a,(DIRBFD)
		cp	(ix+0)			; same driveid as dirsector buffer owner ?
		ret	z			; z=yes, do nothing
_getdir2:	push	hl
		call	ReadDirSec		; read dirsector
		pop	hl
		ret

; Subroutine get next direntry (while searching)
NextDir:	call	H_NEXT
		ld	a,(LASTEN)
		inc	a
		cp	(ix+11)			; last direntry ?
		jr	nc,UpdateDir		; nc=yes, update directory of disk when needed and quit
		ld	(LASTEN),a
		ld	de,$0020
		add	hl,de
		and	(ix+4)			; dirmask
		ret	nz
		inc	c
		call	ReadDirSec		; read dirsector
		ld	hl,(SDIRBU)		; dirsector buffer
		ret

; Subroutine at end of directory
UpdateDir:	call	FlushDirBuf		; flush directory buffer
		scf
		ret

; ---------------------------------------------------------
; Function $13 FDEL
; Delete file (fcb)
; ---------------------------------------------------------
DOS_DELETE:	call	ValPath			; validate FCB drive and filename
		call	nc,FindFirst		; valid, find first directoryentry
		ld	a,$ff
		ret	c			; c=invalid or not found, quit with error
		ret	nz			; device, quit with error
_delete1:	ld	a,$e5
		ld	(DIRTYD),a		; flag directory buffer dirty
		ld	(hl),a			; deleted direntry
		ld	l,(iy+26)
		ld	h,(iy+27)
		ld	a,h			; file has start cluster ?
		or	l
		call	nz,ClusChainFree	; nz=yes, release cluster chain
		call	FindNext		; find next directoryentry
		jr	nc,_delete1		; found, delete next file
		call	_rename4		; update directory of disk (SHOULD BE: CALL WriteDirSec)
		jp	WriteFatBuf		; write FAT buffer (SHOULD BE: JP FlushFatBuf, flush FAT buffer)

; ---------------------------------------------------------
; Function $17 FREN
; Rename file (fcb)
; ---------------------------------------------------------
DOS_RENAME:	call	ValPath			; validate FCB drive and filename
		jr	c,_rename6		; invalid, quit with error
		ld	de,$0005
		add	hl,de			; to new filename
		ld	de,NAME2		; new filenamebuffer
		call	DosValFCB		; LODNAM: validate FCB filename (new filename)
		call	nc,FindFirst		; new filename valid, find first directoryentry
		jr	c,_rename6		; invalid or not found, quit with error
		jr	nz,_rename6
		ld	hl,NAME1
		ld	de,NAME3
		ld	bc,11+1
		ldir				; save filename (search specifier) + orginal DR byte
_rename1:	ld	hl,NAME2		; new filename
		ld	de,NAME1
		ld	b,11
_rename2:	ld	a,(hl)
		cp	'?'			; wildcard char ?
		jr	nz,_rename3		; nz=no, use the char of the new filename
		ld	a,(iy+0)		; yes, use the char of the orginal filename
_rename3:	ld	(de),a
		inc	hl
		inc	de
		inc	iy
		djnz	_rename2
		ld	a,$80
		ld	(de),a			; orginal DR byte b7 set (ignore fileattribute)
		call	ValDevName		; check if devicename
		jr	nc,_rename5		; nc=yes, end rename with error
		ld	a,(LASTEN)
		push	af
		ld	a,$ff
		ld	(LASTEN),a		; flag direntry search start at the begin
		call	FindNext		; find next directoryentry
		pop	bc
		jr	nc,_rename5		; found, so resulting filename does already exist. end rename with error
		ld	a,b
		call	GetDir			; get direntry which get renamed
		ex	de,hl
		ld	hl,NAME1
		ld	bc,11
		ldir				; replace filename with new one
		ld	a,$ff
		ld	(DIRTYD),a		; flag directory buffer dirty
		ld	hl,NAME3
		ld	de,NAME1
		ld	bc,11+1
		ldir				; restore filename (search specifier) + orginal DR byte
		call	FindNext		; find next directoryentry
		jr	nc,_rename1		; found, rename next file
_rename4:	call	FlushDirBuf		; flush directory buffer
		xor	a			; no error
		ret

_rename5:	call	FlushDirBuf		; flush directory buffer
_rename6:	ld	a,$ff			; error
		ret

; Subroutine validate FCB drive and filename
ValPath:	call	H_MOVN
		xor	a
		ld	(CREATI),a		; do not include special file attributes
		ex	de,hl
		ld	a,(hl)
		inc	hl
		ld	(ATTRIB),a		; save FCB DR byte
		and	$0f			; only use b3-b0 for drive
		call	ValDrive		; validate fcb driveid
		ret	c			; c=error
		ld	de,NAME1
		jp	DosValFCB		; LODNAM: validate FCB filename

; Subroutine validate driveid (FCB style)
; Input: A = driveid
ValDrive:	ld	c,a
		ld	a,(SNUMDR)		; maximum drives
		cp	c			; drive number larger than max?
		ret	c			; c=yes
		ld	a,c
		dec	a
		jp	p,_valdrive1
		ld	a,(CURDRV)		; default driveid
_valdrive1:	ld	(THISDR),a		; set current driveid
		ret

; Subroutine get max record and extent
GetMaxRecExt:	ld	a,(iy+31)
		or	a
		jr	nz,_getmax2		; filesize > 16777215, use max value
		ld	a,(iy+28)
		ld	c,(iy+29)
		ld	b,(iy+30)
		add	a,a
		rl	c
		rl	b			; number of records (128 bytes)
		jr	c,_getmax2		; >65535, use max value
		or	a			; is filesize a multiply of 128 ?
		jr	z,_getmax1		; z=yes
		inc	bc			; no, increase the recordnumber
		ld	a,b
		or	c			; does that fit ?
		jr	z,_getmax2		; z=no, use max value
_getmax1:	ld	a,c
		res	7,c			; c = max recordnumber (0-127)
		add	a,a
		rl	b			; b = max extent
		ret	nc			; does fit, quit
_getmax2:	ld	bc,0FF7FH		; extent 255, record 127
		ret

; ---------------------------------------------------------
; Function $0F FOPEN
; Open file (fcb)
; ---------------------------------------------------------
DOS_OPEN:	call	ValFCB			; validate FCB, clear S2 and find directory entry
		jr	c,_rename6		; c=error, quit
		call	GetMaxRecExt		; get max record and extent
		ld	a,(FCBEXT)		; original FCB EX byte
		inc	b			; ?? correct for large files (filesize > 4177919 where extend is $ff)
		cp	b			; is extent of file big enough ?
		jr	nc,_rename6		; nc=no, quit with error
_open1:		call	H_DOOP
		ex	de,hl
		ld	bc,$000f
		add	hl,bc
		call	GetMaxRecExt		; get max record and extent
		ld	a,(FCBEXT)
		cp	b			; orginal FCB EX byte same as max extent ?
		jr	z,_open2		; same, use RC=max recordnumber (means extent is not full)
		ld	c,$80
		jr	c,_open2		; smaller, use RC=128 (means extend is full)
		ld	c,$00			; bigger, use RC=0 (means extend is empty)
_open2:		ld	(hl),c			; RC
		inc	hl
		ex	de,hl
		ld	bc,$001c
		add	hl,bc
		ld	c,$04
		ldir				; copy filesize
		ld	bc,$fff8
		add	hl,bc
		ldi
		ldi				; creation date
		ld	c,$fc
		add	hl,bc
		ldi
		ldi				; creation time
		ld	a,(iy+11)
		bit	7,a
		jr	nz,_open3		; device,
		ld	a,(ix+0)		; driveid
		or	$40			; flag diskfile unchanged
_open3:		ld	(de),a			; devicecode
		inc	de
		ld	a,(LASTEN)
		ld	(de),a			; direntry number
		inc	de
		ld	a,(iy+26)
		ld	(de),a
		inc	de
		inc	de
		ld	(de),a
		dec	de
		ld	a,(iy+27)
		ld	(de),a
		inc	de
		inc	de
		ld	(de),a			; start cluster and last cluster accessed
		inc	de
		xor	a
		ld	(de),a
		inc	de
		ld	(de),a			; last cluster accessed, relative
		ret

; Subroutine handle DSKCHG error
DskChgError:	ld	c,a
		ld	a,(THISDR)		; current driveid
		call	DiskError		; start diskerror handler
		jr	GetFatSec		; get latest FAT (try again)

; Subroutine reset direntry search and get latest FAT
ResetDir:	ld	a,$ff
		ld	(LASTEN),a		; invalid latest direntry (search from the begin)
		ld	(ENTFRE),a		; not found a free direntry

; Subroutine get latest FAT
GetFatSec:	call	H_FATR
		call	GetDPBAdr		; get pointer to DPB of current drive
		ld	a,(THISDR)		; current driveid
		ld	c,(ix+1)		; mediadesciptor
		ld	b,0
		or	a
		call	DiskChange		; DSKCHG (DiskChg_all)
		jr	c,DskChgError		; c=error
		call	SetDPBAdr		; update pointer to DPB of current drive
		ld	l,(ix+19)
		ld	h,(ix+20)		; pointer to FAT buffer of drive
		dec	hl
		ld	a,b			; DSKCHG status
		or	(hl)			; combined with the FAT buffer dirty flag
		ld	a,(THISDR)		; current driveid
		ld	hl,(BUFDRN)
		jp	m,_fatsec1		; FAT buffer invalid OR diskchange unknown, read the FAT
		ret	nz			; FAT buffer dirty OR disk unchanged, do not read the FAT and quit
		cp	l			; current drive same as datasector buffer owner ?
		jr	nz,_fatsec2		; nz=no, read the FAT
		dec	h			; datasector buffer changed ?
		ret	z			; z=yes, do not read the FAT and quit
_fatsec1:	sub	l			; current drive same as datasector buffer owner ?
		jr	nz,_fatsec2		; nz=no, leave the datasector buffer alone
		ld	l,a
		ld	h,a
		dec	l
		ld	(BUFDRN),hl		; invalid datasector buffer
_fatsec2:	ld	a,$ff
		ld	(DIRBFD),a		; invalid dirsector buffer
		jp	NewGetFATbuf

ChangeDPB:	ld	b,(hl)			; mediabyte of FAT sector
		ld	a,(THISDR)		; current driveid
		ld	c,(ix+1)		; mediadescriptor
		push	ix
		pop	hl			; pointer to DPB
		call	DiskGetDPB

; Subroutine update pointer to DPB of current drive
; Input: hl = pointer to DPB
;A4536
SetDPBAdr:	push	hl
		pop	ix			; pointer to DPB
		ex	de,hl
		call	GetDPBTBL		; get DPBTBL entry of current drive
		ld	(hl),e
		inc	hl
		ld	(hl),d			; update pointer to DPB
		ret

; Subroutine get pointer to DPB of current drive
; Output: hl,ix = pointer to DPB
GetDPBAdr:	call	H_GETI
		call	GetDPBTBL		; get DPBTBL entry of current drive
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		push	hl
		pop	ix
		ret

; Subroutine get DPBTBL entry of current drive
; Input: HL = address of pointer
GetDPBTBL:	ld	a,(THISDR)		; current driveid
GetDPBptr:	ld	hl,SDPBLI
		add	a,a
		add	a,l
		ld	l,a
		ret	nc
		inc	h
		ret

; ---------------------------------------------------------
; Function $10 FCLOSE
; Close file (fcb)
; Input:  de = pointer to FCB
; ---------------------------------------------------------
DOS_CLOSE:	push	de
		pop	iy
		call	ValPath			; validate FCB drive and filename
		ld	a,$ff
		ret	c			; invalid, quit with error
		ld	a,(iy+24)
		and	$c0
		ld	a,0			; ok
		ret	nz			; device OR unchanged diskfile, quit
		ld	a,(THISDR)		; current driveid
		ld	hl,(BUFDRN)
		cp	l			; same drive as owner datasector buffer ?
		call	z,FlushDataBuf		; z=yes, flush datasector buffer
		call	GetDPBAdr		; get pointer to DPB of current drive
		ld	a,(iy+25)		; direntrynumber
		call	GetDir			; get direntry
		ld	b,11
		call	CmpName1		; compare with filename1
		jr	nz,CleanFatBuf		; not the same, make FAT buffer unchanged and quit with error
		push	iy
		pop	de
		ld	c,$0b
		add	hl,bc
		ex	de,hl
		ld	c,$16
		add	hl,bc
		ldi
		ldi
		ld	bc,0FFFCH
		add	hl,bc
		ldi
		ldi
		ld	bc,$0004
		add	hl,bc
		ldi
		ldi
		ld	bc,0FFF4H
		add	hl,bc
		ld	bc,$0004
		ldir
		call	WriteDirSec		; update directory of disk

;Subroutine flush FAT buffer
FlushFatBuf:	ld	l,(ix+19)
		ld	h,(ix+20)		; pointer to FAT buffer of drive
		dec	hl
		ld	a,(hl)
		cp	1			; FAT buffer dirty ?
		ret	nz			; nz=no, quit (?? return error if FAT buffer invalid)

; Subroutine write FAT buffer
WriteFatBuf:	call	H_FATW
		call	NewUpdateFAT
		xor	a
		ret

; Subroutine make FAT buffer unchanged and quit with error
CleanFatBuf:	ld	l,(ix+19)
		ld	h,(ix+20)		; pointer to FAT buffer of drive
		dec	hl
		ld	(hl),0			; FAT buffer clean
		ld	a,$ff			; error
		ret

; Subroutine get FAT parameters
; Input:  ix = pointer to DPB
; Output: a  = number of FATs
;         de = first FAT sector
;         b  = number sectors per FAT
;         hl = pointer to FAT buffer

GetFATbuf:	ld	a,(ix+10)		; number of FATs
		ld	l,(ix+19)
		ld	h,(ix+20)		; pointer to FAT buffer of drive
		ld	b,(ix+16)		; number of sectors per FAT
		ld	e,(ix+8)
		ld	d,(ix+9)		; first FAT sector
		ret

; Subroutine get dir parameters
; Input:  ix = pointer to DPB
;         a  = relative dirsector
;         de = first dirsector
; Output: de = dirsector
;         b  = 1
;         hl = pointer to dirsector buffer

GetDirParam:	add	a,(ix+17)
		ld	e,a
		ld	d,(ix+18)
		jr	nc,_dirparam1
		inc	d			; + first dir sector
_dirparam1:	ld	hl,(SDIRBU)		; dirsector buffer
		ld	b,1			; 1 sector
		ret

; ---------------------------------------------------------
; Function $16 FMAKE
; Create file (fcb)
; ---------------------------------------------------------
DOS_CREATE:	push	de
		call	ValPath			; validate FCB drive and filename
		jr	c,_create2		; invalid, quit with error
		inc	hl
		inc	hl
		ld	(hl),0			; clear S2 byte
		ld	hl,NAME1
		ld	a,'?'
		ld	bc,11
		cpir				; wildcard char in filename ?
		jr	z,_create2		; z=yes, quit with error
		call	FindFirst		; find first directoryentry
		jr	nc,_create3		; found, special actions for existing file/device
		ld	a,(ENTFRE)
		cp	$ff			; found free direntry ?
		jr	z,_create2		; z=no, quit with error (directory is full)
		call	GetDir			; get direntry
		push	hl
		pop	iy
		jr	_create4		; setup direntry

_create1:	bit	7,(iy+11)
		jr	nz,_create8		; device, treat as open file
						; file with special fileattribute, quit with error
_create2:	pop	de
		ld	a,$ff
		ret

_create3:	jr	nz,_create1		; device or file with special fileattribute,
		ld	a,(FCBEXT)		; orginal FCB EX byte
		or	a
		jr	nz,_create8		; is not zero, just open the file
		ld	l,(iy+26)
		ld	h,(iy+27)
		ld	a,h
		or	l			; has start cluster ?
		jr	z,_create4		; z=no
		call	ClusChainFree		; release cluster chain
		call	WriteFatBuf		; write FAT buffer
_create4:	push	iy
		pop	de
		ld	hl,NAME1
		ld	bc,11
		ldir				; copy filename in FCB to direntry
		ld	a,(hl)
		rla
		ld	a,$00
		jr	nc,_create5		; b7 DR byte reset, ordinary file
		ld	a,$06			; b7 DR byte set, hidden system file
_create5:	ld	(de),a
		inc	de
		ex	de,hl
		ld	b,10
		xor	a
_create6:	ld	(hl),a
		inc	hl
		djnz	_create6		; clear unused bytes direntry
		push	hl
		call	DirDateTime		; get time and date (dirformat)
		pop	hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl			; fill in time and date in direntry
		xor	a
		ld	b,2+4
_create7:	ld	(hl),a
		inc	hl
		djnz	_create7		; fill in no first cluster, filesize 0 in direntry
		call	WriteDirSec		; update directory of disk

_create8:	push	iy
		pop	hl
		pop	de
		jp	_open1			; continue with open file

; Subroutine read dirsector
; Input:  c = relative dir sector
ReadDirSec:	push	bc
		call	FlushDirBuf		; flush directory buffer
		pop	bc
		ld	b,(ix+0)		; driveid
		ld	(DIRBFI),bc		; set driveid and sector dirsector buffer
		push	bc
		ld	a,c			; relative dirsector
		call	GetDirParam		; setup dirsector parameters
		call	DiskReadSec		; read dirsector with DOS error handling
		pop	bc
		ret

; ---------------------------------------------------------
; Function $2F RDABS
; Absolute sector read
; ---------------------------------------------------------
DOS_ABSREA:	ld	b,h
		ld	a,l
		ld	(THISDR),a		; set current driveid
		call	GetDPBAdr		; get pointer to DPB of current drive
		ld	hl,(DMAADD)		; transferaddress

; Subroutine read sectors with DOS error handling
DiskReadSec:	call	H_DREA
		xor	a
		ld	(READOP),a		; flag read disk operation
		call	_readsec		; read sector
		ret	nc			; no error, quit
		call	RestartSec		; adjust parameters to restart at error sector and start diskerror handler
		dec	a
		jr	z,DiskReadSec		; RETRY, try again
		ret				; IGNORE, quit

_readsec:	ld	a,(ix+0)		; driveid
		ld	c,(ix+1)		; mediadescriptor
		push	hl
		push	de
		push	bc
		call	ReadSector
		pop	de
		ld	c,d
		pop	de
		pop	hl
		ret

; Subroutine adjust parameters to restart at error sector and start diskerror handler
RestartSec:	push	af
		ld	a,c
		sub	b
		ld	c,a
		push	bc
		ld	b,$00
		ex	de,hl
		add	hl,bc
		push	hl
		push	de
		ld	e,(ix+2)
		ld	d,(ix+3)		; sectorsize
		call	DosMultiply		; multiply
		pop	hl
		pop	de
		add	hl,bc
		pop	bc
		pop	af
		ld	c,a
		ld	a,(READOP)		; type of diskoperation
		or	c
		ld	c,a
		ld	a,(ix+0)		; driveid

;Subroutine start diskerror handler
DiskError:	call	H_FATA
		push	bc
		push	de
		push	hl
		ld	hl,(DISKVE)
		call	JPHL			; start diskerror handler in DOS memory
		ld	a,c			; requested action
		pop	hl
		pop	de
		pop	bc
		cp	2
		ret	nz
		jp	WBOOT			; ENDJMP: warm boot

; ---------------------------------------------------------
; Function $30 WRABS
; Absolute sector write
; ---------------------------------------------------------
DOS_ABSWRI:	ld	b,h
		ld	a,l
		ld	(THISDR),a		; set current driveid
		call	GetDPBAdr		; get pointer to DPB of current drive
		ld	hl,(DMAADD)		; transferaddress
		jr	DiskWriteSec		; write sectors with DOS error handling

; ---------------------------------------------------------

; Subroutine flush datasector buffer
FlushDataBuf:	ld	hl,DIRTYB
		xor	a
		cp	(hl)			; datasector buffer dirty ?
		ld	(hl),a			; now it is clean
		ret	z			; z=no, quit
		ld	ix,(BUFDRI)		; saved DPB pointer
		ld	hl,(SBUFFE)		; datasector buffer
		ld	b,1			; 1 sector
		ld	de,(BUFSEC)		; sectornumber of datasector buffer
		jr	DiskWriteSec		; write sector with DOS error handling

; Subroutine flush directory buffer
FlushDirBuf:	ld	a,(DIRTYD)
		or	a			; directory buffer dirty ?
		ret	z			; z=no, quit

; Subroutine write dirsector buffer
WriteDirSec:	call	H_DIRW
		xor	a
		ld	(DIRTYD),a		; directory buffer clean
		ld	a,(DIRBFI)		; current dirsector (offset)
		call	GetDirParam		; setup dirsector parameters

; Subroutine write sectors with DOS error handling
DiskWriteSec:	call	H_DWRI
		ld	a,1
		ld	(READOP),a		; flag write disk operation
		ld	a,(ix+0)		; driveid
		ld	c,(ix+1)		; mediadescriptor
		push	hl
		push	de
		push	bc
		call	WriteSector		; write disksector
		pop	de
		ld	c,d
		pop	de
		pop	hl
		ret	nc			; no error, quit
		call	RestartSec		; adjust parameters to restart at error sector and start diskerror handler
		dec	a
		jr	z,DiskWriteSec		; RETRY, try again
		ret				; IGNORE, quit

; ---------------------------------------------------------
; Function $14 RDSEQ
; Sequential read (fcb)
; Input:  de = pointer to FCB
; ---------------------------------------------------------
DOS_SEQRD:	call	GetRecNum		; get recordnumber from CR,EX and S2 fields
		call	ReadRecord		; read record
		jr	_seqrw			; update sequencial fields

; ---------------------------------------------------------
; Function $15 WRSEQ
; Sequential write (fcb)
; Input:  de = pointer to FCB
; ---------------------------------------------------------
DOS_SEQWRT:	call	GetRecNum			; get recordnumber from CR,EX and S2 fields
		call	WriteRecord			; write record
_seqrw:		call	_zwrite7		; increase recordnumber if something was read/written
		jr	_rndrw2			; update CR,EX and S2 field

; ---------------------------------------------------------
; Function $21 RDRND
; Random read (fcb)
; ---------------------------------------------------------
DOS_RNDRD:	call	_zwrite5		; get recordnumber from Rx fields, 1 record
		call	ReadRecord			; read record
		jr	_rndrw1			; update Rx, CR,EX and S2 field

WriteRndRec:	push	iy
		pop	de

; ---------------------------------------------------------
; Function $22 WRRND
; Random write (fcb)
; ---------------------------------------------------------
DOS_RNDWRT:	call	_zwrite5		; get recordnumber from Rx fields, 1 record
		call	WriteRecord			; write record
_rndrw1:	call	_zwrite4		; update Rx fields
_rndrw2:	ld	a,l
		and	$7f
		ld	(iy+32),a		; CR
		sla	l
		rl	h
		ld	(iy+12),h		; S2
		rl	e
		ld	(iy+14),e		; EX
		ld	a,(DSKERR)		; result recordoperation
		ret

; ---------------------------------------------------------
; Function $27 RDBLK
; Random block read (fcb)
; Input:  de = pointer to FCB
;         hl = number of records
; ---------------------------------------------------------
DOS_BLKRD:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		call	_zwrite6		; get random record number
		call	ReadRecord		; read record(s)
		jr	_blkrw

; ---------------------------------------------------------
; Function $26 WRBLK
; Random block write (fcb)
; Input:  DE = pointer to FCB
;         HL = number of records
; ---------------------------------------------------------
DOS_BLKWRT:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		call	_zwrite6		; get random record number
		call	WriteRecord		; write record(s)
_blkrw:		call	_zwrite7		; increase recordnumber if something was read/written
		call	_zwrite4		; update Rx fields
		ld	l,c
		ld	h,b
		ret

; ---------------------------------------------------------
; Function $28 WRZER
; Random write with zero fill (fcb)
; ---------------------------------------------------------
DOS_ZWRITE:	push	de
		pop	iy
		ld	a,(iy+16)
		ld	c,(iy+17)
		ld	b,(iy+18)
		ld	e,(iy+19)
		add	a,a
		rl	c
		rl	b
		rl	e			; convert filesize to a random record number
		or	a			; was filesize a multiply of 128 ?
		jr	z,_zwrite1
		inc	bc
		ld	a,b
		or	c
		jr	nz,_zwrite1
		inc	e			; no, increase random record number

; the following code depends on the fact that CP/M 2.2 only uses the R0 and R1 field
; code only works when the random record (filesize) is within 65536 records of the random record (fcb)

_zwrite1:	ld	l,(iy+33)
		ld	h,(iy+34)		; R1 and R0
		sbc	hl,bc
		jr	z,WriteRndRec		; exact at end of file, random access write record and quit
		ld	a,(iy+35)
		sbc	a,e			; before end of file ?
		jr	c,WriteRndRec		; c=yes, random access write record and quit
		push	hl			; save number of gap records
		call	WriteRndRec		; random access write record (gap in filled with garbage)
		pop	de
		or	a
		ret	nz			; nz=error, quit

; now the gap is filled. dirsector buffer is used for the zero filled record
; neat code should first flush the dirsector buffer, but this is ommited

		ld	hl,(DMAADD)		; transfer address
		push	hl
		ld	hl,(SDIRBU)
		ld	(DMAADD),hl		; tempory use dirsector buffer
		ld	b,128
_zwrite2:	ld	(hl),a
		inc	hl
		djnz	_zwrite2		; create a zero filed random record
		dec	a
		ld	(DIRBFD),a		; invalid dirsector buffer
		ld	l,(iy+33)
		ld	h,(iy+34)
		sbc	hl,de
		ld	c,l
		ld	b,h
		ex	de,hl
		ld	d,$00
		ld	a,(iy+35)
		sbc	a,d
		ld	e,a			; start record of gap
_zwrite3:	push	hl
		ld	hl,1
		call	WriteRecord
		call	_zwrite7		; increase recordnumber if something was writen
		ld	c,l
		ld	b,h
		pop	hl
		dec	hl
		ld	a,h
		or	l
		jr	nz,_zwrite3		; next record
		pop	hl
		ld	(DMAADD),hl		; restore transfer address
		ret

_zwrite4:	ld	a,(DSKERR)		; result record operation
		ld	(iy+33),l
		ld	(iy+34),h
		ld	(iy+35),e
		inc	d
		dec	d
		ret	z
		ld	(iy+36),d
		ret

_zwrite5:	ld	hl,1		; 1 record
_zwrite6:	push	de
		pop	iy
		ld	c,(iy+33)		; R0
		ld	b,(iy+34)		; R1
		ld	e,(iy+35)		; R2
		ld	d,(iy+36)		; R3
		ret

_zwrite7:	ret	z
		inc	hl
		ld	a,h
		or	l
		ret	nz
		inc	de
		ret

_zwrite8:	pop	hl
		ld	l,c
		ld	h,b
		ld	a,1
		ld	(DSKERR),a		; error in record operation
		xor	a
		ld	c,a
		ld	b,a
		ret

_zwrite9:	ld	(RECCNT),hl		; number of records requested
		ld	(RECPOS+0),bc
		ld	(RECPOS+2),de		; startrecord
		ld	a,(iy+0)
		call	ValDrive		; validate fcb driveid
		jr	c,_zwrite8
		ld	de,$0080
		ld	a,(CPMCAL)
		or	a			; Random Block ?
		jr	nz,_zwrite10		; nz=no
		ld	a,(iy+14)
		ld	d,(iy+15)		; yes, use user set recordsize
		ld	e,a
		or	d			; zero recordsize ?
		jr	nz,_zwrite10		; nz=no
		ld	e,128
		ld	(iy+14),e		; yes, use 128 bytes default
_zwrite10:	inc	d
		dec	d
		jr	nz,_zwrite11
		ld	a,e
		cp	64
		jr	c,_zwrite12
_zwrite11:	xor	a
		ld	(RECPOS+3),a		; recordsize >64, clear b31-b24 of record (use 24 bit recordnumbers)
_zwrite12:	ld	hl,(DMAADD)
		ld	(NEXTAD),hl		; current transferaddress
		xor	a
		ld	(DSKERR),a		; no error in recordoperation
		ld	(TRANS),a		; flag do not increase sector
		ld	bc,(RECCNT)		; number of records requested
		call	DosMultiply		; * recordsize
		ld	a,(iy+24)
		or	a
		ret	m			; DOS device, quit
		push	bc
		call	GetDPBAdr		; get pointer to DPB of current drive
		ld	bc,(RECPOS+0)
		call	DosMultiply		; multiply
		ld	(BYTPOS+0),bc
		push	bc
		ld	bc,(RECPOS+2)
		call	MultiplyHigh		; multiply high word
		ld	(BYTPOS+2),bc		; startbyte = startrecord * recordsize
		ld	h,b
		ld	l,c
		pop	bc			; BCHL = startbyte
		ld	e,(ix+2)
		ld	d,(ix+3)
		call	Divide			; / sectorsize
		ld	(BYTSEC),hl		; offset in sector of startbyte
		ld	(SECPOS),bc		; relative sector of startbyte
		ld	a,(ix+6)
		and	c			; clustermask
		ld	(SECCLU),a		; current relative sector in cluster (of startbyte)
		ld	a,(ix+7)		; clustershift
_zwrite13:	dec	a
		jr	z,_zwrite14
		srl	b
		rr	c
		jr	_zwrite13

_zwrite14:	ld	(CLUSNU),bc		; relative cluster of startbyte
		pop	bc
		xor	a
		ret

; Subroutine multiply
DosMultiply:	call	H_MUL1
		ld	hl,0

; Subroutine multiply high word
MultiplyHigh:	ld	a,b
		ld	b,$11
		jr	_multiply3

_multiply1:	jr	nc,_multiply2
		add	hl,de
_multiply2:	rr	h
		rr	l
_multiply3:	rra
		rr	c
		djnz	_multiply1
		ld	b,a
		ret

; Subroutine divide
DIV16:		ld	hl,0

; Subroutine divide
Divide:		call	H_DIV3
		ld	a,b
		ld	b,$10
		rl	c
		rla
_divide1:	rl	l
		rl	h
		jr	c,_divide4
		sbc	hl,de
		jr	nc,_divide2
		add	hl,de
_divide2:	ccf
_divide3:	rl	c
		rla
		djnz	_divide1
		ld	b,a
		ret

_divide4:	or	a
		sbc	hl,de
		jr	_divide3

; Subroutine calculate partial sector transfers
CalcPartSec:	ld	h,b
		ld	l,c			; bytes to transfer
		ld	bc,(BYTSEC)		; offset in sector startbyte
		ld	a,b
		or	c
		ld	e,a
		ld	d,a
		jr	z,_partsec1		; at start sector, no partial start
		ld	e,(ix+2)
		ld	d,(ix+3)
		ex	de,hl			; sectorsize
		sbc	hl,bc
		ex	de,hl			; bytes left in sector
		sbc	hl,de			; enough ?
		jr	nc,_partsec1		; nc=no, get what you can
		add	hl,de
		ex	de,hl
		ld	hl,0
_partsec1:	ld	(BYTCT1),de		; bytes to transfer from partial sector
		ld	c,l
		ld	b,h			; bytes left after partial sector transfer
		ld	e,(ix+2)
		ld	d,(ix+3)
		call	DIV16			; / sectorsize
		ld	(BYTCT2),hl		; partial bytes in endsector
		ld	(SECCNT),bc		; hole sectors of transfer
		ret

; Subroutine get absolute cluster
GetAbsClus:	call	H_FNDC
		ld	l,(iy+28)
		ld	h,(iy+29)		; current cluster of file
		ld	e,(iy+30)
		ld	d,(iy+31)		; current relative cluster of file
		ld	a,l
		or	h
		jr	z,_absclus5		; file has no start cluster,
		push	bc
		ld	a,c
		sub	e
		ld	c,a
		ld	a,b
		sbc	a,d
		ld	b,a
		jr	nc,_absclus1		; requested cluster behind current, search from current cluster
		pop	bc
		ld	de,0			; relative cluster 0
		ld	l,(iy+26)
		ld	h,(iy+27)	 	; start cluster of file
		push	af
_absclus1:	pop	af
_absclus2:	call	H_SKPC
		ld	a,b
		or	c
		ret	z
		push	de
		push	hl
		call	GETFAT			; get FAT entry content
		pop	de
		call	F16P01
		jr	nc,_absclus4

_absclus3:	pop	de
		inc	de
		dec	bc
		jr	_absclus2

_absclus4:	ex	de,hl
		pop	de
		ret

_absclus5:	inc	bc			; BC<>0 (means not found)
		dec	de
		ret

; Subroutine read datasector
ReadDataSec:	ld	(PREREA),a
		ld	hl,(CLUSNU)		; relative cluster
		ld	a,(SECCLU)		; current relative sector in cluster
		call	ClusSecNum		; get sectornumber of cluster
		ex	de,hl
		call	F16P06
		jr	nz,_rdatasec1		; nz=no, get it
		ld	a,(THISDR)		; current driveid
		ld	l,a
		ld	a,(BUFDRN)
		cp	l			; same drive as owner datasector buffer ?
		jr	z,_rdatasec3		; z=yes
_rdatasec1:	push	de
		push	ix
		call	FlushDataBuf		; flush datasector buffer
		pop	ix
		pop	de
		ld	a,(PREREA)
		or	a			; real or fake read ?
		jr	nz,_rdatasec2		; fake read
		dec	a
		ld	(BUFDRN),a
		ld	hl,(SBUFFE)		; datasector buffer
		ld	b,1			; 1 sector
		push	de
		call	DiskReadSec		; read sector with DOS error handling
		pop	de
_rdatasec2:	ld	(BUFSEC),de		; current datasector
		ld	a,(THISDR)		; current driveid
		ld	(BUFDRN),a		; set owner datasector buffer
		ld	(BUFDRI),ix		; save DPB pointer
_rdatasec3:	ld	a,1
		ld	(TRANS),a		; flag do increase sector
		ld	hl,(NEXTAD)
		push	hl
		ld	bc,(BYTCT1)
		add	hl,bc
		ld	(NEXTAD),hl		; update current transferaddress
		ld	hl,(SBUFFE)		; datasector buffer
		ld	de,(BYTSEC)
		add	hl,de
		pop	de
		ret

; Subroutine do partial sector read if needed

ReadPartSec:	call	H_BUFR
		ld	hl,(BYTCT1)
		ld	a,h
		or	l			; partial sector read?
		ret	z			; z=no, quit
		xor	a			; real read
		call	ReadDataSec		; read datasector
		ldir				; jp BLKMOV
		ret				; "

; Subroutine handle partial sector write
WritePartSec:	call	H_BUFW
		ld	hl,(BYTCT1)
		ld	a,h
		or	l			; partial start ?
		ret	z			; z=no, quit
		ld	hl,(SECPOS)
		inc	hl
		ld	(SECPOS),hl		; update relative sector of startbyte
		xor	a
		ex	de,hl
		ld	hl,(VALSEC)
		sbc	hl,de			; sector behind end of file ?
		rra				; if yes, fake read
		call	ReadDataSec		; read datasector
		ex	de,hl
		ldir				; BLKMOV
		ld	a,1
		ld	(DIRTYB),a		; flag datasector dirty
		ret

; Subroutine last partial sector ?
LastPartSec:	ld	hl,0
		ld	(BYTSEC),hl
		ld	hl,(BYTCT2)
		ld	(BYTCT1),hl
		ld	a,h
		or	l
		scf
		ret	z

; Subroutine to next sector (only when partial read was done)
NextPartSec:	ld	a,(TRANS)
		or	a			; flag do not increase
		ret	z			; z=yes, quit
		ld	a,(SECCLU)
		cp	(ix+6)			; clustermask
		jr	c,_nextpart1		; still sectors left in this cluster, increase relative sector in cluster
		ld	hl,(CLUSNU)
		call	F16P01
		ccf
		ex	de,hl
		ret	c			; is the end cluster, quit
		ex	de,hl
		call	GETFAT			; get FAT entry content
		ld	(CLUSNU),hl		; new current cluster of file
		ld	hl,(LASTPO)
		inc	hl
		ld	(LASTPO),hl		; new current relative cluster of file
		ld	a,$ff			; relative sector in cluster 0
_nextpart1:	inc	a
		ld	(SECCLU),a
		or	a
		ret

; Finish CON read
ConReadFinish:	ld	a,(hl)
		ldi
		cp	CR
		jr	nz,_conrf1
		ld	(hl),LF
_conrf1:	cp	LF
		jr	z,_devrf1
		ld	a,b
		or	c
		jr	nz,ConReadFinish
_conrf2:	ld	(CONTPO),hl

; finish read record for dos devices
DevReadFinish:	ld	(NEXTAD),de		; update current transferaddress
		jp	nz,_readrec5
		res	6,(iy+24)
		jp	_readrec5

_devrf1:	call	DosConout		; console output
		ld	hl,$0000
		ld	a,c
		or	b
		jr	nz,_conreadrec1
		inc	a
		jr	_conrf2

; Subroutine read record for dos devices
DevReadRec:	ld	de,(NEXTAD)		; current transferaddress
		inc	a
		jr	z,ConReadRec		; CON, handle
		inc	a
		jr	nz,DevReadFinish	; PRN, quit

; read record AUX
AuxReadRec:	call	DOS_READER		; auxiliary input
		ld	(de),a
		inc	de
		cp	$1a			; CTRL-Z?
		jr	z,DevReadFinish		; z=yes
		dec	bc
		ld	a,b
		or	c			; all bytes done ?
		jr	nz,AuxReadRec		; nz=no, next byte
		inc	a
		jr	DevReadFinish

; read record CON
ConReadRec:	ld	hl,(CONTPO)
		ld	a,h
		or	l
		jr	nz,ConReadFinish
_conreadrec1:		ld	hl,127
		ld	a,(YCONBF+0)
		cp	l
		jr	z,_conreadrec2
		ld	(YCONBF+0),hl
_conreadrec2:	push	bc
		push	de
		ld	de,YCONBF
		call	DOS_BUFIN
		pop	de
		pop	bc
		ld	hl,YCONBF+2
		ld	a,(hl)
		cp	$1a
		jr	nz,ConReadFinish
		ld	(de),a
		inc	de
		ld	a,LF
		call	DosConout		; console output
		xor	a
		ld	h,a
		ld	l,a
		jr	_conrf2

; Subroutine read record
; Input:  de:bc = record number
;         hl    = number of records
; Output: de:hl = last record done
;         bc    = number of records done

ReadRecord:	call	_zwrite9		; initialize record info
		jp	m,DevReadRec		; dos device, special action
		ld	l,(iy+16)
		ld	h,(iy+17)
		ld	de,(BYTPOS+0)
		or	a
		sbc	hl,de
		push	hl
		ld	l,(iy+18)
		ld	h,(iy+19)
		ld	de,(BYTPOS+2)
		sbc	hl,de
		pop	hl
		jp	c,_devwr6		; startbyte behind end of file, quit with nothing done
		jr	nz,_readrec1		; startbyte at least 65536 bytes from the end of file, go get it
		ld	a,h
		or	l
		jp	z,_devwr6		; startbyte is at end of file, quit with nothing done
		push	hl
		sbc	hl,bc			; requested number of bytes past file ?
		pop	hl
		jr	nc,_readrec1		; nc=no, go get it
		ld	b,h
		ld	c,l			; only read number of bytes until the end of file
_readrec1:	call	H_ENUF
		call	CalcPartSec		; calculate partial sector transfers
		ld	bc,(CLUSNU)		; relative cluster
		call	GetAbsClus		; get absolute cluster
		ld	a,b
		or	c			; found ?
		jp	nz,_devwr6		; nz=no, quit with nothing done
		ld	(CLUSNU),hl		; current cluster = cluster of startbyte
		ld	(LASTPO),de		; current relative cluster = relative cluster of startbyte
		call	ReadPartSec		; do partial sector read if needed
		ld	hl,(SECCNT)
		ld	a,h
		or	l
		jp	z,_readrec4		; not any whole sectors to transfer, to partial end
		call	NextPartSec		; to next sector (only when partial read was done)
		jr	c,_readrec5		; there is no next,
		ld	a,1
		ld	(TRANS),a		; flag do increase sector
		ld	a,(SECCLU)		; current relative sector in cluster
		ld	bc,(SECCNT)
		ld	hl,(CLUSNU)		; current cluster of file
_readrec2:	push	bc
		call	CalcSeqSec		; calculate sequential sectors
		push	bc
		push	af
		ld	b,a
		call	DiskReadSec		; read sectors with DOS error handling
		pop	af
		ld	c,a
		ld	b,$00			; number of sectors read
		jr	c,_readrec3		; sectors read does not include the sector in the datasector buffer
		ld	a,(DIRTYB)
		or	a			; datasector buffer dirty ?
		jr	z,_readrec3		; z=no, then no need to transfer the datasector buffer
		push	bc
		ld	c,(ix+2)
		ld	b,(ix+3)		; sectorsize
		push	bc
		push	hl
		ld	hl,(BUFSEC)		; sectornumber of datasector buffer
		sbc	hl,de
		ex	de,hl
		call	DosMultiply		; multiply
		pop	hl
		add	hl,bc
		pop	bc
		ex	de,hl
		ld	hl,(SBUFFE)		; datasector buffer
		ldir				; BLKMOV
		pop	bc
_readrec3:	pop	de
		pop	hl
		or	a
		sbc	hl,bc			; done all whole sectors ?
		jr	z,_readrec4		; z=yes, go partial end
		ld	c,l
		ld	b,h
		ex	de,hl
		call	F16P01
		ex	de,hl
		jr	nc,_readrec5
		ld	hl,(LASTPO)
		inc	hl
		ld	(LASTPO),hl		; increase current relarive cluster
		xor	a			; current relative sector in cluster = first sector
		ex	de,hl
		jr	_readrec2		; again

_readrec4:	call	LastPartSec		; last partial sector ?
		call	nc,ReadPartSec		; yes, do partial sector read if needed
_readrec5:	call	H_SETF
		ld	hl,(NEXTAD)		; current transferaddress (end)
		ld	de,(DMAADD)		; transferaddress (begin)
		or	a
		sbc	hl,de
		ld	c,l
		ld	b,h			; size of transfer
		ld	de,$0080
		ld	a,(CPMCAL)
		or	a			; Random Block
		jr	nz,_readrec6		; nz=no, use 128 bytes recordsize
		ld	e,(iy+14)
		ld	d,(iy+15)		; user recordsize for Random Block
_readrec6:	call	DIV16			; how many records ?
		ld	a,h
		or	l			; partial records ?
		jr	z,_readrec8			; z=no
		inc	bc			; records +1
		ex	de,hl
		sbc	hl,de			; 'missed' bytes
		ld	de,(NEXTAD)
_readrec7:	xor	a
		ld	(de),a
		inc	de
		dec	hl
		ld	a,h
		or	l
		jr	nz,_readrec7		; clear 'missed' bytes
_readrec8:	ld	hl,(RECCNT)		; number of records requested
		sbc	hl,bc
		jr	z,_readrec9		; all done,
		inc	a
		ld	(DSKERR),a		; error in record operation
_readrec9:	call	H_SETC
		ld	hl,(CLUSNU)
		ld	(iy+28),l
		ld	(iy+29),h		; current cluster of file FCB
		ld	hl,(LASTPO)
		ld	(iy+30),l
		ld	(iy+31),h		; current relative cluster of file FCB
_readrec10:	ld	hl,(RECPOS+0)
		ld	de,(RECPOS+2)		; startrecord
		ld	a,b
		or	c			; done any records ?
		ret	z			; z=no, quit
		dec	bc
		add	hl,bc
		inc	bc
		ret	nc
		inc	de			; return current record
		ret

;Subroutine write record for dos devices
DevWriteRec:	ld	hl,(DMAADD)		; transfer address
		or	$40
		inc	a
		jr	z,_devwr3		; CON, handle
		inc	a
		jr	z,_devwr2		; AUX, handle
		inc	a
_devwr1:	jr	z,_devwr4		; NUL, handle

		ld	a,(hl)
		inc	hl
		cp	$1a
		jr	z,_devwr4
		call	_lstout1		; printer output
		dec	bc
		ld	a,b
		or	c
		jr	_devwr1

_devwr2:	ld	a,(hl)
		inc	hl
		call	_auxout1		; auliary output
		cp	$1a
		jr	z,_devwr4
		dec	bc
		ld	a,b
		or	c
		jr	nz,_devwr2
		jr	_devwr4

_devwr3:	ld	a,(hl)
		inc	hl
		cp	$1a
		jr	z,_devwr4
		call	DosConout		; console output
		dec	bc
		ld	a,b
		or	c
		jr	nz,_devwr3
_devwr4:	ld	bc,(RECCNT)		; no. of records
		jr	_readrec10

_devwr5:	ld	c,e
		ld	b,d			; clusters to skip
		call	_absclus2		; get next absolute cluster
		ld	a,b
		or	c			; found ?
		jp	z,_writerec5		; z=yes
		call	ClusChainAlloc		; allocate cluster chain
		jp	nc,_writerec5		; ok, go writing

_devwr6:	call	H_WRTE
		xor	a
		ld	c,a
		ld	b,a			; no records read/write
		inc	a
		ld	(DSKERR),a		; error in record operation
		jr	_readrec10

; Subroutine write record
; Input: de:bc = record number
;        hl    = number of records
WriteRecord:	call	_zwrite9		; initialize record info
		push	af
		push	bc
		call	DirDateTime		; get time and date (dirformat)
		ld	(iy+20),c
		ld	(iy+21),b
		ld	(iy+22),e
		ld	(iy+23),d
		pop	bc
		pop	af
		jp	m,DevWriteRec		; DOS device, special action
		res	6,(iy+24)		; flag FCB changed
		push	bc
		call	CalcPartSec		; calculate partial sector transfers
		pop	bc
		ld	hl,(BYTPOS+0)
		ld	de,(BYTPOS+2)		; startbyte
		ld	a,b
		or	c			; zero bytes to write (only possible with Random Block) ?
		jp	z,FileSizeAdjust	; z=yes, filesize adjust action
		dec	bc
		add	hl,bc
		jr	nc,_writerec1
		inc	de			; endbyte
_writerec1:	ld	b,h
		ld	c,l
		ex	de,hl
		ld	e,(ix+2)
		ld	d,(ix+3)
		call	Divide			; / sectorsize
		ld	h,b
		ld	l,c			; relative sector of endbyte
		ld	b,(ix+7)		; clustershift
		dec	b
		jr	z,_writerec3
_writerec2:	srl	h
		rr	l
		djnz	_writerec2		; relative cluster of endbyte
_writerec3:	push	hl
		ld	c,(iy+16)
		ld	b,(iy+17)
		ld	l,(iy+18)
		ld	h,(iy+19)		; filesize
		call	Divide			; / sectorsize
		ld	a,h
		or	l			; offset in sector
		jr	z,_writerec4
		inc	bc			; relative sector
_writerec4:	call	H_NORN
		ld	(VALSEC),bc		; relative sector behind fileend
		ld	bc,(CLUSNU)		; relative cluster of startbyte
		call	GetAbsClus		; get absolute cluster
		ld	(CLUSNU),hl		; current cluster = cluster of startbyte
		ld	(LASTPO),de		; current relative cluster = relative cluster of startbyte
		ex	(sp),hl
		or	a
		sbc	hl,de			; start and endbyte in same cluster ?
		ex	de,hl
		pop	hl
		jr	z,_writerec5		; z=yes
		ld	a,b
		or	c			; is cluster of startbyte found ?
		jp	z,_devwr5			; z=yes, make chain to cluster of endbyte if needed and start writing
		push	bc
		ld	c,e
		ld	b,d			; clusters to allocate
		call	ClusChainAlloc		; allocate cluster chain
		pop	bc
		jp	c,_devwr6		; failed, quit with nothing done
		ld	de,(LASTPO)
		inc	de			; relative cluster to start
		dec	bc			; clusters to skip
		call	_absclus2		; get next absolute cluster
		ld	(CLUSNU),hl		; cluster of startbyte
		ld	(LASTPO),de		; relative cluster of startbyte
_writerec5:	call	WritePartSec		; handle partial sector write
		ld	hl,(SECCNT)
		ld	a,h
		or	l			; any complete sectors ?
		jr	z,_writerec8		; z=no, goto partial end
		ld	de,(SECPOS)
		add	hl,de
		ld	(SECPOS),hl		; update relative sector of startbyte
		call	NextPartSec		; to the next sector (only when partial write was done)
		ld	a,1
		ld	(TRANS),a		; flag do increase sector
		ld	a,(SECCLU)		; current relative sector in cluster
		ld	hl,(CLUSNU)		; relative cluster
		ld	bc,(SECCNT)		; whole sectors
_writerec6:	push	bc
		call	CalcSeqSec		; calculate sequencial sectors
		push	bc
		push	af
		ld	b,a
		call	F16P07
_writerec7:	call	DiskWriteSec		; write datasectors with DOS error handling
		pop	af
		pop	de
		pop	hl
		ld	c,a
		xor	a
		ld	b,a
		sbc	hl,bc			; whole sectors left ?
		jr	z,_writerec8		; z=no, go to partial end
		ld	c,l
		ld	b,h
		ld	hl,(LASTPO)
		inc	hl
		ld	(LASTPO),hl		; update relative cluster
		ex	de,hl
		jr	_writerec6		; again

_writerec8:	call	H_WRTL
		call	LastPartSec		; last partial sector ?
		call	nc,WritePartSec		; partial end, handle partial sector write
		ld	hl,(NEXTAD)		; current transferaddress
		ld	de,(DMAADD)		; transferaddress
		or	a
		sbc	hl,de
		ld	de,(BYTPOS+0)
		add	hl,de
		ld	de,(BYTPOS+2)
		jr	nc,_writerec9
		inc	de
_writerec9:	ld	(BYTPOS+0),hl
		ld	(BYTPOS+2),de		; startbyte = startbyte + transfersize
		ld	c,(iy+16)
		ld	b,(iy+17)
		or	a
		sbc	hl,bc
		ld	c,(iy+18)
		ld	b,(iy+19)
		ex	de,hl
		sbc	hl,bc			; has file expanded ?
		jr	c,_writerec11		; c=no
_writerec10:	push	iy
		pop	hl
		ld	de,$0010
		add	hl,de
		ex	de,hl
		ld	hl,BYTPOS		; filelength = endbyte
		ld	bc,4
		ldir
_writerec11:	ld	bc,(RECCNT)		; no. of records
		jp	_readrec9

; filesize adjust
FileSizeAdjust:	ld	a,h
		or	l
		or	d
		or	e			; startbyte zero ?
		jr	z,_filesa6		; z=yes, kill chain and quit
		ld	bc,1
		sbc	hl,bc
		ex	de,hl
		dec	bc
		sbc	hl,bc
		ld	b,d
		ld	c,e			; filesize = startbyte-1
		ld	e,(ix+2)
		ld	d,(ix+3)
		call	Divide			; / sectorsize
		ld	a,(ix+7)		; clustershift
_filesa1:	dec	a
		jr	z,_filesa2
		srl	b
		rr	c
		jr	_filesa1

_filesa2:	call	GetAbsClus		; get absolute cluster
		ld	a,b
		or	c			; found ?
		jr	z,_filesa4		; z=yes, this means chain must be shortend
		call	ClusChainAlloc		; allocate cluster chain
		jp	c,_devwr6		; failed, quit with nothing done
_filesa3:	ld	bc,0
		ld	(RECCNT),bc		; number of records = 0
		ld	(LASTPO),bc		; current relative cluster = 0
		ld	l,(iy+26)
		ld	h,(iy+27)		; start cluster of file
		ld	(CLUSNU),hl		; current cluster = start cluster of file
		jr	_writerec10

_filesa4:	ld	bc,$ffff
		call	ClusSetEntry		; mark end & release rest chain
_filesa5:	dec	de
		ld	a,1
		ld	(de),a			; flag FAT buffer changed
		jr	_filesa3

_filesa6:	ld	l,(iy+26)
		ld	h,(iy+27)
		ld	a,h
		or	l			; file has start cluster ?
		jr	z,_filesa3		; z=no
		xor	a
		ld	(iy+26),a
		ld	(iy+27),a		; file has no start cluster (empty file)
		call	ClusChainFree		; release cluster chain
		jr	_filesa5		; mark FAT buffer changed

; Subroutine calculate sequential sectors
CalcSeqSec:	call	H_OPTI
		ld	d,a
		push	hl
		inc	b
		dec	b
		jr	z,_calcs1
		ld	c,$ff
_calcs1:	ld	e,c
		push	de
		ld	a,(ix+6)		; clustermask
		ld	(SECCLU),a		; current relative sector in cluster
		inc	a
		sub	d
		ld	b,a
_calcs2:	ld	(CLUSNU),hl
		push	hl
		call	GETFAT			; get FAT entry content
		pop	de
		ld	a,c
		sub	b
		ld	c,a
		jr	z,_calcs3
		ld	b,(ix+6)		; clustermask
		jr	c,_calcs5
		inc	b
		inc	de
		ex	de,hl
		sbc	hl,de
		ex	de,hl
		jr	z,_calcs2
_calcs3:	pop	de
		ex	(sp),hl
		push	hl
		push	de
		ld	a,e
		sub	c
		ld	e,a
		ld	d,$00
		ld	c,(ix+2)
		ld	b,(ix+3)		; sectorsize
		call	DosMultiply		; multiply
		pop	af
		ld	hl,(NEXTAD)
		push	hl
		add	hl,bc
		ld	(NEXTAD),hl		; update current transferaddress
		pop	bc
		pop	hl
		push	bc
		push	de
		ex	de,hl
		ld	hl,(CLUSNU)
		sbc	hl,de
		ld	bc,(LASTPO)
		add	hl,bc
		ld	(LASTPO),hl
		ex	de,hl
		call	ClusSecNum		; get sectornumber of cluster
		ex	de,hl
		pop	bc
		ld	a,(BUFDRN)
		cp	(ix+0)			; driveid
		ld	a,c
		scf
		jr	nz,_calcs4
		call	F16P06
		ld	a,c
		jr	c,_calcs4
		ld	h,b
		ld	l,c
		add	hl,de
		dec	hl
		ld	bc,(BUFSEC)		; sectornumber of datasector buffer
		sbc	hl,bc
_calcs4:	pop	hl
		pop	bc
		ret

_calcs5:	add	a,b
		ld	(SECCLU),a		; current relative sector in cluster
		ld	c,$00
		jr	_calcs3

; Subroutine get sectornumber of cluster
; Input:  hl = cluster
;         a  = relative sector in cluster
; Output: hl = sectornumber
ClusSecNum:	call	H_FIGR
		push	bc
		ld	b,(ix+7)		; clustershift
		dec	hl
		dec	hl
		jp	F16P05
_clusn1:	sla	l
		rl	h
		djnz	_clusn1
_clusn2:	or	l
		ld	l,a
		ld	c,(ix+12)
		ld	b,(ix+13)
		add	hl,bc			; + first datasector
		pop	bc
		ret

; Subroutine get recordnumber from S2,EX and CR fields
GetRecNum:	push	de
		pop	iy
		ld	c,(iy+32)		; CR (current record)
		ld	b,(iy+12)		; EX (extent)
		ld	e,(iy+14)		; S2
		ld	d,0
		sla	c
		srl	e
		rr	b
		rr	c			; debc = recordnumber
		ld	hl,1			; 1 record
		ret

; Subroutine allocate cluster chain
ClusChainAlloc:	call	H_ALLO
		ld	e,(ix+14)
		ld	d,(ix+15)
		ld	(MAXCLS),de
		push	hl
r171:		push	bc
		push	hl
		ld	d,h
		ld	e,l
r170:		push	de
		ex	de,hl
		ld	hl,(MAXCLS)
		dec	hl
		or	a
		sbc	hl,de
		ex	de,hl
		pop	de
		jr	nc,r167
		ld	a,e
		or	d
		jr	nz,r168
		pop	hl
		pop	hl
		pop	hl
		ld	bc,$ffff
		ld	e,(ix+19)
		ld	d,(ix+20)
		call	FAT_write
		scf
		ret
r167:		inc	hl
		call	ClusFreeAlloc
		jp	r170
r168:
		dec	de
		ex	de,hl
		call	ClusFreeAlloc
		ex	de,hl
		jr	r170

; Subroutine allocate cluster if free
; Input:  hl = cluster number
ClusFreeAlloc:	push	hl			; store cluster number
		push	de			; store
		call	GETFAT			; cluster free ?
		pop	de			; restore
		pop	hl			; restore cluster number
		ret	nz			; nz=no, no alloc
		pop	bc			; discard return address
		ld	c,l
		ld	b,h			; store cluster number
		ex	(sp),hl			; store cluster number, restore previous cluster number
		ld	e,(ix+19)
		ld	d,(ix+20)		; pointer to FAT buffer of drive
		call	FAT_write
		pop	hl			; restore cluster number
		pop	bc			; restore number of clusters to allocate
		dec	bc			; update number of clusters to allocate
		ld	a,b
		or	c			; finished ?
		jr	nz,r171
		ld	bc,$ffff
		call	NewPutFAT
		dec	de
		ld	a,1
		ld	(de),a			; FAT changed
		pop	hl			; restore start cluster
		push	hl			; store start cluster
		call	FAT_read
		pop	bc			; restore start cluster
		ld	a,c
		or	b			; add chain to existing chain ?
		ret	nz
		ld	(iy+26),l
		ld	(iy+27),h		; update start cluster of file
		ret

; Subroutine release cluster chain
ClusChainFree:	ld	bc,0			; FAT entry = free

; Subroutine set cluster entry and release rest of cluster chain
ClusSetEntry:	call	H_RELB
		push	hl			; store cluster number
		call	GETFAT			; get FAT entry content
		ex	(sp),hl			; store next cluster number, restore cluster number
		call	NewPutFAT
		pop	hl			; restore next cluster number
		ld	a,h
		or	l			; free FAT entry ?
		ret	z			; z=yes, quit
		call	F16P01
		jr	c,ClusChainFree
		ret
		cp	$f8			; cluster chain end marker ?
		jr	c,ClusChainFree		; c=no, next cluster in chain
		ret

; ---------------------------------------------------------
; Function $11 SFIRST
; Search for first entry (fcb)
; ---------------------------------------------------------
DOS_SRCHFR:	call	ValFCB			; validate FCB, clear S2 and find direntry
_sfirst1:	jr	c,_sfirst5		; error, quit
		ld	a,(LASTEN)
		jr	z,_sfirst2		; file, save direntry number for search next
		ld	a,$ff			; device, flag search next invalid
_sfirst2:	ld	(SRCHLO),a
		ld	(SRCHIX),ix		; save pointer to DPB
		ld	de,(DMAADD)		; transferaddress
		ld	a,(THISDR)		; current driveid
		inc	a
		ld	(de),a
		inc	de
		ld	a,(hl)
		cp	$05
		jr	nz,_sfirst3
		ld	(hl),$e5
_sfirst3:	ld	bc,32
		ldir				; BLKMOV
		call	GetMaxRecExt		; get max record and extent
		ld	a,(FCBEXT)
		cp	b			; orginal FCB EX byte same as max extent ?
		jr	z,_sfirst4			; same, RC = max record
		jr	nc,_sfirst5		; bigger, quit with error
		ld	c,$80			; smaller, RC = 128 (means extend is full)
_sfirst4:	ld	hl,(DMAADD)		; transferaddress
		ld	de,$000C
		add	hl,de
		ld	b,(hl)			; MS-DOS fileattribute
		ld	(hl),a			; EX = orginal FCB EX byte (CP/M: requested extent)
		inc	hl
		ld	(hl),b			; S1 = MS-DOS fileattribute (CP/M: reserved)
		inc	hl
		ld	(hl),d			; S2 = 0 (CP/M: extent high byte)
		inc	hl
		ld	(hl),c			; RC = (CP/M: recordcount)
		xor	a			; CP/M direntry 0, no error
		ret

_sfirst5:	ld	a,$ff
		ld	(SRCHLO),a		; search for next invalid
		ret

; ---------------------------------------------------------
; Function $12 SNEXT
; Search for next entry (fcb)
; ---------------------------------------------------------
DOS_SRCHNX:	call	ValPath			; validate FCB drive and filename
		jr	c,_sfirst5		; invalid,
		ld	a,(SRCHLO)		; saved direntrynumber of last search first
		cp	$ff
		jr	z,_sfirst5		; flag search next invalid, quit with error
		ld	(LASTEN),a
		ld	ix,(SRCHIX)		; saved pointer to DPB
		call	FindNext		; find next directoryentry
		jr	_sfirst1		; finish

; ---------------------------------------------------------
; Function $23 FSIZE
; Get file size (fcb)
; ---------------------------------------------------------
DOS_FILESI:	call	ValFCB			; validate FCB, clear S2 and find direntry
		ld	a,$ff
		ret	c			; error, quit
		push	de
		pop	ix
		ld	a,(iy+28)
		ld	c,(iy+29)
		ld	b,(iy+30)
		ld	e,(iy+31)
		add	a,a
		rl	c
		rl	b
		rl	e			; convert filesize to random record
		or	a			; filesize a multiply of 128 ?
		jr	z,_fsize1		; z=yes
		inc	bc
		ld	a,b
		or	c
		jr	nz,_fsize1
		inc	e			; increase random record
_fsize1:	ld	(ix+33),c
		ld	(ix+34),b
		ld	(ix+35),e		; set R2,R1 and R0
		xor	a
		ret				; quit without error

; ---------------------------------------------------------
; Function $18 LOGIN
; Get login vector
; Output: hl = login vector
; ---------------------------------------------------------
DOS_LOGIN:	ld	a,(SNUMDR)
		ld	b,a
		xor	a
_login1:	scf
		rla
		djnz	_login1		 	; all drives online
		ret

; ---------------------------------------------------------
; Function $1A _SETDTA
; Input:  de = disk transfer address
; ---------------------------------------------------------
DOS_SETDMA:	ld	(DMAADD),de		; set transferaddress
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
DOS_GETEFA:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		ld	a,e
		call	ValDrive		; validate fcb driveid
		ld	a,$ff
		ret	c			; error, quit
		call	GetFatSec		; get latest FAT
		ld	e,(ix+19)
		ld	d,(ix+20)
		push	de
		pop	iy			; pointer to FAT buffer of drive
		ld	hl,2			; start at clusterentry 2
		ld	b,h
		ld	c,h			; free cluster = 0
		ld	e,(ix+14)
		ld	d,(ix+15)
		dec	de			; number of clusters on disk
		push	de
_alloc1:	push	de
		push	hl
		call	GETFAT			; get FAT entry content
		pop	hl
		pop	de
		jr	nz,_alloc2
		inc	bc			; free clusters + 1
_alloc2:	inc	hl
		dec	de
		ld	a,e
		or	d
		jr	nz,_alloc1		; next cluster
		ld	h,b
		ld	l,c			; number of free clusters
		pop	de			; number of clusters
		ld	a,(ix+6)
		inc	a			; number of sectors per cluster
		ld	c,(ix+2)
		ld	b,(ix+3)		; sectorsize
		ret

; ---------------------------------------------------------
; Function $0D DSKRST
; Disk reset
; ---------------------------------------------------------
DOS_DSKRES:	ld	hl,$0080
		ld	(DMAADD),hl		; default transferaddress
		xor	a
		ld	(CURDRV),a		; default driveid 0 (A:)

DOS_WRTFAT:	call	FlushDataBuf		; flush datasector buffer
		ld	hl,SDPBLI
		ld	a,(SNUMDR)		; all drives
_wrfat1:	ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to DPB
		inc	hl
		push	hl
		push	af
		push	de
		pop	ix
		call	FlushFatBuf		; flush FAT buffer
		pop	af
		pop	hl
		dec	a
		jr	nz,_wrfat1		; next drive
		ret

; ---------------------------------------------------------
; Function $19 CURDRV
; Get current drive
; ---------------------------------------------------------
DOS_GETDRV:	ld	a,(CURDRV)
		ret

; ---------------------------------------------------------
; Function $24 SETRND
; Set random record (fcb)
; ---------------------------------------------------------
DOS_SETRND:	call	GetRecNum		; get recordnumber from CR,EX and S2 field
		ld	(iy+33),l
		ld	(iy+34),h
		ld	(iy+35),e
		ret

; ---------------------------------------------------------
; Function $0E SELDSK
; Select disk
; Input:  e   = drive number (0=A, 1=B, etc.)
; Output: l,a = number of drives available
; ---------------------------------------------------------
DOS_SELDSK:	ld	a,(SNUMDR)
		cp	e
		ret	c
		ret	z
		ld	hl,CURDRV
		ld	(hl),e
		ret

; ---------------------------------------------------------
; Function $0A BUFIN
; Buffered console input
; ---------------------------------------------------------
DOS_BUFIN:	push	de
		ld	a,(CARPOS)
		ld	(STARTP),a		; save current console columnpos to record start of inputline
		xor	a
		ld	(INSERT),a		; not in insertmode
		ld	h,d
		ld	l,e
		ld	b,a
		ld	c,(hl)			; size of buffer
		inc	hl
		ld	d,a
		ld	e,(hl)			; length of line already in buffer
		inc	hl
		ld	ix,YCONTP
		ld	a,e
		cp	c			; is lengthbyte valid ?
		jr	nc,_bufin1		; equal, use the line in buffer as basis otherwise use empty line as basis
		push	hl			; length smaller than size of buffer
		add	hl,de
		ld	a,(hl)
		pop	hl
		cp	CR			; then line must be terminated by a CR
_bufin1:	jr	z,_bufin3		; it is, use the line in buffer as basis
_bufin2:	ld	e,d			; use empty line as basis

; line input main loop, also lineinput CTRL-F
_bufin3:	call	H_GETC			; hook
		call	DOS_IN
_bufin4:	push	hl
		push	bc
		ld	hl,KeyTab
		ld	bc,NKEYNT		; number of keyentries
		cpir
		add	hl,bc
		add	hl,bc
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,c
		pop	bc
		ex	(sp),hl
		ret

; lineinput CTRL-A, MSX graphic header
_bufin5:	call	DOS_IN
		cp	$40
		jr	c,_bufin4
		cp	$60
		jr	nc,_bufin4
		push	af
		ld	a,b
		inc	a
		cp	c
		jr	nc,_bufin7		; beep
		ld	a,1
		ld	(ix+0),a
		inc	ix
		inc	b
		call	DOS_BUFOUT
		pop	af


; line input, normal key action
_bufin6:	push	af
		ld	a,b
		cp	c
		jr	nc,_bufin7		; beep
		pop	af
		ld	(ix+0),a
		inc	ix
		inc	b
		call	DOS_BUFOUT
		ld	a,(INSERT)
		or	a			; insertmode ?
		jr	nz,_bufin3		; nz=yes
		inc	d
		ld	a,e
		cp	d
		jr	c,_bufin3
		ld	a,(hl)
		dec	a
		inc	hl
		jr	nz,_bufin3
		inc	d
		inc	hl
		jr	_bufin3

_bufin7:	pop	af
		ld	a,BELL
		call	DosConout		; console output
		jr	_bufin3

; line input UP key, ESC key, CTRL-U (VOID)
_bufin8:	pop	de
		ld	a,(STARTP)
		ld	b,a
		ld	a,(CARPOS)
		sub	b			; length of the inputline
		jr	z,_bufin10		; empty inputline, restart line input
		ld	b,a
_bufin9:	call	_bufin46
		djnz	_bufin9
_bufin10:	jp	DOS_BUFIN		; restart lineinput

; line input CTRL-J
_bufin11:	call	DOS_CRLF		; newline
_bufin12:	jr	_bufin3

DOS_CRLF:	ld	a,CR
		call	DosConout		; console output
		ld	a,LF
		jp	DosConout		; console output

; line input CR key
_bufin13:	pop	de
_bufin14:	call	DosConout		; console output
		push	de
		inc	de
		ld	a,b
		ld	(de),a
		cp	c
		push	af
		inc	de
		ld	c,b
		xor	a
		ld	b,a
		or	c
		jr	z,_bufin15
		ld	hl,YCONTP
		ldir
_bufin15:	pop	af
		jr	z,_bufin16
		ld	a,CR
		ld	(de),a
_bufin16:	pop	de
		ret

; line input LEFT key, BS key (BS)
_bufin17:	inc	b
		dec	b
		jr	z,_bufin19
		dec	b
		dec	ix
		call	_bufin46
		inc	b
		dec	b
		jr	z,_bufin18
		dec	b
		dec	ix
		ld	a,(ix+0)
		dec	a
		jr	z,_bufin19
		inc	b
		inc	ix
_bufin18:	ld	a,(ix+0)
		cp	SPACE
		jr	nc,_bufin19
		cp	TAB
		jr	z,_bufin20
		call	_bufin46
_bufin19:	ld	a,(INSERT)
		or	a			; insertmode ?
		jr	nz,_bufin12		; nz=yes
		inc	d
		dec	d
		jr	z,_bufin12
		dec	d
		ld	a,d
		cp	e
		jr	nc,_bufin12
		dec	hl
		ld	a,d
		cp	$01
		jr	c,_bufin12
		dec	hl
		ld	a,(hl)
		dec	a
		inc	hl
		jr	nz,_bufin12
		dec	d
		dec	hl
		jr	_bufin12

_bufin20:	push	hl
		push	bc
		ld	a,(STARTP)
		ld	c,a			; start of the inputline
		inc	b
		dec	b
		jr	z,_bufin23
		ld	hl,YCONTP
_bufin21:	ld	a,(hl)
		inc	hl
		cp	$01
		jr	z,_bufin22
		inc	c
		cp	SPACE
		jr	nc,_bufin22
		cp	TAB
		jr	z,_bufin26
		inc	c
_bufin22:	djnz	_bufin21
_bufin23:	ld	a,(CARPOS)		; current console columnpos
		sub	c
		jr	z,_bufin25
		ld	b,a
_bufin24:	call	_bufin46
		djnz	_bufin24
_bufin25:	pop	bc
		pop	hl
		jr	_bufin19

_bufin26:	ld	a,c
		add	a,$07
		and	$f8
		ld	c,a
		jr	_bufin22

; line input INS key (INSERT)
_bufin27:	ld	a,(INSERT)
		xor	$01
		ld	(INSERT),a
		jp	_bufin3

; line input HOME key (NEWLINE)
_bufin28:	ld	a,$8f
		pop	de
		call	_bufin14
		call	DOS_CRLF		; newline
		ld	a,(STARTP)
		or	a			; start of the inputline at the begin of a line ?
		jp	z,DOS_BUFIN		; z=yes, restart lineinput routine
		ld	b,a
		ld	a,' '
_bufin29:	call	DosConout		; console output
		djnz	_bufin29
		jp	DOS_BUFIN		; restart lineinput routine

; line input DOWN key (COPYALL)
_bufin30:	ld	a,$ff
		jr	_bufin35

; line input CTRL-L (SKIPUP)
_bufin31:	call	_bufin38
		jp	c,_bufin3
		push	bc
		ld	c,a
		ld	b,$00
		add	hl,bc
		pop	bc
		add	a,d
		ld	d,a
		jp	_bufin3

; line input SELECT key (COPYUP)
_bufin32:	call	_bufin38
		jp	c,_bufin3
		jr	_bufin35

; line input DEL key (SKIP1)
_bufin33:	ld	a,d
		cp	e
		jp	nc,_bufin3
		inc	d
		ld	a,(hl)
		dec	a
		inc	hl
		jp	nz,_bufin3
		inc	d
		inc	hl
		jp	_bufin3

; line input RIGHT key (COPY1)
_bufin34:	ld	a,(hl)
		dec	a
		ld	a,$01
		jr	nz,_bufin35
		inc	a
_bufin35:	push	af
		xor	a
		ld	(INSERT),a		; insertmode off
		ld	a,b
		cp	c
		jr	nc,_bufin37
		ld	a,d
		cp	e
		jr	nc,_bufin37
		ld	a,(hl)
		cp	$01
		jr	nz,_bufin36
		ld	a,b
		inc	a
		cp	c
		jr	nc,_bufin37
		ld	a,(hl)
_bufin36:	inc	hl
		ld	(ix+0),a
		inc	ix
		call	DOS_BUFOUT
		inc	b
		inc	d
		pop	af
		dec	a
		jr	nz,_bufin35
		jp	_bufin3

_bufin37:	pop	af
		jp	_bufin3

_bufin38:	call	DOS_IN
		cp	$01
		jr	nz,_bufin41
		call	DOS_IN
		cp	$40
		jr	c,_bufin41
		cp	$60
		jr	nc,_bufin41
		push	hl
		push	de
		push	bc
		ld	iy,$0000
_bufin39:	scf
		push	af
		ld	a,$01
		call	_bufin41
		jr	c,_bufin40
		ld	c,a
		ld	b,$00
		add	hl,bc
		add	a,d
		ld	d,a
		push	iy
		pop	af
		add	a,c
		push	af
		pop	iy
		inc	hl
		pop	af
		cp	(hl)
		dec	hl
		jr	nz,_bufin39
		push	iy
_bufin40:	pop	af
		pop	bc
		pop	de
		pop	hl
		ret

_bufin41:	push	bc
		push	af
		ld	a,e
		sub	d
		jr	c,_bufin44
		jr	z,_bufin44
		dec	a
		jr	z,_bufin44
		ld	c,a
		ld	b,$00
		pop	af
		push	hl
		push	af
		ld	a,(hl)
		dec	a
		jr	nz,_bufin42
		inc	hl
		dec	c
_bufin42:	pop	af
		inc	c
		dec	c
		jr	nz,_bufin43
		pop	hl
		pop	bc
		scf
		ret

_bufin43:	inc	hl
		cpir
		pop	hl
		jr	nz,_bufin45
		ld	a,e
		sub	d
		dec	a
		sub	c
		pop	bc
		ret

_bufin44:	pop	af
_bufin45:	pop	bc
		scf
		ret

_bufin46:	ld	a,BS
		call	DosConout
		ld	a,SPACE
		call	DosConout
		ld	a,BS
		jr	DosConout

DOS_BUFOUT:	cp	SPACE
		jr	nc,DosConout
		cp	TAB
		jr	z,DosConout
		cp	$01
		jr	z,DosConout
		push	af
		ld	a,'^'
		call	DosConout
		pop	af
		or	$40
		jr	DosConout

; keytable line input
; first table contains all keycodes, code 8 at the end is a fake one for 'other key', because it is already in the table
; second table contains all serviceroutines, but in reserve order (so last one belongs to the first keycode)

KeyTab:		db	$06,$7f,$08,$0d,$0a,$15,$0b,$0c
		db	$01b,$12,$18,$1c,$1d,$1e,$1f,$01
		db	$08

		dw	_bufin6
		dw	_bufin5,_bufin30,_bufin8,_bufin17,_bufin34,_bufin32,_bufin27,_bufin8
		dw	_bufin31,_bufin28,_bufin8,_bufin11,_bufin13,_bufin17,_bufin33,_bufin3
TKEYNT:
NKEYNT		equ	(TKEYNT-KeyTab)/3

; ---------------------------------------------------------
; Function $02 CONOUT
; Console output
; ---------------------------------------------------------
DOS_CONOUT:	ld	a,e
DosConout:	call	H_OUT
		cp	HOME
		jr	z,_conout2
		cp	FF
		jr	z,_conout2
		cp	RIGHT
		jr	z,_conout1
		cp	LEFT
		jr	z,_conout3
		cp	CR
		jr	z,_conout2
		cp	BS
		jr	z,_conout3
		cp	TAB
		jr	z,_conout4
		cp	DEL
		jr	z,_conout3
		cp	SPACE
		jr	c,_conout1
		push	hl
		ld	hl,CARPOS
		inc	(hl)			; increase console columnpos
		pop	hl
_conout1:	push	bc
		ld	b,a
		call	_conout6
		ld	a,b
		call	DOS_SOUT
		ld	a,(PFLAG)
		or	a			; console output also to printer ?
		ld	a,b
		pop	bc
		ret	z			; z=no, quit
		jp	K_LPTOUT		; output to printer

_conout2:	push	af
		xor	a
		ld	(CARPOS),a		; console columpos
		pop	af
		jr	_conout1

_conout3:	push	hl
		ld	hl,CARPOS
		dec	(hl)			; decrease console columnpos
		pop	hl
		jr	_conout1

_conout4:	ld	a,' '
		call	DosConout		; console output
		ld	a,(CARPOS)
		and	$07
		jr	nz,_conout4		; to the next console tabposition
		ret

_conout5:	cp	$10			; CTRL-P
		jr	z,_conout7
		cp	$0e			; CTRL-N
		jr	z,_conout7
		cp	$03			; CTRL-C
		jr	z,_conout7
		ret

_conout6:	call	DOS_SSBIOS		; check if keyboardinput available
		ret	z			; z=no, quit
		cp	$13			; CTRL-S ?
		jr	nz,_conout5		; z=no, check other specials
		call	DOS_SIN			; get keyboardinput (the CTRL-S) then next wait for other consoleinput
_conout7:	call	DOS_SIN			; get keyboardinput
		cp	$10			; CTRL-P?
		jr	z,_conout8		; z=yes, enable printer output
		cp	$0e			; CTRL-N?
		jr	z,_conout9		; z=yes, disable printer output
		cp	$03			; CTRL-C ?
		ret	nz			; nz=no, quit
		ld	hl,(BREAKV)
		jp	JPHL			; start abort handler in DOS memory

_conout8:	ld	a,1
		ld	(PFLAG),a
		ret

_conout9:	xor	a
		ld	(PFLAG),a
		ret

; ---------------------------------------------------------
; Function $0B CONST
; Console status
; ---------------------------------------------------------
DOS_CONSTA:	call	_conout6
		ld	a,$00
		ret	z
		or	$ff
		ret

; ---------------------------------------------------------
; Function $01 CONIN
; Console input
; ---------------------------------------------------------
DOS_CONIN:	call	DOS_IN
		push	af
		call	DosConout		; console output
		pop	af
		ret

; ---------------------------------------------------------
; Function $08 INNOE
; Direct input
; ---------------------------------------------------------
DOS_IN:		call	_conout7
		jr	z,DOS_IN
		ret

; ---------------------------------------------------------
; Function $06 DIRIO
; Direct console i/o
; Input:  a = $ff for console input, a<>$ff for console output
; Output: a = character (console input)
; ---------------------------------------------------------
DOS_RAWIO:	ld	a,e
		cp	$ff			; console input ?
		jp	nz,DOS_SOUT		; console output, output to screen and quit
		call	DOS_SSBIOS		; check if keyboardinput available
		jp	nz,DOS_SIN		; nz=yes, get keyboardinput and quit
		xor	a
		ret

; ---------------------------------------------------------
; Function $07 DIRIN
; Direct input
; ---------------------------------------------------------
DOS_RAWINP:	jp	DOS_SIN			; get keyboardinput

; ---------------------------------------------------------
; Function $05 LSTOUT
; Printer output
; ---------------------------------------------------------
DOS_LIST:	ld	a,e
_lstout1:	push	af
		call	_conout6
		pop	af
		jp	K_LPTOUT		; output to printer

; ---------------------------------------------------------
; Function $03 _AUXIN
; Auxiliary input
; ---------------------------------------------------------
DOS_READER:	call	_conout6
		jp	SAUXIN

; ---------------------------------------------------------
; Function $04 AUXOUT
; Auxiliary ouput
; ---------------------------------------------------------
DOS_PUNCH:	ld	a,e
_auxout1:	push	af
		call	_conout6
		pop	af
		jp	SAUXOU

; No secret message

; Get date and time in directory format
DirDateTime:	call	H_DATE
		call	GetTimeStamp		; get time and date values
		ld	a,c
		add	a,a
		add	a,a
		add	a,a
		rl	b
		add	a,a
		rl	b
		add	a,a
		rl	b
		srl	d
		or	d
		ld	e,a
		ld	d,b
		ld	bc,(MONTH)
		ld	a,c
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		rl	b
		ld	c,a
		ld	a,(DAY)			; current day
		or	c
		ld	c,a
		ret

GetTimeStamp:	call	GetDateTime		; get date and time from clockchip
		jr	c,_timestamp3
		push	de
		push	hl
		ld	de,(DAYCNT)		; days since 1-1-1980
		or	a
		sbc	hl,de
		pop	hl
		pop	de
		ret	z
		ld	(DAYCNT),hl
		push	bc
		push	de
		ld	c,l
		ld	b,h
		ld	de,4*365+1
		call	DIV16			; divide
		ld	a,c
		add	a,a
		add	a,a
		add	a,a
		ld	b,$00
		ld	de,TimeTab
		call	_timestamp4
		srl	a
		jr	nc,_timestamp1
		ld	de,200
		add	hl,de
_timestamp1:	call	_timestamp6		; setup days in februari
		ld	a,$01
		ld	de,MONTAB
		call	_timestamp4
		ld	(MONTH),a
		inc	l
		ld	a,l
		ld	(DAY),a		 	; current day
_timestamp2:	call	_setdat2
		pop	de
		pop	bc
		ret

_timestamp3:	push	bc
		push	de
		ld	hl,(YEAR)
		call	_setdat4		; calculate days since 1-1-1980
		jr	_timestamp2

_timestamp4:	ex	de,hl
		ld	c,(hl)
		inc	hl
		ex	de,hl
		or	a
		sbc	hl,bc
		jr	c,_timestamp5
		inc	a
		jr	_timestamp4

_timestamp5:	add	hl,bc
		ret

_timestamp6:	call	H_SETY
		ld	(YEAR),a		; year (offset)
_timestamp7:	and	$03
		ld	a,28
		jr	nz,_timestamp8
		inc	a
_timestamp8:	ld	(MONTAB+1),a
		ret

TimeTab:	db	200,166,200,165,200,165,200,165

; ---------------------------------------------------------
; Function $2A GDATE
; Get date
; Output: hl = year 1980...2079
;         d  = month (1=Jan...12=Dec)
;         e  = date (1...31)
;         c  = day of week (0=Sun...6=Sat)
; ---------------------------------------------------------
DOS_GETDAT:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		call	GetTimeStamp		; get time and date values
		ld	hl,(YEAR)
		ld	de,1980
		add	hl,de
		ld	de,(DAY)		; current day and month
		ld	a,(WEEKDA)
		ret

; ---------------------------------------------------------
; Function $2B SDATE
; Set date
; Input:  hl = year 1980...2079
;         d  = month (1=Jan...12=Dec)
;         e  = date (1...31)
; Output: a  = $00: valid date $be: invalid date
;         c  = $00: valid date $ff: invalid date
DOS_SETDAT:	ld	bc,-1980
		add	hl,bc
		jr	nc,_setdat3		; year <1980, error
		ld	a,h
		or	a
		jr	nz,_setdat3		; yearoffset not in 1 byte, error
		ld	a,l
		cp	120
		jr	nc,_setdat3		; year >2099, error
		call	_timestamp7		; setup febuari days
		inc	e
		dec	e
		jr	z,_setdat3		; day 0, error
		ld	a,d
		or	a
		jr	z,_setdat3		; month 0, error
		cp	12+1
		jr	nc,_setdat3		; month >12, error
		push	hl
		ld	hl,MONTAB-1
		add	a,l
		ld	l,a
		jr	nc,_setdat1
		inc	h
_setdat1:	ld	a,(hl)			; days in month
		pop	hl
		cp	e
		jr	c,_setdat3		; invalid day, error
		ld	(DAY),de		; current day and month
		call	_setdat4		; calculate days since 1-1-1980
		call	StoreDate		; store date (clockchip or otherwise)
_setdat2:	ld	bc,(DAYCNT)		; days since 1-1-1980
		ld	de,7
		inc	bc
		inc	bc
		call	DIV16			; divide
		ld	a,l
		ld	(WEEKDA),a
		xor	a
		ret

_setdat3:	ld	a,$ff
		ret

_setdat4:	ld	a,l			; year (offset)
		call	_timestamp6		; setup days in februari
		ld	c,l
		srl	c
		srl	c			; /4
		ld	b,0
		ld	de,4*365+1
		call	DosMultiply		; multiply
		ld	l,c
		ld	h,b
		ld	a,(YEAR)		; year (offset)
		and	$03
		add	a,a
		ld	de,TimeTab
		ld	b,0
		inc	a
		call	_setdat5
		ld	de,MONTAB
		ld	a,(MONTH)		; current month
		call	_setdat5
		ld	a,(DAY)			; current day
		dec	a
		ld	c,a
		add	hl,bc
		ld	(DAYCNT),hl		; days since 1-1-1980
		ret

_setdat5:	dec	a
		ret	z
		ex	de,hl
		ld	c,(hl)
		inc	hl
		ex	de,hl
		add	hl,bc
		jr	_setdat5

; ---------------------------------------------------------
; Function $2C GTIME
; Get time
; Output: h   = hours (0...23)
;         c,l = minutes (0...59)
;         d   = seconds (0...59)
;         e   = centiseconds (always zero)
; ---------------------------------------------------------
DOS_GETTIM:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		call	GetTimeStamp		; get time and date values
		ld	h,b
		ld	l,c
		xor	a
		ret

; ---------------------------------------------------------
; Function $2D STIME
; Set time
; Input:  h = hours (0...23)
;         l = minutes (0...59)
;         d = seconds (0...59)
;         e = centiseconds (ignored)
; Output: a = $00: valid time $bd: invalid time
;         c = $00: valid time $ff: invalid time
; ---------------------------------------------------------
DOS_SETTIM:	ld	b,h
		ld	c,l
		ld	a,b
		cp	24
		jr	nc,_setdat3
		ld	a,59
		cp	c
		jr	c,_setdat3
		cp	d
		jr	c,_setdat3
		ld	a,e
		cp	100
		jr	nc,_setdat3
		call	StoreTime		; store time (clockchip or otherwise)
		xor	a
		ret

; ---------------------------------------------------------
; Function $2E VERIFY
; Set/reset verify flag
; Input:  e = verify flag
; ---------------------------------------------------------
DOS_SETRAW:	ld	a,e
		ld	(RAWFLG),a
		ret

; ---------------------------------------------------------

; Subroutine validate FCB filename
; Input:  hl = address of FCB+1
;         de = destination
DosValFCB:	ld	a,(hl)
		cp	' '
		scf
		ret	z			; filename that start with a space is illegal, quit
		ld	bc,$0802		; first do the filename, then the fileextension
		cp	$e5
		jr	nz,_valfcb1		; not the charcode that is used as deleted file marker
		ld	a,$05
		ld	(de),a
		inc	hl
		inc	de
		dec	b			; use replacement charcode 0x05, otherwise file entry looks deleted
		ld	a,$e5
		call	ValKanjiChar		; is this a double byte 'header' char ?
		jr	nc,_valfcb1		; nc=no, no special action
		ld	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		dec	b			; copy 'follow' char
_valfcb1:	ld	a,(hl)
		call	ValKanjiChar		; is this a double byte 'header' char ?
		jr	nc,_valfcb2		; nc=no, do upcasing and check
		ld	(de),a
		inc	hl
		inc	de			; copy 'header' char
		dec	b
		scf
		ret	z			; no 'follow' char, quit with error
		ld	a,(hl)
		jr	_valfcb5		; copy 'follow' char and continue

_valfcb2:	ld	a,(COUNTR)
		and	a
		ld	a,(hl)
		jr	z,_valfcb3		; japanese have no accent chars,
		cp	$80
		jr	c,_valfcb3		; normal ASCII
		cp	$ba
		jr	nc,_valfcb3
		push	hl			; 0x80-0xB9 accent chars
		push	bc
		ld	c,a
		ld	b,$00
		ld	hl,UpcaseCharTab-$80
		add	hl,bc
		ld	a,(hl)			; get the upcase version of the accent char
		pop	bc
		pop	hl
_valfcb3:	cp	'a'
		jr	c,_valfcb4
		cp	'z'+1
		jr	nc,_valfcb4
		sub	$20			; lowercase char, make upcase
_valfcb4:	cp	$20
		ret	c			; control codes not allowed, quit with error
		push	hl
		push	bc
		ld	hl,IllegalCharTab
		ld	bc,$000a
		cpir				; one of the illegal chars ?
		pop	bc
		pop	hl
		scf
		ret	z			; z=yes, quit with error
_valfcb5:	ld	(de),a			; copy char
		inc	hl
		inc	de
		djnz	_valfcb1		; next char
		ld	b,$03
		dec	c
		jr	nz,_valfcb1		; now do the file extension
		or	a			; flag no error
		ld	a,(hl)
		ld	(FCBEXT),a		; save the FCB EX byte
		ret

IllegalCharTab:	db	".\"/[]:+=;,"		; c-escape for " char

; Subroutine check if double byte header char
ValKanjiChar:	push	hl
		ld	hl,KANJTA
		cp	(hl)
		ccf
		jr	nc,_valkc1		; below (F30F), quit (not in range)
		inc	hl
		cp	(hl)
		jr	c,_valkc1		; below (F310), quit (in range 1)
		inc	hl
		cp	(hl)
		ccf
		jr	nc,_valkc1		; below (F311), quit (not in range)
		inc	hl
		cp	(hl)
_valkc1:	pop	hl
		ret

; Table $80-0B9H accent upcase chars
UpcaseCharTab:	db	$80,$9A,"E","A",$8E,"A",$8F,$80
		db	"E","E","E","I","I","I",$8E,$8F
		db	$90,$92,$92,"O",$99,"O","U","U"
		db	"Y",$99,$9A,$9B,$9C,$9D,$9E,$9F
		db	"A","I","O","U",$A5,$A5,$A6,$A7
		db	$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF
		db	$B0,$B0,$B2,$B2,$b4,$b4,$B6,$B6
		db	$B8,$B8

; ------------------------------------------------------------------------------
; *** Kernel subroutines ***
; ------------------------------------------------------------------------------

; ---------------------------------------
; Get slots
; ---------------------------------------

; Subroutine get slotid of page 3
; Output: a = slotid
DosGetSlotP3:	ld	b,6
		db	$021			; LD HL,xxxx (skips next instruction)

; Subroutine get slotid of page 2
; Output: a = slotid
DosGetSlotP2:	ld	b,4
		call	RSLREG
		push	bc
r101:		rrca
		djnz	r101
		call	r103
		pop	bc
		or	(hl)
		ld	c,a
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)
		dec	b
		dec	b
r102:		rrca
		djnz	r102
		and	$0c
		or	c
		ret

r103:		and	$03
		ld	hl,EXPTBL
r104:		ld	b,$00
		ld	c,a
		add	hl,bc
		ret

; Subroutine: get my disk hardware driver slotid
; not used / return 0 to match to value in DRVTBL.
GETSLT:		xor	a
		ret

; Subroutine: get my disk hardware driver workarea
; Output: HL = IX = pointer to workarea
; Remark: used by the disk interface handler and driver
GETWRK:		ld	hl,(MYWORK)
		push	hl
		pop	ix
		ret

; Subroutine read disksector
; Input: a  = driveid
;        b  = number of sectors
;        c  = mediadescriptor
;        de = start sector
;        hl = transferaddress
ReadSector:	and	a
		jr	DosDiskIO

; Subroutine write disksector
WriteSector:	scf

; Subroutine PHYDIO BIOS call (H_PHYD)
DosDiskIO:	; Convert BEER 24-bit sector number method to FAT16 / Nextor 23-bit sector number method
		push    af
		ld      a,d
		and     e
		inc     a
		jr	nz,r60551		; sector number <> ffff is 16-bit
		ld	de,(F16LOSEC)		; load sector number bit 0..15
		ld	a,(F16HISEC)		; load sector number bit 16..22
		and	$07f			; make sure that 16-bit flag is reset
		ld	c,a
r60551: 	pop     af
		push    ix
		ld      ix,DSKIO
		jr      CallDriver

DiskChange:	push    ix
		ld      ix,DSKCHG
		jr      CallDriver

DiskGetDPB:	push    ix
		ld      ix,GETDPB

; call driver directly since there is only 1 interface
CallDriver:	push    iy
		push    hl
		ld      (TARGET),a		; save driveid (for PROMPT)
		di
		call    CLPRM1			; jp (ix)
		pop     hl
		pop     iy
		pop     ix
		ei
		ret


; Subroutine mark FAT buffer as invalid
; Input: a = driveid
DosInvalidFbuf:	ld	hl,SDPBLI
		call	r104
		add	hl,bc
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to DPB
		ld	hl,$0013
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		dec	de
		ex	de,hl
		ld	(hl),$ff
		ret

; ------------------------------------------------------------------------------
; Multi-FAT swapper (SOLiD)
; ------------------------------------------------------------------------------

FAT_read:	ld 	a,h
		or 	l
		jp 	nz,GETFAT
		ld 	hl,(FATSWAP1)		; see FAT_write
		ret

NewGetFAT:	call	H_UNPA
		call	FAT_Swapper
		ld	a,(ix+15)
		cp	$10
		jp	nc,F16P03
		jp	OldGetFAT

FAT_write:	ld	a,h
		or	l
		jr 	nz,NewPutFAT
		ld 	(FATSWAP1),bc		; see FAT_read
		ret

NewPutFAT:	call	FAT_Swapper
		dec	de
		ld	a,$01
		ld	(de),a
		inc	de
		jp	PutFAT

FAT_Swapper:	push	hl
		ld	hl,(FATSWAP4)		; Is this FAT in common buffer
		ld	e,(ix+19)
		ld	d,(ix+20)
		or	a
		sbc	hl,de
		pop	hl
		ret	nz			; No -> do no swapping
		ld	a,(FATSWAP2)		; Is current's drive FAT buffered?
		cp 	(ix+0)
		jr	z,r569
		call	SWAFAT			; if no -> swap immediately
		jr	r570

r569:		call	F16P04
		push	hl
		ld	hl,FATSWAP3
		cp 	(hl)
		pop	hl			; compare to block # in buffer
		call	nz,SWAFAT 		; If no match -> swap FATS
r570:		ld	a,(ix+15)
		cp	$10
		ld	a,h
		jr	nc,A746F
		and 	$03
		ld	h,a			; mask FAT entry addres to block size
		ret

A746F:		sub	$03
		jr	nc,A746F
		add	a,$03
		ld	h,a
		ret

; ------------------------------------------
; Swapping FAT buffers
; ------------------------------------------
SWAFAT:		push	hl
		call	SaveFATbuf
		pop	hl
ReadFATbuf:	ld	a,(ix+0)
		ld	(FATSWAP2),a		; FAT's drive
		call	F16P04
		ld	(FATSWAP3),a		; FAT's block no
		push	hl
		push	de
		push	bc
		call	GetFATbuf
		ld	b,a
		ld	a,(FATSWAP3)
		ld	c,a
		add	a,a
		add	a,c
		add	a,e
		ld	e,a
		jr	nc,r571
		inc	d
r571:		push	hl
		push	de
		push	bc
		ld	b,3
		call	DiskReadSect
		pop	bc
		pop	de
		pop	hl
		jr	nc,r572
		ld	a,(ix+16)
		add	a,e
		ld	e,a
		jr	nc,r573
		inc	d
r573:		djnz	r571
		call	GetFATbuf
		ld	b,3
		call	DiskReadSec
r572:		pop	bc
		pop	de
		pop	hl
		ret

DiskReadSect:	call	H_DREA			; read sector hook
		ld	a,(ix+0)
		ld	c,(ix+1)
		jp	ReadSector

NewUpdateFAT:	push	de
		ld	e,(ix+19)
		ld	d,(ix+20)
		ld	hl,(FATSWAP4)
		or	a
		sbc	hl,de
		pop	de
		jp	nz,r574
SaveFATbuf:	push	bc
		push	de
		push	ix
		ld	a,(FATSWAP2)
		call	GetDPBptr
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		push	bc
		pop	ix
		call	GetFATbuf
		ld	b,a
		dec	hl
		ld	a,(hl)
		ld	(hl),0
		inc	hl
		cp	1
		jp	nz,r575
		ld	a,(FATSWAP3)
		ld	c,a
		add	a,a
		add	a,c
		add	a,e
		ld	e,a
		jr	nc,r576
		inc	d
r576:		push	de
		push	bc
		push	hl
		ld	a,(FATSWAP3)
		ld	b,a
		add	a,a
		add	a,b
		ld	b,a
		ld	a,(ix+16)
		sub	b
		jr	z,r575
		cp	3
		ld	b,a
		jr	c,r577
		ld	b,3
r577:		ld	a,(ix+0)
		call	DiskWriteSec
		pop	hl
		pop	bc
		pop	de
		ld	a,e
		add	a,(ix+16)
		ld	e,a
		jr	nc,r578
		inc	d
r578:		djnz	r576
r575:		pop	ix
		pop	de
		pop	bc
		ret

r574:		call	GetFATbuf
r579:		push	af
		push	ix
		push	hl
		push	de
		push	bc
		ld	a,(ix+0)
		ld	c,(ix+1)
		call	DiskWriteSec
		pop	bc
		pop	de
		ld	a,e
		add	a,b
		ld	e,a
		ld	a,0
		adc	a,d
		ld	d,a
		pop	hl
		pop	ix
		pop	af
		dec	a
		jr	nz,r579
		ret

; ------------------------------------------
NewGetFATbuf:	ld	a,(FATSWAP2)		; Drive of FAT buffer
		cp	$ff			; bugfix, skip if FAT drive not set
		jr	z,r001			; "
		cp	(ix)			; Is current drive?
		call	nz,SaveFATbuf
r001:		ld	a,(THISDR)		; Current driveid
		call	ReadFATbuf
		ld	l,(ix+19)
		ld	h,(ix+20)		; pointer to FAT buffer of drive
		dec	hl
		ld	(hl),0			; FAT buffer clean
		jp	ChangeDPB

; ------------------------------------------------------------------------------
; FAT16 routines
; The FAT16 partition boot sector disk parameters should have the directory
; entries setting to an 8-bit value e.g. $E0 or $70 (maximum is $FF)!
; ------------------------------------------------------------------------------

; ------------------------------------------
F16P01:		ld	a,(ix+15)		; max clusters high byte
		cp	$10			; > 12-bit?
		jr	nc,r1602
		ld	a,h
		cp	$0f			; end cluster chain (FAT12)?
r1601:		ret	c
		ld	a,l
		cp	$f7			; end of chain: nc >= fff7 or >=0ff7
		ret
r1602:		ld	a,h
		cp	$ff			; end cluster chain (FAT16)?
		jr	r1601

; ------------------------------------------
F16P02:		push	de
		ld	e,l
		ld	d,h
		ld	a,(ix+15)		; max clusters high byte
		cp	$10			; > 12-bit?
		jp	c,r1603			; c=no
		add	hl,de
		pop	de
		add	hl,de
		ld	(hl),c
		inc	hl
		ld	(hl),b
		ret

r1603:		ld	a,b
		and	$0f
		ld	b,a
		jp	OldPutFAT

; ------------------------------------------
F16P03:		add	hl,hl
		add	hl,de
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		or	h
		ret

; ------------------------------------------
F16P04:		ld	a,(ix+15)		; max clusters high byte
		cp	$10			; > 12-bit?
		jr	nc,A75B2		; nc=yes
		ld	a,h
		rra
		rra
		and	$03
		ret
A75B2:		ld	a,h
		push	bc
		ld	c,$ff
A75B6:		inc	c
		sub	$03
		jr	nc,A75B6
		ld	a,c
		pop	bc
		ret

; ------------------------------------------
F16P05:		ld	c,$00
		dec	b
		jr	z,r1605
r1604:		add	hl,hl
		rl	c
		djnz	r1604
r1605:		or	l
		ld	l,a
		ld	a,c
		ld	c,(ix+12)
		ld	b,(ix+13)
		add	hl,bc
		adc	a,$00
		pop	bc
		ret	z
		ld	(F16LOSEC),hl		; store 24-bit sector number bit 0..15
		ld	l,a
		ld	h,$00
		ld	(F16HISEC),hl		; store 24-bit sector number bit 16..23
		ld	hl,$ffff		; indicator to use 24-bit sector number in DSKIO routine
		ret

; ------------------------------------------
F16P06:		ld	a,e
		and	d
		inc	a
		jr	z,r1606
		ld	hl,(BUFSEC)		; sectornumber in data buffer
		or	a
		sbc	hl,de
		ret
r1606:		inc	a
		scf
		ret

; ------------------------------------------
F16P07:		jr	nc,r1607
		ld	a,d
		and	e
		inc	a
		ret	nz
r1607:		ld	a,$ff
		ld	(BUFDRN),a		; driveid of sector in directory buffer
		ret

; ------------------------------------------------------------------------------
; *** Paging helper routines (DOS 2 style) ***
; ------------------------------------------------------------------------------

; ---------------------------------------------------------
; Subroutine interrupt handler
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
; Disk ROM compatible subroutines
; ---------------------------------------------------------

; No need to disable/enable a disk rom to transfer data in page 1.
PH_XFER:	ldir
		ret

; Catch command.com calls to disk rom for format and basic command
; other uses of DOSON and DOSOF will just return with no operation.
PH_DOSON:	ex	(sp),hl
		ld	a,(hl)
		cp	$c3			; Test BASIC:	c3 xx 40 (jp $4022)
		jr	z,testbasic
		cp	$cd			; Test FORMAT:	cd xx 40 (call $4025)
		jr	z,testformat
		ex	(sp),hl
		ret

testbasic:
testformat:	inc	hl
		inc	hl
		ld	a,(hl)
		cp	$40
		jr	nz,retdos
		inc	hl			; skip jump/call to command
		ex	(sp),hl
		ret

retdos:		dec	hl			; return no action
		dec	hl
		ex	(sp),hl
PH_DOSOF:	ret

; ---------------------------------------------------------
; Subroutine RDSLT
; ---------------------------------------------------------
PH_RDSLT:	res	6,d
		jr	r021

; ---------------------------------------------------------
; Subroutine WRSLT
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
; Subroutine CALLF
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
; Subroutine CALSLT
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
; Subroutine ENASLT
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
r026:
PatchTPA:	call	nz,X0046		; direct call until TPA page 0 is initialized (SSLOTL)
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


; ---------------------------------------------------------------------------------------
; Include ram resident part of disk interface driver

		DEFINE	DRV_SYS
	IF PPIDE || CFIDE
		INCLUDE	"../driver/driver.asm"
		INCLUDE	"../driver/drv_ide.asm"
	ELIF JIO
		INCLUDE	"../driver/drv_jio.asm"
	ENDIF

; ---------------------------------------------------------------------------------------

	IFDEF DEBUG
			; set kernel code end marker
		db	"~END~"
	ENDIF

		DEPHASE

DosSize		EQU	$-DosBegin
