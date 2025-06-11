; ------------------------------------------------------------------------------
; cxdos1.inc
; CXDOS / MSX constants and definitions
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		DEFINE	CXDOS1		; prevent double declarations
		DEFINE	FAT16DOS1	; include FAT16
		DEFINE	IDEDOS1		; include DOS 1 enhancements

; CXDOS definitions
; ------------------------------------------------------------------------------

;DEBUG		equ	$01		; Include debug information / integrity checks

TBASE		equ	$0100		; Start of TPA
BOT32K		equ	$8000		; Bottom of 32 KB RAM
BOT16K		equ	$C000		; Bottom of 16 KB RAM
TMPSTK		equ	BOT16K+256	; Temporary stack

SYSBASE		equ	$C100		; msxdos.sys base address (256-byte page boundary, above TMPSTK)
SYSMAX		equ	$4A0		; Maximum size of msxdos.sys code
DOSBASE		equ	SYSBASE+SYSMAX	; Kernel base address
DOSMAX		equ	$1F60		; Maximum size of kernel code
DOSFREE		equ	DOSBASE+DOSMAX	; Bottom of free variable memory for DOS (256-byte boundary)

; CXDOS
MYWORK		equ	$F1D3		; pointer to driver workarea
SP_IRQ		equ	$F1D5		; temporary save stackpointer (KEYINT)
IRQ_ST		equ	$F1D7		; pointer to temporary stack (KEYINT)
SS_TEMP		equ	$F1DC		; Temporary secondary slot value
F16HISEC	equ	$F1DD		; FAT16 32-bit sector number bit 16..31 (only bit 16..23 used) (old: 0xFD0F)
F16LOSEC	equ	$F1DF		; FAT16 32-bit sector number bit 0..15 (old: 0xFD0D)
FATSWAP1	equ	$F1ED		; FAT swap value (old: 0xFD09)
FATSWAP2	equ	$F1EF		; FAT swap current drive flag (old: 0xFFD9)
FATSWAP3	equ	$F1F0		; FAT swap block number in buffer (old: 0xFFDA)
FATSWAP4	equ	$F1F2		; FAT swap common buffer indicator (old: 0xFFDB)

; MSX-DOS 2
PUT_P1		equ	$F21E		; PUT_P1 handler
GET_P1		equ	$F221		; GET_P1 handler
TIMI_S		equ	$F2C0		; H.TIMI handler
CODE_S		equ	$F2D0		; BDOS code segment
DOSVER		equ	$F313	  	; DOS version

; BASIC
ENDBUF		equ	$F660		; The BASIC screen buffer (258 bytes) is re-used for the DOS interrupt stack

; Potential re-usable BASIC work area:
; F55D to F87E = 820 bytes
; F92A to FAF4 = 459 bytes

; ----------------------------------------------
; CXDOS constants
BELL		equ	$07
BS		equ	$08
TAB		equ	$09
LF		equ	$0A
HOME		equ	$0B
FF		equ	$0C
CR		equ	$0D
INS		equ	$12
CTRL_Z		equ	$1A
ESC		equ	$1B
RIGHT		equ	$1C
LEFT		equ	$1D
SPACE		equ	$20
DEL		equ	$7F

; ------------------------------------------------------------------------------
; BIOS ROM / MSXDOS scratch area

WBOOT		equ     $0000		; CP/M warm boot
KBDOS		equ     $0005		; CP/M BDOS call
RDSLT		equ     $000C		; Reads an address value in another slot
WRSLT		equ	$0014		; Writes an address value in another slot
CALSLT		equ     $001C		; Call a routine in another slot
ENASLT		equ     $0024		; Switch to slot in specified 16K page
IDBYT0		equ     $002B		; ROM Version
CALLF		equ	$0030		; Interslot call (slot:routine)
CHAR_16		equ     $0034		; Default Kanji character range
KEYINT		equ	$0038		; Interrupt routine (MSX uses interrupt mode 1 / no nonmaskable interrupts)
SSLOT		equ     $003B		; Secondary slot helper routines
SSLOTE		equ     $004B		; "
SSLOTL		equ	$0046		; "
CHSNS		equ     $009C		; Check status of the keyboard buffer
CHGET		equ     $009F		; Console input character (wait)
CHPUT		equ     $00A2		; Console output character
LPTOUT		equ     $00A5		; Printer output character
BREAKX		equ     $00B7		; Check status of CTRL+STOP (MSX break program keys)
ERAFNK		equ	$00CC		; Hide bottom function key line
SNSMAT	 	equ     $0141		; Get specified line of the keyboard matrix (machine/region dependent)

; ------------------------------------------------------------------------------
; DOS static data area (F1C9-F37F)

DATABA		equ	$F1C9		; Start of disk system static work area
SPRTBUF		equ	$F1C9		; BDOS function $09 STROUT
BLKMOV		equ	$F1D9		; move block (obsolete)
JPHL		equ	$F1E8		; start handler in DOS memory
LODNAM		equ	$F1F4		; validate FCB filename
ENDJMP		equ	$F1E2		; warm boot (obsolete)
IONAME		equ	$F1F7		; device name table
DEVDIR		equ	$F20B		; directory entry structure for devices
MONTAB		equ	$F22B		; month table
CARPOS		equ	$F237		; BDOS console output columnpos
STARTP		equ	$F238		; console columnpos at start of lineinput
INSERT		equ	$F239		; lineinput insert flag
PFLAG 		equ	$F23B		; console output to printer flag
DIRTYD		equ	$F23C		; directory buffer dirty flag
DMAADD		equ	$F23D		; transfer address
BUFSEC		equ	$F23F		; sectornumber in data buffer
BUFDRN		equ	$F241		; driveid of sector in data buffer
DIRTYB		equ	$F242		; data buffer dirty flag
BUFDRI		equ	$F243		; DPB pointer current operation
DIRBFI		equ	$F245		; sectornumber (offset) in directory buffer
DIRBFD		equ	$F246		; driveid of sector in directory buffer
CURDRV		equ	$F247		; default driveid
DAY		equ	$F248		; current day (1..31)
MONTH		equ	$F249		; current month (1..12)
YEAR		equ	$F24A		; current year (offset to 1980)
DAYCNT		equ	$F24C		; current days since 1-1-1980
WEEKDA		equ	$F24E		; current day of week (0=sunday)
HOOKBE		equ	$F24F		;
H_UNPA		equ	$F252		; get fat entry content hook
H_DEVN		equ	$F255		; check if devicename hook
H_CONT		equ	$F258		; try next direntry hook
H_GETE		equ	$F25B		; get next direntry hook
H_NEXT		equ	$F25E		; next direntry hook
H_MOVN		equ	$F261		; validate FCB drive and filename hook
H_DOOP		equ	$F264		; fcb open hook
H_FATR		equ	$F267		; get latest FAT hook
H_GETI		equ	$F26A		; get pointer to DPB of current drive hook
H_FATW		equ	$F26D		; write FAT hook
H_DREA		equ	$F270		; read sector hook
H_FATA		equ	$F273		; diskerror hook
H_DIRW		equ	$F276		; write dirsector hook
H_DWRI		equ	$F279		; write sector hook
H_MUL1		equ	$F27C		; multiply hook
H_DIV3		equ	$F27F		; divide hook
H_FNDC		equ	$F282		; get absolute cluster hook
H_SKPC		equ	$F285		; get next absolute cluster hook
H_BUFR		equ	$F288		; partical sector read hook
H_BUFW		equ	$F28B		; partical sector write hook
H_ENUF		equ	$F28E		; start read recordoperation from disk hook
H_SETF		equ	$F291		; finish read recordoperation from disk hook
H_SETC		equ	$F294		; end read recordoperation from disk hook
H_WRTE		equ	$F297		; record operation error at start hook
H_NORN		equ	$F29A		; start write recordoperation to disk hook
H_WRTL		equ	$F29D		; finish write recordoperation to disk hook
H_OPTI		equ	$F2A0		; calculate sequential sectors hook
H_FIGR		equ	$F2A3		; get sectornumber of cluster hook
H_ALLO		equ	$F2A6		; allocate FAT chain hook
H_RELB		equ	$F2A9		; release FAT chain hook
H_GETC		equ	$F2AC		; lineinput headloop hook
H_OUT 		equ	$F2AF		; console output hook
H_DATE		equ	$F2B2		; get time and date for direntry hook
H_SETY		equ	$F2B5		; setup days in februari hook
LASTEN		equ	$F2B8		; current direntry number
NAME1		equ	$F2B9		; filename1
ATTRIB		equ	$F2C4		; original DR byte FCB
NAME2		equ	$F2C5		; filename2 (rename)
NAME3		equ	$F2D0		; temporary save for NAME1 and ATTRIB (rename)
CREATI		equ	$F2DC		; flag ignore fileattributes
SECCLU		equ	$F2DD		; current relative sector in cluster
DSKERR		equ	$F2DE		; result of record operation
TRANS		equ	$F2DF		; flag increase current relative sector in cluster (0 means not)
PREREA		equ	$F2E0		; flag flake read (0 means real read)
THISDR		equ	$F2E1		; current driveid
NEXTAD		equ	$F2E2		; transfer address for record operation
RECPOS		equ	$F2E4		; start record (32 bit) for recordoperation
RECCNT		equ	$F2E8		; number of records for recordoperation (DOS1)
LASTPO		equ	$F2EA		; current relative cluster of file (DOS1)
CLUSNU		equ	$F2EC		; current cluster of file (DOS1)
SECPOS		equ	$F2EE		; start relative sector for recordoperation
VALSEC		equ	$F2F0		; relative cluster after fileend for write recordoperation
BYTSEC		equ	$F2F2		; start offset in sector for recordoperation
BYTPOS		equ	$F2F4		; start fileposition (32 bit) for recordoperation
BYTCT1		equ	$F2F8		; partical sector transfer at start
BYTCT2		equ	$F2FA		; partical sector transfer at end
SECCNT		equ	$F2FC		; number of complete sectors to transfer
ENTFRE		equ	$F2FE		; first free direntry (0FFH if none found)
READOP		equ	$F2FF		; flag diskoperation (0 if read, 1 if write)
CONTPO		equ	$F300		; pointer to remaining lineinput from CON read record operation
MAXCLS		equ	$F302		; temporary save for maximium cluster
SPSAVE		equ	$F304		; MSX-DOS save stackpointer
CPMCAL		equ	$F306	  	; CP/M function
SRCHFC		equ	$F307		; saved pointer to FCB search first, used for search next
SRCHIX		equ	$F309		; saved pointer to DPB search first, used for search next
SRCHLO		equ	$F30B		; saved current direntry number search first/next (FFh means invalid)
FCBEXT		equ	$F30C		; original EX byte FCB
RAWFLG		equ	$F30D		; read after write (verify) flag
COUNTR		equ	$F30E  		;
KANJTA		equ	$F30F  		; double byte header table
DISKVE		equ	$F323  		; pointer to disk error handler
BREAKV		equ	$F325	  	; pointer to break handler
AUXBOD		equ	$F327	  	;
BDOSBO		equ	$F331		; BDOS hook (MSXHOOK style)
KEYVLD		equ	$F336		; flag saved input available (0 = none available)
KEYCHR		equ	$F337		; save input
TIMFLG		equ	$F338	  	;
CURDAT		equ	$F33B		; days since 1-1-1980, used when no clockchip
TARGET		equ	$F33F	  	;
NOTFIR		equ	$F340		; cold boot flag
RAMAD0		equ	$F341		; slotid DOS ram page 0
RAMAD1		equ	$F342		; slotid DOS ram page 1
RAMAD2		equ	$F343		; slotid DOS ram page 2
RAMAD3		equ	$F344		; slotid DOS ram page 3
FILMAX		equ	$F345		; maximum number of diskbasic FCB's
DOSFLG		equ	$F346		; MSXDOS started flag
SNUMDR		equ	$F347		; number of drives
MASTER		equ	$F348		; slotid disksystem ROM
HIMSAV		equ	$F349	  	;
DOSHIM		equ	$F34B	  	;
SSECBUF		equ	$F34D		;
SBUFFE		equ	$F34F		; pointer to data sector buffer
SDIRBU		equ	$F351		; pointer to directory sector buffer
SDPBLI		equ	$F355	  	;
RSLREG		equ	$F365	  	;
SDOSON		equ	$F368		; enable disk system ROM on page 1
SDOSOF		equ	$F36B		; enable TPA RAM on page 1
XFER		equ	$F36E		; transfer to/from TPA RAM on page 1
SAUXIN		equ	$F371		;
SAUXOU		equ	$F374		; auxiliary output routine
BDOS		equ	$F37D	  	;

; ------------------------------------------------------------------------------
; BIOS/BASIC work area (F380-FFFF)

VARWRK		equ	$F380		; Start of variable workarea for MSX BASIC / BIOS
RDPRIM		equ	$F380
WRPRIM		equ	$F385
CLPRIM		equ	$F38C
CLPRM1		equ	$F398
KBUF  		equ	$F41F
YCONTP  	equ     KBUF+58         ; KBUF is reused for temporary buffer buffered input, size 128
YCONBF  	equ     KBUF+186        ; KBUF is reused for read CON device records, size 127+2
DRVTBL		equ	$FB21		; disk driver table
BOTTOM		equ	$FC48
HIMEM		equ	$FC4A
CSRSW		equ	$FCA9		; Cursor on/off flag
EXPTBL		equ	$FCC1
SLTTBL		equ	$FCC5
H_TIMI  	EQU     $FD9F
AFFFF		equ	$FFFF
