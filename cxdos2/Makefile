# Make z80 binary from asm source with z88dk tools
# CXDOS Compact Extended MSXDOS alternative

SOURCE_CS0 := cs0_sys.asm cs0_con.asm cs0_dsk.asm cs0_fcb.asm cs0_fhs.asm cs0_ram.asm
SOURCE_CS1 := cs1_sys.asm cs1_ipl.asm cs1_pag.asm cs1_xio.asm cs1_tim.asm cs1_env.asm cs1_msg.asm cs1_ram
SOURCE_CSX := csx_bas.asm

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
	CP	:= copy /b
	GETDATE	:= powershell get-date -format "{yyyyMMdd}" 
	QUOTE	:= "
	/	:= $(strip \)
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
	CP	:= cp
	GETDATE	:= date +'%Y%m%d'
	QUOTE	:= '"'
	/	:= /
endif 

# Note: the rom folder must be an existing folder
ROM 	:= .$/rom

all:	ppide2 cfide2 jio2

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

ppide2:	rdate
	@echo Assembling CXDOS2 for BEER IDE..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCXDOS2 -DPPIDE -Oobj -o=beer_cxdos2.bin $(SOURCE_CS0) $(SOURCE_CS1) $(SOURCE_CSX)
	z88dk-appmake +glue -b obj/beer_cxdos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/beer_cxdos2__.bin -o $(ROM)/beer_cxdos2.rom -s 32768 --org 0
	@echo done

cfide2:	rdate
	@echo Assembling CXDOS2 for SODA IDE..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCXDOS2 -DCFIDE -Oobj -o=soda_cxdos2.bin $(SOURCE_CS0) $(SOURCE_CS1) $(SOURCE_CSX)
	z88dk-appmake +glue -b obj/soda_cxdos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/soda_cxdos2__.bin -o $(ROM)/soda_cxdos2.rom -s 32768 --org 0
	@echo done

jio2:	rdate
	@echo Assembling CXDOS2 for JIO..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCXDOS2 -DJIO -Oobj -o=jio_cxdos2.bin $(SOURCE_CS0) $(SOURCE_CS1) $(SOURCE_CSX)
	z88dk-appmake +glue -b obj/jio_cxdos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/jio_cxdos2__.bin -o $(ROM)/jio_cxdos2.rom -s 32768 --org 0
	@echo done

clean:
	$(RMF) obj$(RMD)
	$(RMF) rdate.inc

