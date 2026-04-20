# Make z80 binary from asm source with z88dk tools
# CXDOS Compact Extended MSXDOS alternative

SOURCE_CS0 := cs0_sys.asm cs0_con.asm cs0_dsk.asm cs0_fcb.asm cs0_fhs.asm cs0_ram.asm
SOURCE_CS1 := cs1_sys.asm cs1_ipl.asm cs1_pag.asm cs1_xio.asm cs1_tim.asm cs1_env.asm cs1_msg.asm cs1_ram
SOURCE_CSX := csx_bas.asm

MAKEFLAGS += --no-print-directory

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
	CP	:= copy /b
	GETDATE	:= powershell get-date -format "{yyyy-MM-dd}" 
	QUOTE	:= "
	/	:= $(strip \)
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
	CP	:= cp
	GETDATE	:= date +'%Y-%m-%d'
	QUOTE	:= '"'
	/	:= /
endif 

# Note: the rom folder must be an existing folder
ROM 	:= .$/rom

all:	beer2 soda2 malt2 jio2

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc
	
# build logic for all interfaces
cxdos2:	rdate
	@echo Assembling CXDOS2 for $(INTERFACE)...
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCXDOS2 $(DEFINES) -Oobj -o=cxdos2 $(SOURCE_CS0) $(SOURCE_CS1) $(SOURCE_CSX)
	z88dk-appmake +glue -b obj/cxdos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/cxdos2__.bin -o $(ROM)/$(PREFIX)_cxdos2.rom -s 32768 --org 0
	@echo done
	

beer2:	
	@$(MAKE) cxdos2 INTERFACE="BEER IDE" DEFINES=-DBEER PREFIX=beer

soda2:	
	@$(MAKE) cxdos2 INTERFACE="SODA IDE" DEFINES=-DSODA PREFIX=soda
	
malt2:
	@$(MAKE) cxdos2 INTERFACE="MALT IDE" DEFINES=-DMALT PREFIX=malt
	
jio2:	
	@$(MAKE) cxdos2 INTERFACE="JIO" DEFINES=-DJIO PREFIX=jio

clean:
	$(RMF) obj$(RMD)
	$(RMF) rdate.inc

