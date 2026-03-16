# Make z80 binary from asm source with z88dk tools

SOURCES1 := main.asm kernel.asm sys.asm

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

all:	beer1 soda1 malt1 jio1 pio1 uart1

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

# build logic for all interfaces
cxdos1:	rdate
	@echo Assembling CXDOS1 for $(INTERFACE)...
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m $(DEFINES) -Oobj -o=cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/cxdos1 --filler 0xFF --clean
	$(CP) obj$/cxdos1__.bin bin$/CXDOS1$(SUFFIX).COM
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBASBIN $(DEFINES) -Oobj -o=cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/cxdos1 --filler 0xFF --clean
	$(CP) obj$/cxdos1__.bin bin$/CXDOS1$(SUFFIX).BIN
	@echo done

beer1:
	@$(MAKE) cxdos1 INTERFACE="BEER IDE" DEFINES=-DBEER SUFFIX=B

soda1:
	@$(MAKE) cxdos1 INTERFACE="SODA IDE" DEFINES=-DSODA SUFFIX=C

malt1:
	@$(MAKE) cxdos1 INTERFACE="MALT IDE" DEFINES=-DMALT SUFFIX=D

jio1:
	@$(MAKE) cxdos1 INTERFACE="JIO" DEFINES=-DJIO SUFFIX=A

pio1:
	@$(MAKE) cxdos1 INTERFACE="JIO LPT" DEFINES="-DJIO -DLPTIO" SUFFIX=AP

uart1:
	@$(MAKE) cxdos1 INTERFACE="JIO UART" DEFINES="-DJIO -DUART" SUFFIX=AU

clean:
	$(RMF) obj$(RMD)
	$(RMF) rdate.inc
