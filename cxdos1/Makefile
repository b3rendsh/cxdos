# Make z80 binary from asm source with z88dk tools

SOURCES1 := main.asm kernel.asm sys.asm

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

all:	ppide1 cfide1 jio1

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

ppide1:	rdate
	@echo Assembling CXDOS1 for BEER IDE..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DDEBUG -DPPIDE -Oobj -o=beer_cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/beer_cxdos1 --filler 0xFF --clean
	$(CP) obj$/beer_cxdos1__.bin bin$/CXDOS1B.COM
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DDEBUG -DBLOAD -DPPIDE -Oobj -o=beer_cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/beer_cxdos1 --filler 0xFF --clean
	$(CP) obj$/beer_cxdos1__.bin bin$/CXDOS1B.BIN
	@echo done

cfide1:	rdate
	@echo Assembling CXDOS1 for SODA IDE..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCFIDE -Oobj -o=soda_cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/soda_cxdos1 --filler 0xFF --clean
	$(CP) obj$/soda_cxdos1__.bin bin$/CXDOS1C.COM
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBLOAD -DCFIDE -Oobj -o=soda_cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/soda_cxdos1 --filler 0xFF --clean
	$(CP) obj$/soda_cxdos1__.bin bin$/CXDOS1C.BIN
	@echo done

jio1:	rdate
	@echo Assembling CXDOS1 for JIO..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DJIO -Oobj -o=jio_cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/jio_cxdos1 --filler 0xFF --clean
	$(CP) obj$/jio_cxdos1__.bin bin$/CXDOS1A.COM
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBLOAD -DJIO -Oobj -o=jio_cxdos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/jio_cxdos1 --filler 0xFF --clean
	$(CP) obj$/jio_cxdos1__.bin bin$/CXDOS1A.BIN

clean:
	$(RMF) obj$(RMD)
	$(RMF) rdate.inc

