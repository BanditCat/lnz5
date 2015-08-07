################################################################################
## Copyright (c) Jonathan(Jon) DuBois 2015. This file is part of LNZ.         ##
################################################################################


.PHONY: all
all: debug



# Toolchain.
CC=gcc
GHC=ghc
HAPPY=happy
GHCFLAGS=-Wall -Werror -XConstrainedClassMethods -XDeriveDataTypeable -XDeriveFoldable -XDeriveFunctor -XDeriveGeneric -XDeriveTraversable -XEmptyDataDecls -XExistentialQuantification -XExplicitNamespaces -XFlexibleContexts -XFlexibleInstances -XForeignFunctionInterface -XFunctionalDependencies -XGeneralizedNewtypeDeriving -XImplicitParams -XKindSignatures -XLiberalTypeSynonyms -XMagicHash -XMultiParamTypeClasses -XParallelListComp -XPatternGuards -XPostfixOperators -XRankNTypes -XRecursiveDo -XScopedTypeVariables -XStandaloneDeriving -XTypeOperators -XTypeSynonymInstances -XUnboxedTuples -XUnicodeSyntax -XUnliftedFFITypes
HAPPYFLAGS=-a -g -c -i


TARGET=lnz.exe
TARGETDEFINE=-DWINDOWS

# Actual build rules.
# These are supposed to be everything that might be edited.
TXTS:=$(TXTS) $(wildcard ./*.txt) ./Makefile ./README.md ./windowsResource.rc
YS:=$(YS) $(wildcard ./*.y)
HS:=$(HS) $(wildcard ./*.hs)
OBJS:=windowsResource.o 
$(OBJS): Makefile

# windres
windowsResource.o: windowsResource.rc bcj.ico
	windres $< -o $@

# Override defaults
lcparser.hs: lcparser.y
	$(HAPPY) $(HAPPYFLAGS) $< -o $@

$(TARGET): $(HS) $(OBJS)
	$(GHC) main.hs $(OBJS) -o $@ 
	$(STRIP)
	$(PACK)

TAGS: $(HS)
	etags --declarations --ignore-indentation $^


.PHONY: release 
release: $(TARGET)
release: GHCFLAGS:=-O $(TARGETDEFINE) $(GHCFLAGS)
release: LDFLAGS:=-O4 -flto $(LDFLAGS)
release: STRIP:=strip -p $(TARGET)
release: PACK:=upx --best $(TARGET)

.PHONY: debug 
debug: $(TARGET)
debug: CCFLAGS:=$(TARGETDEFINE) -DDEBUG $(CCFLAGS)


.PHONY: clean
clean:
	rm -f ./*.o ./$(TARGET) 

.PHONY: backup
backup: clean release
	git add -A
	git commit -a -m "$(shell cat ~/lnz4/workingon.txt)" || true

.PHONY: run
run: all
	./$(TARGET)

.PHONY: unixify
unixify:
	dos2unix -U $(TXTS) $(SRCS)

