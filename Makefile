IDRIS2	= idris2
IPKG	= mos6502.ipkg

all:
	$(IDRIS2) --build $(IPKG)

install: all
	$(IDRIS2) --install $(IPKG)