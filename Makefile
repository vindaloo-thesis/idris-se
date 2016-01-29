
all: lib cg

.PHONY: force

lib: force
	cd lib && idris --build ethereum.ipkg -i lib --interface
	  
cg: force
	cabal install

clean:
	rm lib/*.ibc
	rm lib/Ethereum/*.ibc

