build:
	cabal build all \
	--disable-documentation

install:
	cabal install client-polysemy servant-polysemy \
	--overwrite-policy=always  --disable-documentation

clean:
	cabal clean

PHONY: build install install-local clean
