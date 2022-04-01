build:
	cabal build all \
	--disable-documentation  --disable-profiling

install:
	cabal install client-polysemy servant-polysemy \
	--overwrite-policy=always  --disable-documentation

clean:
	cabal clean

PHONY: build install install-local clean
