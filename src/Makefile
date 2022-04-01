build:
	cabal build all \
	--disable-documentation  --disable-profiling

install:
	cabal install client-polysemy servant-polysemy \
	--overwrite-policy=always  --disable-documentation

configure:
	cabal configure --disable-documentation  --disable-profiling
	cabal freeze
	cabal outdated --freeze-file

clean:
	cabal clean

PHONY: configure build install install-local clean
