TARGET	= all

build:
	cabal build $(TARGET) \
	--disable-documentation  --disable-profiling

install:
	cabal install client-polysemy servant-polysemy \
	--overwrite-policy=always  --disable-documentation

ghcVersion     :=$(shell ghc --version | sed -e 's/^.*version //')

configure:
	cabal configure --disable-documentation  --disable-profiling
	cabal freeze
	mv cabal.project.freeze cabal.project.freeze.ghc-$(ghcVersion)
	cabal outdated --freeze-file

clean:
	cabal clean

PHONY: configure build install install-local clean
