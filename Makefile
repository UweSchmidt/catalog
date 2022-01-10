build:
	$(MAKE) -C src build

install:
	$(MAKE) -C src install

# install symlink to server and client into local bin dir ./bin

catalogVersion :=$(shell grep -e '^version=' bin/versionbump.sh | sed -e 's/^version=//' -e 's/["]//g' )
ghcVersion     :=$(shell ghc --version | sed -e 's/^.*version //')

servant-polysemy :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/servant-polysemy-$(catalogVersion)/*/servant-polysemy/build/servant-polysemy/servant-polysemy)
client-polysemy  :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/client-polysemy-$(catalogVersion)/*/client-polysemy/build/client-polysemy/client-polysemy)

install-local:
	$(MAKE) build
	# echo $(catalogVersion) $(ghcVersion) $(servant-polysemy) $(client-polysemy)
	ln -sf ../$(servant-polysemy) bin/servant-polysemy
	ln -sf ../$(client-polysemy)  bin/client-polysemy
	ls -l bin
version:
	bin/versionbump.sh
	$(MAKE) -C src install

PHONY: build install install-local
