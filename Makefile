local:
	$(MAKE) install-local

build:
	$(MAKE) -C src build

install:
	$(MAKE) -C src install

install-local:
	$(MAKE) build
	$(MAKE) copy-apps

bump-version:
	bin/versionbump.sh
	$(MAKE) local

configure:
	$(MAKE) -C src configure

start:
	bin/start-catalog-server.sh

clean:
	$(MAKE) -C src clean

check-external-progs:
	bin/check-external-progs.sh

# copy server and client to local bin dir ./bin

catalogVersion :=$(shell grep -e '^version=' bin/versionbump.sh | sed -e 's/^version=//' -e 's/["]//g' )
ghcVersion     :=$(shell ghc --version | sed -e 's/^.*version //')

servant-polysemy :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/servant-polysemy-$(catalogVersion)/*/servant-polysemy/build/servant-polysemy/servant-polysemy)
client-polysemy  :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/client-polysemy-$(catalogVersion)/*/client-polysemy/build/client-polysemy/client-polysemy)

copy-apps:
	@# echo $(catalogVersion) $(ghcVersion) $(servant-polysemy) $(client-polysemy)
	cp -f $(servant-polysemy) bin/servant-polysemy
	cp -f $(client-polysemy)  bin/client-polysemy
	ls -l bin

PHONY: configure build install install-local local bump-version clean start check-external-progs copy-apps
