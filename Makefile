build:
	$(MAKE) -C src build

install:
	$(MAKE) -C src install

local:
	$(MAKE) install-local

install-local:
	$(MAKE) build
	$(MAKE) copy-apps

bump-version:
	bin/versionbump.sh
	$(MAKE) build

configure:
	$(MAKE) -C src configure

start:
	bin/start-catalog-server.sh

clean:
	$(MAKE) -C src clean

check-external-progs:
	bin/check-external-progs.sh

# copy server and client to local bin dir ./bin

catalogVersion   :=$(shell grep -e '^version=' bin/versionbump.sh | sed -e 's/^version=//' -e 's/["]//g' )
ghcVersion       :=$(shell ghc --version | sed -e 's/^.*version //')

servant-polysemy :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/servant-polysemy-$(catalogVersion)/*/servant-polysemy/build/servant-polysemy/servant-polysemy)
client-polysemy  :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/client-polysemy-$(catalogVersion)/*/client-polysemy/build/client-polysemy/client-polysemy)
catalog-test     :=$(shell echo src/dist-newstyle/build/*/ghc-$(ghcVersion)/catalog-test-$(catalogVersion)/*/catalog-test/build/catalog-test/catalog-test)

copy-apps:
	@# echo $(catalogVersion) $(ghcVersion) $(servant-polysemy) $(client-polysemy)
	cp -f $(servant-polysemy) bin/servant-polysemy
	cp -f $(client-polysemy)  bin/client-polysemy
	cp -f $(catalog-test)     bin/catalog-test
	ls -l bin

PHONY: configure build install install-local local bump-version clean start check-external-progs copy-apps
