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

# copy server and client to local binaries dir ./bin

catalogVersion   :=$(shell grep -e '^version=' bin/versionbump.sh | sed -e 's/^version=//' -e 's/["]//g' )
ghcVersion       :=$(shell ghc --version | sed -e 's/^.*version //')
servant-polysemy :=$(shell cd src; cabal list-bin servant-polysemy)
client-polysemy  :=$(shell cd src; cabal list-bin client-polysemy)
arch             :=$(shell arch)

copy-apps:
	echo $(arch) $(catalogVersion) $(ghcVersion) $(servant-polysemy) $(client-polysemy)
	test -d bin/$(arch) || mkdir bin/$(arch)
	cp -f $(servant-polysemy) bin/$(arch)/servant-polysemy
	cp -f $(client-polysemy)  bin/$(arch)/client-polysemy
	ls -l bin/$(arch)

PHONY: configure build install install-local local bump-version clean start check-external-progs copy-apps
