build:
	$(MAKE) -C src build

install:
	$(MAKE) -C src install

install-global:
	$(MAKE) -C src install-global

version:
	bin/versionbump.sh
	$(MAKE) -C src install

PHONY: build install install-global
