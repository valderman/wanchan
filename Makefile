STACKOPTS=--split-objs --local-bin-path ./artefacts

binary: web
	stack install $(STACKOPTS) || /usr/local/bin/stack install $(STACKOPTS)
	strip -s artefacts/wanchan || strip -s artefacts/wanchan.exe

all: deb cabal

deb: binary
	mkdir -p artefacts
	debuild -us -ec -b
	mv ../wanchan_*_amd64.deb artefacts/
	rm ../wanchan_*_amd64.build
	rm ../wanchan_*_amd64.buildinfo
	rm ../wanchan_*_amd64.changes

cabal: web
	mkdir -p artefacts
	cabal sdist
	mv dist/wanchan-*.tar.gz artefacts

help:
	@echo "The following targets are available:"
	@echo "  all:            build all available packages (currently only deb)"
	@echo "  binary:         build the wanchan binary (this is the default)"
	@echo "  deb:            build a debian package"
	@echo "  deps:           install all necessary dependencies"
	@echo "  global-install: build and install binary into /usr/local/bin"
	@echo "  user-install:   build and install binary into ~/.local/bin"

deps:
	stack update
	haste-cabal update
	stack setup
	haste-cabal install haste-app
	haste-cabal install --only-dependencies

user-install: binary
	mkdir -p ~/.local/bin
	cp artefacts/wanchan ~/.local/bin/

global-install: binary
	cp artefacts/wanchan /usr/local/bin/

web:
	hastec WebMain.hs -oassets/WebMain.js
