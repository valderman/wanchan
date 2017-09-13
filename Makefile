STACKOPTS=--split-objs --local-bin-path .

binary: web
	stack install $(STACKOPTS) || /usr/local/bin/stack install $(STACKOPTS)
	strip -s ./nyanbda || strip -s ./nyanbda.exe

all: deb

deb:
	debuild -us -ec -b
	mv ../nyanbda_*_amd64.deb ./
	rm ../nyanbda_*_amd64.build
	rm ../nyanbda_*_amd64.buildinfo
	rm ../nyanbda_*_amd64.changes

help:
	@echo "The following targets are available:"
	@echo "  all:            build all available packages (currently only deb)"
	@echo "  binary:         build the nyanbda binary (this is the default)"
	@echo "  deb:            build a debian package"
	@echo "  deps:           install all necessary dependencies"
	@echo "  global-install: build and install binary into /usr/local/bin"
	@echo "  user-install:   build and install binary into ~/.local/bin"

deps:
	stack setup
	git clone https://github.com/valderman/haste-app.git
	cd haste-app && git checkout 0.1.0.0
	haste-cabal install ./haste-app
	haste-cabal install --only-dependencies

user-install: prepared-binary
	mkdir -p ~/.local/bin
	cp nyanbda ~/.local/bin/

global-install: prepared-binary
	cp nyanbda /usr/local/bin/

web:
	hastec WebMain.hs
