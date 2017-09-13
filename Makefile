all: prepared-binary

install:
	echo "Either specify user-install or global-install."
	echo "`make user-install' will install into ~/.local/bin."
	echo "`make global-install' will install into /usr/local/bin."

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

prepared-binary: binary web
	strip -s ./nyanbda || strip -s ./nyanbda.exe
	embedtool -p1 -r -w ./nyanbda _site/*

web:
	hastec WebMain.hs
	rm -r _site || true
	mkdir -p _site
	cp assets/* _site/
	mv WebMain.js _site/WebMain.js

binary:
	stack install --local-bin-path .
