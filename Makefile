all: prepared-binary

install:
	echo "Either specify user-install or global-install."
	echo "`make user-install' will install into ~/.local/bin."
	echo "`make global-install' will install into /usr/local/bin."

deps:
	cabal install --only-dependencies
	haste-cabal install --only-dependencies

user-install: prepared-binary
	mkdir -p ~/.local/bin
	cp nyanbda ~/.local/bin/

global-install: prepared-binary
	cp nyanbda /usr/local/bin/

prepared-binary: binary web
	strip -s dist/build/nyanbda/nyanbda
	embedtool -p1 -r -w dist/build/nyanbda/nyanbda _site/*
	cp dist/build/nyanbda/nyanbda ./

web:
	hastec WebMain.hs
	rm -r _site || true
	mkdir -p _site
	cp assets/* _site/
	mv WebMain.js _site/WebMain.js

binary:
	cabal configure
	cabal build
