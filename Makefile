all: web binary

web:
	haste-cabal configure
	haste-cabal build
	mkdir -p _site
	cp dist/build/WebMain/WebMain _site/WebMain.js
	cp assets/* _site/

binary:
	cabal configure
	cabal build
