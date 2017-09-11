all: web binary

web:
	mkdir -p _site
	hastec WebMain.hs
	mv WebMain.js _site/
	cp assets/* _site/

binary:
	cabal build
