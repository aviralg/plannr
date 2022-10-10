R := R

.PHONY: all build check document test

all: clean document build check install

build: document
	$(R) CMD build .

check: build
	$(R) CMD check plannr*tar.gz

clean:
	-rm -f plannr*tar.gz
	-rm -fr plannr.Rcheck
	-rm -rf src/*.o src/*.so

install: clean
	$(R) CMD INSTALL .

uninstall:
	$(R) --slave -e "remove.packages('plannr')"

document: install-devtools
	$(R) --slave -e "devtools::document()"

website: document
	$(R) --slave -e "pkgdown::build_site()"

test: install-devtools
	$(R) --slave -e "devtools::test()"

lintr: install-lintr
	$(R) --slave -e "quit(status = length(print(lintr::lint_package())) != 0)"

install-devtools:
	$(R) --slave -e "if (!require('devtools')) install.packages('devtools')"

install-lintr:
	$(R) --slave -e "if (!require('lintr')) install.packages('lintr')"
