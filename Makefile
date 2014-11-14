# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

VERSION=$(shell grep Version: _oasis | awk '{ print $$2 }')

release:
	./mk-release.sh ${VERSION} || true
	scp qcheck-${VERSION}.tar.gz cedeela.fr:~/simon/root/software/releases/

push_doc: doc
	scp -r qcheck.docdir/* cedeela.fr:~/simon/root/software/qcheck

tags:
	otags qCheck.ml qCheck.mli

man:
	mkdir -p man/man3/
	ocamlfind ocamldoc -I _build/ -man -d man/man3 qCheck.ml qCheck.mli

install_file: doc man
	@rm qcheck.install || true
	@echo 'doc: [' >> qcheck.install
	@for m in $(wildcard qcheck.docdir/*.html) ; do \
		echo "  \"?$${m}\"" >> qcheck.install; \
	done
	@echo ']' >> qcheck.install
	@echo 'man: [' >> qcheck.install
	@for m in $(wildcard man/man3/[A-Z]*.3o) ; do \
		echo "  \"?$${m}\"" >> qcheck.install; \
	done
	@echo ']' >> qcheck.install

VERSION=$(shell awk '/^Version:/ {print $$2}' _oasis)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" *.ml *.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" *.ml *.mli

.PHONY: man install_file tags release update_next_tag
