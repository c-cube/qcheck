# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

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

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

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

.PHONY: man install_file tags
