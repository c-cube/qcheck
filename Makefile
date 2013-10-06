lib:
	ocaml setup.ml -build

all: lib

push_doc: lib
	scp -r qCheck.docdir/* cedeela.fr:~/simon/root/software/qcheck

clean:
	ocaml setup.ml -clean

install: lib
	ocaml setup.ml -install

uninstall:
	ocaml setup.ml -uninstall

reinstall:
	ocaml setup.ml -reinstall

tags:
	otags *.ml *.mli

.PHONY: all clean tags install uninstall reinstall

