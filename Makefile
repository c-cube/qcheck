
all: build test

build:
	@dune build @install

test:
	@dune runtest --force

clean:
	@dune clean

doc:
	@dune build @doc

example-test:
	@dune exec example/ounit/QCheck_test.exe

example-ounit-test:
	@dune exec example/ounit/QCheck_ounit_test.exe

example-runner:
	@dune exec example/QCheck_runner_test.exe -- -v --debug-shrink=log.tmp

example-alcotest:
	@dune exec example/alcotest/QCheck_alcotest_test.exe

VERSION=$(shell awk '/^version:/ {print $$2}' qcheck.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" `find src -name '*.ml' -or -name '*.mli'`
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" `find src -name '*.ml' -or -name '*.mli'`

release: update_next_tag
	@echo "release version $(VERSION)..."
	git tag -f $(VERSION) ; git push origin :$(VERSION) ; git push origin $(VERSION)
	opam publish https://github.com/c-cube/qcheck/archive/$(VERSION).tar.gz
	@echo "review the release, then type 'opam publish submit qcheck.$(VERSION)/'"


watch:
	@dune build @all -w

.PHONY: benchs test examples update_next_tag watch release
