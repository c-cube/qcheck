
all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer

clean:
	@dune clean

doc:
	@dune build @doc

example-test:
	@dune exec example/ounit/QCheck_test.exe

example-ounit-test:
	@dune exec example/ounit/QCheck_ounit_test.exe

example-runner:
	@dune exec example/QCheck_runner_test.exe

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
	opam publish prepare https://github.com/c-cube/qcheck/archive/$(VERSION).tar.gz
	@echo "review the release, then type 'opam publish submit qcheck.$(VERSION)/'"


watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make all; \
	done

.PHONY: benchs tests examples update_next_tag watch release
