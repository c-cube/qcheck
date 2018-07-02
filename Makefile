
all: build test

build:
	jbuilder build @install

test:
	jbuilder runtest --no-buffer

clean:
	jbuilder clean

doc:
	jbuilder build @doc

EXAMPLES=$(addprefix example/, ounit/QCheck_test.exe ounit/QCheck_ounit_test.exe QCheck_runner_test.exe)

examples:
	jbuilder build $(EXAMPLES)

VERSION=$(shell awk '/^version:/ {print $$2}' qcheck.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

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
