
all: build test

build:
	jbuilder build @install

test:
	jbuilder runtest --no-buffer

clean:
	jbuilder clean

doc:
	jbuilder build @doc

EXAMPLES=$(addprefix example/, QCheck_test.exe QCheck_ounit_test.exe QCheck_runner_test.exe)

examples:
	jbuilder build $(EXAMPLES)

VERSION=$(shell awk '/^version:/ {print $$2}' qcheck.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make all; \
	done

.PHONY: benchs tests examples update_next_tag push_doc push_stable watch
