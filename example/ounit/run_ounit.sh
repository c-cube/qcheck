#!/usr/bin/env sh

# custom script to run qcheck-ounit and filter non reproducible parts

OUT=`./QCheck_ounit_test.exe $@`
CODE=$?

# remove non deterministic output
echo "$OUT" | sed 's/in: .*seconds/in: <nondet> seconds/'
exit $CODE
