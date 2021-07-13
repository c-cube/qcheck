#!/usr/bin/env sh

# custom script to run qcheck-alcotest and filter non reproducible parts

OUT=`./QCheck_alcotest_test.exe $@`
CODE=$?

# remove non deterministic output
echo "$OUT" | grep -v 'This run has ID' | sed 's/! in .*s\./!/'
exit $CODE
