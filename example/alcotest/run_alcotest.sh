#!/usr/bin/env sh

# custom script to run qcheck-alcotest and filter non reproducible parts

OUT=`./QCheck_alcotest_test.exe $@`
CODE=$?

# remove non deterministic output
echo "$OUT" | grep -v 'This run has ID' \
  | grep -v 'Full test results in' \
  | grep -v 'Logs saved to' \
  | grep -v 'Raised at file' \
  | grep -v 'Called from file' \
  | sed 's/! in .*s\./!/'
exit $CODE
