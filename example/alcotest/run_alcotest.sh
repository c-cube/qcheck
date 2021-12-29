#!/usr/bin/env sh

# custom script to run qcheck-alcotest and filter non reproducible parts

OUT=`./QCheck_alcotest_test.exe $@`
CODE=$?

# remove non deterministic output
echo "$OUT" | grep -v 'This run has ID' \
  | grep -v 'Full test results in' \
  | grep -v 'Logs saved to' \
  | grep -v 'Raised at ' \
  | grep -v 'Called from ' \
  | sed 's/! in .*s\./!/' \
  | sed 's/`.*.Error`/`Error`/g' \
  | sed 's/[ \t]*$//g' \
  | tr -s "\n"
exit $CODE
