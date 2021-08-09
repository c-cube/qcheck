#!/usr/bin/env sh

# custom script to run qcheck-ounit and filter non reproducible parts

OUT=`./QCheck_ounit_test.exe $@`
CODE=$?

# remove non deterministic output
echo "$OUT" \
  | grep -v 'File .*, line .*' \
  | grep -v 'Called from ' \
  | grep -v 'Raised at ' \
  | sed 's/in: .*seconds/in: <nondet> seconds/'
exit $CODE
