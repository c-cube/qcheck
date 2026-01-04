# Changes

## NEXT RELEASE (202?-??-??)

- Remove deprecated generator bindings:
  - `QCheck.Gen`:
    - `big_nat` `neg_int` `pint` `small_nat` `small_int` `small_signed_int` `int_pos_corners`, `int_corners`
    - `ui32` `ui64`
    - `pfloat` `nfloat`
    - `opt`
    - `string_readable` `small_string`
    - `small_list` `list_repeat`
    - `array_repeat` `small_array`
    - `oneofl` `oneofa`
    - `frequency` `frequencyl` `frequencya`
    - `shuffle_a` `shuffle_l` `shuffle_w_l`
    - `flatten_l` `flatten_a` `flatten_opt` `flatten_res`
  - `QCheck.arbitrary`:
    - `small_nat` `small_int` `small_signed_int` `pos_int` `neg_int` - `small_int_corners`
    - `pos_float` `neg_float`
    - `printable_char` `numeral_char`
    - `string_gen_of_size` `string_gen` `small_string` `string_of_size`
      `printable_string` `printable_string_of_size` `string_printable_of_size` `small_printable_string` `string_small_printable`
      `numeral_string` `numeral string` `numeral_string_of_size` `string_numeral_of_size`
    - `bytes_gen_of_size` `bytes_of_size`
    - `small_list` `list_of_size`
    - `array_of_size`
    - `choose` `oneofl` `oneofa`
    - `frequency` `frequencyl` `frequencya`
  - `QCheck2.Gen`:
    - `pint` `small_nat` `big_nat` `neg_int` `small_int` `small_signed_int`
      `small_int_corners` `int_pos_corners` `int_corners`
    - `ui32` `ui64`
    - `pfloat` `nfloat`
    - `opt`
    - `small_string`
    - `small_list` `list_repeat`
    - `small_array` `array_repeat`
    - `oneofl` `oneofa`
    - `frequency` `frequencyl` `frequencya`
    - `shuffle_a` `shuffle_l` `shuffle_w_l`
    - `flatten_l` `flatten_a` `flatten_opt` `flatten_res`
- Other removed deprecated `QCheck2` bindings:
  - `Print.comap`
  - `Shrink.int_aggressive`
  - `Observable.map`
  - `TestResult.stats` `TestResult.warnings` `TestResult.collect`


## 0.91 (2025-12-21)

- Add ocamlmig `@@migrate` attributes to deprecated combinator names.
  The following are missing a direct translation:
  - `QCheck.Gen.big_nat` at most 1_000_000 without a direct translation in distribution
  - `QCheck.Gen.neg_int` a non-uniform generator without a direct translation in distribution
  - `QCheck.Gen.int_pos_corners` deprecated without a replacement
  - `QCheck.Gen.int_corners` deprecated without a replacement
  - `QCheck.Gen.shuffle_a` is in-place, whereas the replacement `QCheck.Gen.shuffle_array` isn't
  - `QCheck.neg_int` a non-uniform generator without a direct translation in distribution
  - `QCheck.small_int_corners` without a direct translation as `int_small_corners` is signed
  - `QCheck2.Gen.big_nat` at most 1_000_000 without a direct translation in distribution
  - `QCheck2.Gen.neg_int` a non-uniform generator without a direct translation in distribution
  - `QCheck2.Gen.small_int_corners` without a direct translation as `int_small_corners` is signed
  - `QCheck2.Gen.int_pos_corners` deprecated without a replacement
  - `QCheck2.Gen.int_corners` deprecated without a replacement


## 0.90 (2025-12-19)

- Remove deprecated function generators `QCheck.{fun1_unsafe,fun2_unsafe}`
- Renamed `float` generator combinators:
  - Add `QCheck.Gen.{float_pos, float_neg}` and deprecate `QCheck.Gen.{pfloat, nfloat}`
  - Add `QCheck.{float_pos, float_neg}` and deprecate `QCheck.{pos_float, neg_float}`
  - Add `QCheck2.{float_pos, float_neg}` and deprecate `QCheck2.{pfloat, nfloat}`
  - Add `QCheck.(--.)` as a synonym for `QCheck.float_range` for consistency
  - Add `float_exp` to `QCheck.Gen`, `QCheck`, and `QCheck2.Gen` and make
    the existing `exponential` binding a synonym for it
- Renamed `char` generator combinators:
  - Add `QCheck.char_range` for consistency
  - Add `QCheck.{char_printable,printable}` and deprecate `QCheck.printable_char`
  - Add `QCheck.{char_numeral,numeral}` and deprecate `QCheck.numeral_char`
  - Add `QCheck.Gen.char_printable` and make `QCheck.Gen.printable` an alias for it
  - Add `QCheck.Gen.char_numeral` and make `QCheck.Gen.numeral` an alias for it
  - Add `QCheck2.char_printable` and make `QCheck2.printable` an alias for it
  - Add `QCheck2.char_numeral` and make `QCheck2.numeral` an alias for it
- Deprecate `QCheck.Gen.opt` and `QCheck2.Gen.opt` for consistency
- Renamed `bytes` generator combinators:
  - Add `QCheck.bytes_size` for consistency
  - Deprecate `QCheck.bytes_gen_of_size` and `QCheck.bytes_of_size`
  - Removed optional `gen` parameter from `QCheck.Gen.bytes` for consistency (API breaking):
    To fix it, replace `Gen.bytes ~gen:char_gen` with `Gen.bytes_of char_gen`
  - Added `QCheck.Gen.bytes_size_of`, `QCheck.bytes_size_of`, and
    `QCheck2.Gen.bytes_size_of` for consistency
- Renamed `string` generator combinators:
  - Add `QCheck.string_size` for consistency
  - Removed optional `gen` parameter from `QCheck.Gen.string` for consistency (API breaking):
    To fix it, replace `Gen.string ~gen:char_gen` with `Gen.string_of char_gen`
  - Add `QCheck.Gen.string_size_of`, `QCheck.string_size_of`, and
    `QCheck2.Gen.string_size_of` for consistency
  - Deprecate `QCheck.Gen.small_string`, `QCheck.small_string`, and `QCheck2.small_string`
  - Deprecate `QCheck.string_gen_of_size`
  - Deprecate `QCheck.string_gen`
  - Deprecate `QCheck.string_of_size`
  - Deprecate `QCheck.printable_string`
  - Deprecate `QCheck.printable_string_of_size`
  - Deprecate `QCheck.string_printable_of_size`
  - Deprecate `QCheck.small_printable_string`
  - Deprecate `QCheck.string_small_printable`
  - Deprecate `QCheck.numeral_string`
  - Deprecate `QCheck.string_numeral`
  - Deprecate `QCheck.numeral_string_of_size`
  - Deprecate `QCheck.string_numeral_of_size`
- Renamed `array` generator combinators:
  - Add `QCheck.Gen.array_small` and deprecate `QCheck.Gen.small_array`
  - Add `QCheck2.Gen.array_small` and deprecate `QCheck2.Gen.small_array`
  - Add `QCheck.array_small` for consistency
  - Add `QCheck.array_size` and deprecate `QCheck.array_of_size`
  - Deprecate badly named `QCheck.Gen.array_repeat` and `QCheck2.Gen.array_repeat`
- Renamed `list` generator combinators:
  - Add `QCheck.list_size` and deprecate  `QCheck.list_of_size`
  - Add `QCheck.Gen.list_small` and deprecate `QCheck.Gen.small_list`
  - Add `QCheck.list_small` and deprecate `QCheck.small_list`
  - Add `QCheck2.Gen.list_small` and deprecate `QCheck2.Gen.small_list`
  - Deprecate badly named `QCheck.Gen.list_repeat` and `QCheck2.Gen.list_repeat`
- Renamed `int` generator combinators:
  - Add `QCheck.Gen.int_small` and deprecate `QCheck.Gen.small_signed_int`
  - Add `QCheck.int_small` and deprecate `QCheck.small_signed_int`
  - Add `QCheck2.Gen.int_small` and deprecate `QCheck2.Gen.small_signed_int`
  - Add `QCheck.Gen.int_pos` and deprecate `QCheck.Gen.pint`
  - Add `QCheck2.Gen.int_pos` without optional `origin` and deprecate `QCheck2.Gen.pint`
  - Add uniformly distributed `QCheck.Gen.int_neg` and deprecate non-uniform `QCheck.Gen.neg_int`
  - Add uniformly distributed `QCheck.int_neg` and deprecate non-uniform `QCheck.neg_int`
  - Add uniformly distributed `QCheck2.Gen.int_neg` and deprecate non-uniform `QCheck2.Gen.neg_int`
  - Add `QCheck.Gen.int_small_corners`
  - Add `QCheck.int_small_corners`, expand `QCheck.Gen.int_corners`, and deprecate unsigned `QCheck.small_int_corners`
  - Add `QCheck2.Gen.int_small_corners`, expand `QCheck2.Gen.int_corners`, and deprecate (unsigned) `QCheck2.Gen.small_int_corners`
  - Deprecate unused `QCheck.Gen.big_nat`
  - Deprecate unused `QCheck2.Gen.big_nat`
  - Add missing `QCheck.nat`
  - Add `QCheck.Gen.int_pos_small` and alias `QCheck.Gen.nat_small` and deprecate `QCheck.Gen.small_nat`
  - Add `QCheck.int_pos_small` and alias `QCheck.nat_small` and deprecate `QCheck2.Gen.small_nat`
  - Add `QCheck2.Gen.int_pos_small` and alias `QCheck2.Gen.nat_small` and deprecate `QCheck2.Gen.small_nat`
  - Add `QCheck.Gen.int_pos_mid` alias for `QCheck.Gen.nat`
  - Add `QCheck.int_pos_mid` alias for `QCheck.nat`
  - Add `QCheck2.Gen.int_pos_mid` and alias `QCheck2.Gen.nat_mid`
- Renamed monadic and applicative generator combinators:
  - Add missing `QCheck.Gen.bind` for consistency
  - Add missing `QCheck.Gen.ap` for consistency
- Renamed `flatten` combinators:
  - Add `QCheck.Gen.flatten_list` and deprecate `QCheck.Gen.flatten_l`
  - Add `QCheck.Gen.flatten_array` and deprecate `QCheck.Gen.flatten_a`
  - Add `QCheck.Gen.flatten_option` and deprecate `QCheck.Gen.flatten_opt`
  - Add `QCheck.Gen.flatten_result` and deprecate `QCheck.Gen.flatten_res`
  - Add `QCheck2.Gen.flatten_list` and deprecate `QCheck2.Gen.flatten_l`
  - Add `QCheck2.Gen.flatten_array` and deprecate `QCheck2.Gen.flatten_a`
  - Add `QCheck2.Gen.flatten_option` and deprecate `QCheck2.Gen.flatten_opt`
  - Add `QCheck2.Gen.flatten_result` and deprecate `QCheck2.Gen.flatten_res`
- Renamed `shuffle` combinators:
  - Add `QCheck.Gen.shuffle_array` and deprecate in-place `QCheck.Gen_shuffle_a`
  - Add `QCheck.Gen.shuffle_list` and deprecate `QCheck.Gen_shuffle_l`
  - Add `QCheck.Gen.shuffle_list_weighted` and deprecate `QCheck.Gen_shuffle_w_l`
  - Add `QCheck2.Gen.shuffle_array` and deprecate `QCheck2.Gen_shuffle_a`
  - Add `QCheck2.Gen.shuffle_list` and deprecate `QCheck2.Gen_shuffle_l`
  - Add `QCheck2.Gen.shuffle_list_weighted` and deprecate `QCheck2.Gen_shuffle_w_l`
- Renamed `oneof` combinators:
  - Add `QCheck.Gen.oneof_list` and deprecate `QCheck.Gen.oneofl`
  - Add `QCheck.Gen.oneof_array` and deprecate `QCheck.Gen.oneofa`
  - Add `QCheck.oneof_list` and deprecate `QCheck.oneofl`
  - Add `QCheck.oneof_array` and deprecate `QCheck.oneofa`
  - Un-deprecate `QCheck.oneof` with a better specification and optional parameters
    like `QCheck.oneof_weighted` and deprecate `QCheck.choose` for consistency
  - Add `QCheck2.Gen.oneof_list` and deprecate `QCheck2.Gen.oneofl`
  - Add `QCheck2.Gen.oneof_array` and deprecate `QCheck2.Gen.oneofa`
- Renamed `frequency` combinators:
  - Add `QCheck.Gen.oneof_weighted` and deprecate `QCheck.Gen.frequency`
  - Add `QCheck.Gen.oneof_list_weighted` and deprecate `QCheck.Gen.frequencyl`
  - Add `QCheck.Gen.oneof_array_weighted` and deprecate `QCheck.Gen.frequencya`
  - Add `QCheck.oneof_weighted` and deprecate `QCheck.frequency`
  - Add `QCheck.oneof_list_weighted` and deprecate `QCheck.frequency_list`
  - Add `QCheck.oneof_array_weighted` and deprecate `QCheck.frequency_array`
  - Add `QCheck2.Gen.oneof_weighted` and deprecate `QCheck2.Gen.frequency`
  - Add `QCheck2.Gen.oneof_list_weighted` and deprecate `QCheck2.Gen.frequencyl`
  - Add `QCheck2.Gen.oneof_array_weighted` and deprecate `QCheck2.Gen.frequencya`
- Add missing `QCheck2.Gen.map_keep_input` for consistency
- Add `QCheck.no_shrink` for consistency
- Fix shrinking for `QCheck2.Gen.exponential` which could shrink to `infinity`
- Remove deprecation annotations for `QCheck.Test.{get,set}*` helpers
- Remove deprecation annotation for `QCheck2.Gen.add_shrink_invariant`
- Renamed `corners` bindings
  - Deprecate `QCheck.Gen.int_corners` and `QCheck.Gen.int_pos_corners`
  - Deprecate `QCheck2.Gen.int_corners` and `QCheck2.Gen.int_pos_corners`


## 0.27 (2025-10-31)

- Add `QCheck.Shrink.float` and enable shrinking for `QCheck.float`
- Add `QCheck.Shrink.float_bound` and enable shrinking for
  `QCheck.float_bound_inclusive` and `QCheck.float_bound_exclusive`
- Add `QCheck.Shrink.float_range` and enable shrinking for `QCheck.float_range`
- Enable shrinking for `QCheck.{pos_float,neg_float,exponential}`
- Patch `QCheck.Print.float` and `QCheck2.Print.float` to print negative nans
  consistently as "-nan" also on Windows and macOS, and correct documentation
  for `QCheck.{float,pos_float,neg_float}` in that they may produce `nan`s since
  #350 from 0.26
- Eta-expand a couple of partial application to compile under OxCaml

## 0.26 (2025-07-25)

- Align printed `collect` statistics and also add a percentage
- Fix `QCheck{,2}.Gen.float` generator which would only generate numbers with an
  exponent between 2^{-21} and 2^22
- Elaborate on `QCheck`/`QCheck2` situation in README
- Add a missing `description` field to the *.opam files
- Document `Shrink` invariants in the `QCheck` module
- Fix a qcheck-ounit test suite failure on OCaml 5.4, removing a needless extra newline
- Fix QCheck2 `float_range` operator which would fail on negative bounds
- Fix `QCHECK_MSG_INTERVAL` not being applied to the first in-progress message

## 0.25 (2025-04-05)

- Restore `Test.make`'s `max_fail` parameter which was accidentally broken in 0.18
- Adjust `stats` computation of average and standard deviation to
  limit precision loss, print both using scientific notation, and
  workaround MinGW float printing to also pass expect tests
- Fix dune snippets missing a language specifier in README.adoc
  causing `asciidoc` to error
- Add a note to `QCheck{,2.Gen}.small_int_corners` and `QCheck{,2}.Gen.graft_corners`
  about internal state, and fix a range of documentation reference warnings
- Reorganize and polish the `README`, rewrite it to use `qcheck-core`, and add
  a `QCheck2` integrated shrinking example
- Document `QCHECK_MSG_INTERVAL` introduced in 0.20
- Add `QCheck{,2}.Gen.map{4,5}` combinators
- [ppx_deriving_qcheck] Support `ppxlib.0.36.0` based on the OCaml 5.2 AST

## 0.24 (2025-02-17)

- [qcheck-alcotest] Add an optional `speed_level` parameter to `to_alcotest`
- Adjust the `QCheck2.Gen.list` shrinker to produce minimal counterexamples at size 3 too
- Replace the `QCheck2` OCaml 4 `Random.State.split` hack with a faster one
- Improve the `QCheck2.Gen.list` shrinker heuristic and utilize the improved
  shrinker in other `QCheck2` `{list,array,bytes,string,function}*` shrinkers
- Use `split` and `copy` in `Random.State` underlying `QCheck2` to
  avoid non-deterministic shrinking behaviour
- Add missing documentation strings for `QCheck.{Print,Iter,Shrink,Gen}` and `QCheck2.Gen`.
- Add `result` combinators to `QCheck`, `QCheck.{Gen,Print,Shrink,Observable}`,
  and `QCheck2.{Gen,Print,Observable}`.
- Add missing combinators `QCheck{,2}.Print.int{32,64}`, `QCheck.Gen.int{32,64}`,
  `QCheck{,2}.Observable.int{32,64}`, and deprecate `QCheck.Gen.{ui32,ui64}`
- Document `dune` usage in README

## 0.23 (2024-12-12)

- Quote and escape in `Print.string` and `Print.char` in the `QCheck` module,
  mirroring the `QCheck2.Print` module's behaviour. Also quote and
  escape `Print.bytes` in both `QCheck` and `QCheck2`.
- Clean-up `QCheck` and `QCheck2` documentation pages
- Add `exponential` generator to `QCheck`, `QCheck.Gen`, and `QCheck2.Gen`
- Add `Shrink.bool` and use it in `QCheck.bool`
- Remove unread `fun_gen` field from `QCheck2`'s `fun_repr_tbl` type
  thereby silencing a compiler warning

## 0.22 (2024-07-05)

- Remove `QCheck2.TestResult.get_instances` as retaining previous test inputs
  cause memory leaks
- Make `QCheck2.state.res` immutable, silencing a compilation warning

## 0.21.3 (2023-11-30)

- Drop the dependency on `base-bytes` as it is provided in all supported
  versions of the OCaml compiler

## 0.21.2 (2023-08-25)

- Reintroduce the `Shrink.list_spine` fix by catching `Invalid_argument` and
  falling back on an address comparison.
- Fix #273 by lowering `ppx_deriving_qcheck`'s `qcheck` dependency to `qcheck-core`

## 0.21.1 (2023-05-19)

- Roll back the `Shrink.list_spine` fix, as it was utilizing polymorphic
  equality that can raise an exception on function comparison.

## 0.21 (2023-05-08)

- make `Test.check_result`, `Test.check_cell_exn`, and
  `Test.check_exn` honor test polarity by raising
  `Test_unexpected_success` when a negative test (expected to have a
  counter example), unexpectedly succeeds.
- fix issue with `ppx_deriving_qcheck` deriving a generator with unbound
  `gen` for recursive types [#269](https://github.com/c-cube/qcheck/issues/269)
  and a related issue when deriving a generator for a record type
- fix #241 causing `QCheck.Shrink.int*` to emit duplicates, also affecting `QCheck.Shrink.{char,string}`
- fix a cornercase where `Shrink.list_spine` would emit duplicates

## 0.20 (2022-11-07)

- add several new `bytes` combinators:
  - `{QCheck,QCheck2}.Gen.{bytes_size,bytes,bytes_of,bytes_printable,bytes_small,bytes_small_of}`
  - `QCheck.{Print,Shrink,Observable}.bytes`
  - `QCheck2.{Print,Shrink}.bytes`
  - `QCheck.{bytes_gen_of_size,bytes_of,bytes,bytes_small,bytes_small_of,bytes_of_size,bytes_printable}`
- add new `string` combinators and aliases:
  - `{QCheck,QCheck2}.Gen.{string_small,string_small_of}`
  - `QCheck.{string_small,string_small_of,string_of,string_printable,string_printable_of_size,string_small_printable,string_numeral,string_numeral_of_size}`
- (`QCheck2.small_string` character generator argument is no more optional - reverted again due to backwards incompatibility)
- add an optional argument with conservative default to `Shrink.string`
- fix shrinkers in `QCheck.{printable_string,printable_string_of_size,small_printable_string,numeral_string,numeral_string_of_size}` [#257](https://github.com/c-cube/qcheck/issues/257)
- add `QCheck2.Gen.set_shrink` to modify the generator's shrinker
- add `QCheck2.Gen.no_shrink` to build a generator with no shrinking
- add an environment variable `QCHECK_MSG_INTERVAL` to control `QCheck_base_runner.time_between_msg`
- fix unknown option error message referring to `qtest`

## 0.19.1 (2022-07-13)

- fix: allow `~count` in `Test.make` to be 0
- fix: allow `~long_factor` in `Test.make` to be 0

## 0.19 (2022-07-08)

- use `Float.equal` for comparing `float`s in the `Observable` module underlying function generators.

- add optional `debug_shrink` parameters in alcotest interface and
  expose default `debug_shrinking_choices` in test runners

- add missing `?handler` parameter to `Test.check_cell_exn`

- remove `--no-buffer` option on `dune runtest` to avoid garbling the
  test output

- add an option `retries` parameter to `Test.make` et al. for checking a
  property repeatedly while shrinking.
  This can be useful when testing non-deterministic code.
  [#212](https://github.com/c-cube/qcheck/pull/212)

- add `tup2` to `tup9` for generators

- add `Test.make_neg` for negative property-based tests, that are
  expected not to satisfy the tested property.

- rename `Gen.opt` to `Gen.option` but keep the old binding for compatibility.

- add additional expect and unit tests and refactor expect test suite

- fix function generation affecting reproducability [#236](https://github.com/c-cube/qcheck/issues/236)

- add a shrinker performance benchmark [#177](https://github.com/c-cube/qcheck/pull/177)

- fix distribution of `QCheck2.printable` which would omit certain characters

- shrinker changes
  - recursive list shrinker with better complexity
  - string shrinker reuses improved list shrinker and adds char shrinking
  - function shrinker now shrinks default entry first and benefits from list shrinker improvements
  - replacing the linear-time char shrinker with a faster one reusing the bisecting int shrinker algorithm
  - add `Shrink.char_numeral` and `Shrink.char_printable`
  - add shrinking for `char arbitrary`s `char`, `printable_char`, and `numeral_char`

- documentation updates:
  - clarify upper bound inclusion in `Gen.int_bound` and `Gen.int_range`
  - clarify `printable_char` and `Gen.printable` distributions
  - add missing `string_gen_of_size` and `small_printable_string` documentation
  - document `QCheck_alcotest.to_alcotest`
  - fix documented size distribution for `arbitrary` generators
    `string_gen`, `string`, `printable_string`, `numeral_string`, `list`, and `array`
  - fix exception documentation for `check_result`, `check_cell_exn`, and `check_exn`
  - fix documentation for the distribution of `Gen.printable` and `printable_char`
  - fix documentation for the shrinking behaviour of `QCheck2.printable`

- add environment variable `QCHECK_LONG_FACTOR` similar to `QCHECK_COUNT` [#220](https://github.com/c-cube/qcheck/pull/220)

- make test suite run on 32-bit architectures

## 0.18.1 (2022-02-03)

- fix `Gen.{nat,pos}_split{2,}`
- fix stack overflow in #156

## 0.18 (2021-09-03)

This releases marks the addition of `QCheck2`, a module where generation
and shrinking are better integrated.
See [#109](https://github.com/c-cube/qcheck/pull/109) and [#116](https://github.com/c-cube/qcheck/pull/116).

This API is still experimental. The normal `QCheck` module is still there
and hasn't changed much.

deprecations and breakges:

- make `QCheck.Test_result.t` abstract and add missing getters
- deprecate `QCheck.oneof`
- deprecate `Gen.string_readable` in favor of `Gen.(string_of char)` or the new `Gen.string_printable`
- require at least OCaml 4.08

other changes:

- unsigned int32 and int64
- rename `small_int_corners`
- add `?ratio` to `opt`, to modify random distribution of options

## 0.17 (2021-02-15)

- new function: `Gen.delay`

- install printer for an internal exception
- fix(runner): use random state independently for each test
- Fixes distribution and `min_int` issues
- doc: point to @jmid 's website

## 0.16 (2020-11-04)

- fix(runner): detect more failures in the runner
- fix: catch exceptions in generators and log them. (#99)
- test: add test for #99
- fix doc

## 0.15 (2020-09-06)

- fix: in main runner, remove reset line in more places if `colors=false`
- fix: invalid arg in `int_range` when a<0
- fix(runner): do not use ansi code for random seed if `colors=false`
- feat: on `>=4.08`, provide let operators

## 0.14 (2020-07-30)

- modify `int_range` to make it accept ranges bigger than `max_int`.
- less newline-verbose stats
- add `int{32,64}` shrinkers to arbitrary gens
- add `int{32,int64}` shrinkers
- move to ounit2 for `QCheck_ounit`

## 0.13 (2020-01-24)

- make counter private
- Add debug shrinking log
- fix: small fix related to stdlib/pervasives
- feat: add flatten combinators in `gen`

## 0.12 (2019-11-08)

- fix singleton list shrinking
- feat: add `Gen.char_range` and `Gen.(<$>)` (credit @spewspews)

## 0.11 (2019-10-02)

- Add `QCheck.Gen.{string_of,string_readable}`
- fix `int_bound` bound inclusiveness problem
- change implementation of `int_bound` to generate values using `Random.State.int` for `bound < 2^30`
- add weighted shuffled lists generator
- add `float_range` to generate a floating-point number in the given range (inclusive)
- add `float_bound_inclusive` and `float_bound_exclusive` to generate floating-point numbers between 0 and a given bound

## 0.10 (2019-07-15)

- `Shrink`: decompose Shrink.list into Shrink.list_spine and Shrink.list_elems
- `Gen.fix` has a more general and useful type
- update README to include `Rely` section (qcheck now available for reason-native!)
- Fix stat printing
- speed-up list shrinker
- Better int shrinking
- core: modify proba distributions again, add `big_nat`
- feat: add `small_array`, modify distributions
- print number of warnings in runner's summary
- refactor: modify type of results to make them more accurate
- feat: warn/fail if too many tests passed only b/c precondition failed

## 0.9 (2018-09-18)

- add `qcheck-ounit` sublibrary
- use environment variables to configure `qcheck-alcotest` tests
- alcotest backend for qcheck
- make `qcheck.ounit` tests verbose by default
- make `qcheck` is a compatibility library, depends on `qcheck-core`
- split lib into `qcheck` and `qcheck.ounit`
- add `TestResult.is_success` helper
- give access to list of instances in test results
- allow setting `time_between_msg` in runner

- chore: remove submodule
- chore: add travis support
- doc: explanations about qcheck.ounit runners
- doc: update readme

## 0.8 (2018-02-03)

- migrate to jbuilder
- fix warnings
- add some useful functions
- update oasis files (close #48)
- update copyright header (closes #47)

## 0.7 (2017-08-21)

- switch to BSD license, make it more explicit (close #43)
- improve multi-line message printing in ounit (closes #46)
- fix complexity of `add_stat`
- allow negative entries in statistics (see #40)
- add a way for tests to report messages to the user (see #39)
- add `QCheck.Shrink.int_aggressive` and make default int shrinker faster
- shrinker for `map_keep_input`
- add `QCheck.set_gen`, missing for some reason

- more compact verbose output (see #33)
- better handling of dynamic progress line
- Add colors to checkmarks in verbose mode
- improve statistics display for runner

- recover exception of shrunk input
- print status line before the solving starts

## 0.6 (2017-05-29)

- add `find_example` and `find_example_gen` to synthesize values from
  properties (see #31)
- add `QCheck.gen` for accessing the random generator easily
- colorful runners, with `--no-colors` to disable them
- add more generator (for corner cases)
- better generation of random functions (see #8),
  using `Observable` and an efficient internal representation using
  heterogeneous tuples, printing, and shrinking.  deprecate old hacks.
- add statistics gathering and display (see #30)

- better printing of Tuple
- improve `Shrink.{array,list}` (see #32)
- Change asserts to raise `Invalid_arg` (following the doc), and update doc
- Change `Gen.{int_bount,int_range}` to support up to 2^62

## 0.5.3.1 (2017-01-27)

- fix regression in runner output (print results of `collect`)
- update the `@since` tags

## 0.5.3 (2017-01-25)

- missing char in `Gen.char` (close #23)
- add `test` and `doc` to opam
- add `small_list` generator
- add `~long_factor` to tests and runner, for long tests
- add more examples in readme, better doc for runners
- improved reporting when running qcheck tests
- add `Test.get_count` on test cells

## 0.5.2 (2017-01-11)

- Add cli option for backtraces in `QCheck_runner`
- Add test case for raising exception
- Better handling of backtraces
- All tests now have a name
- Add step function called on each instance in a test
- make `small_int` a deprecated alias to `small_nat`
- add `small_signed_int`
- remove some warnings
- use safe-string, and fix related bug
- Add long tests options to `QCheck_runner`
- Add `length` specification for `to_ounit2_test`
- Added paragraph in README about long tests

## 0.5.1 (2017-01-08)

- document exceptions
- add `small_nat`, change `small_int` semantics (close #10)
- add `QCheck.assume_fail`
- add `QCheck.assume`; explain preconditions a bit (close #9)
- Polish documentation
- Added quad support uniformly

## 0.5 (2016-12-16)

- merge back from `qtest`: big changes in API, shrinking, use `'a arbitrary`
  type that combines printer, generator, shrinker, etc. (see git log)
- merlin file
- reorganize sources, `_oasis`, `.merlin`, etc.

## 0.4 (2015-06-09)

- bugfix in `fix_fuel`

- if verbose enabled, print each test case
- add `QCheck.run_main`
- `QCheck_ounit.~::`
- add `(>:::)`
- add `qcheck_ounit ml{lib,dylib}`
- trivial ounit integration
- make `test_cell.name` optional
- `Arbitrary.fix_fuel(_gen)`: add a recursive case
- `Arbitrary.fix_fuel_gen`, similar to `fix_fuel` but threading a state bottom-up to make choices depend on the path
- `Arbitrary.fail_fix` to fail in a fixpoint
- helper cases for `Arbitrary.fix_fuel`

## 0.3 (2014-11-14)

- get rid of submodule `generator`
- `Arbitrary.fix_fuel`, to generate complex recursive structures
- new combinators (infix map, applicative funs, shuffle)
- remove generator/Generator, and a deprecation warning
- output of printers of lists/arrays now parsable by ocaml toplevel

## 0.2 (2013-11-07)

- integrate Gabriel Scherer's `Generator` into `QCheck`
- add `|||`
- add `Prop.raises`
- print the faulty instance in case of error (if a printer is available)
- some combinators for `QCheck.Arbitrary`
- `QCheck.mk_test` takes more arguments

## 0.1 (2013-10-06)

- oasis based build system
- source files
