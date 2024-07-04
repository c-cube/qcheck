# Changes

## 0.22

- Remove `QCheck2.TestResult.get_instances` as retaining previous test inputs
  cause memory leaks
- Make `QCheck2.state.res` immutable, silencing a compilation warning

## 0.21.3

- Drop the dependency on `base-bytes` as it is provided in all supported
  versions of the OCaml compiler

## 0.21.2

- Reintroduce the `Shrink.list_spine` fix by catching `Invalid_argument` and
  falling back on an address comparison.
- Fix #273 by lowering `ppx_deriving_qcheck`'s `qcheck` dependency to `qcheck-core`

## 0.21.1

- Roll back the `Shrink.list_spine` fix, as it was utilizing polymorphic
  equality that can raise an exception on function comparison.

## 0.21

- make `Test.check_result`, `Test.check_cell_exn`, and
  `Test.check_exn` honor test polarity by raising
  `Test_unexpected_success` when a negative test (expected to have a
  counter example), unexpectedly succeeds.
- fix issue with `ppx_deriving_qcheck` deriving a generator with unbound
  `gen` for recursive types [#269](https://github.com/c-cube/qcheck/issues/269)
  and a related issue when deriving a generator for a record type
- fix #241 causing `QCheck.Shrink.int*` to emit duplicates, also affecting `QCheck.Shrink.{char,string}`
- fix a cornercase where `Shrink.list_spine` would emit duplicates

## 0.20

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

## 0.19.1

- fix: allow `~count` in `Test.make` to be 0
- fix: allow `~long_factor` in `Test.make` to be 0

## 0.19

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

## 0.18.1

- fix `Gen.{nat,pos}_split{2,}`
- fix stack overflow in #156

## 0.18

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

## 0.17

- new function: `Gen.delay`

- install printer for an internal exception
- fix(runner): use random state independently for each test
- Fixes distribution and `min_int` issues
- doc: point to @jmid 's website

## 0.16

- fix(runner): detect more failures in the runner
- fix: catch exceptions in generators and log them. (#99)
- test: add test for #99
- fix doc

## 0.15

- fix: in main runner, remove reset line in more places if `colors=false`
- fix: invalid arg in `int_range` when a<0
- fix(runner): do not use ansi code for random seed if `colors=false`
- feat: on `>=4.08`, provide let operators

## 0.14

- modify `int_range` to make it accept ranges bigger than `max_int`.
- less newline-verbose stats
- add `int{32,64}` shrinkers to arbitrary gens
- add `int{32,int64}` shrinkers
- move to ounit2 for `QCheck_ounit`

## 0.13

- make counter private
- Add debug shrinking log
- fix: small fix related to stdlib/pervasives
- feat: add flatten combinators in `gen`

## 0.12

- fix singleton list shrinking
- feat: add `Gen.char_range` and `Gen.(<$>)` (credit @spewspews)

## 0.11

- Add `QCheck.Gen.{string_of,string_readable}`
- fix `int_bound` bound inclusiveness problem
- change implementation of `int_bound` to generate values using `Random.State.int` for `bound < 2^30`
- add weighted shuffled lists generator
- add `float_range` to generate a floating-point number in the given range (inclusive)
- add `float_bound_inclusive` and `float_bound_exclusive` to generate floating-point numbers between 0 and a given bound

## 0.10

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

## 0.9

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

## 0.8

- migrate to jbuilder
- fix warnings
- add some useful functions
- update oasis files (close #48)
- update copyright header (closes #47)

## 0.7

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

## 0.6

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

## 0.5.3.1

- fix regression in runner output (print results of `collect`)
- update the `@since` tags

## 0.5.3

- missing char in `Gen.char` (close #23)
- add `test` and `doc` to opam
- add `small_list` generator
- add `~long_factor` to tests and runner, for long tests
- add more examples in readme, better doc for runners
- improved reporting when running qcheck tests
- add `Test.get_count` on test cells

## 0.5.2

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

## 0.5.1

- document exceptions
- add `small_nat`, change `small_int` semantics (close #10)
- add `QCheck.assume_fail`
- add `QCheck.assume`; explain preconditions a bit (close #9)
- Polish documentation
- Added quad support uniformly

## 0.5

- merge back from `qtest`: big changes in API, shrinking, use `'a arbitrary`
  type that combines printer, generator, shrinker, etc. (see git log)
- merlin file
- reorganize sources, `_oasis`, `.merlin`, etc.

## 0.4

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

## 0.3

- get rid of submodule `generator`
- `Arbitrary.fix_fuel`, to generate complex recursive structures
- new combinators (infix map, applicative funs, shuffle)
- remove generator/Generator, and a deprecation warning
- output of printers of lists/arrays now parsable by ocaml toplevel

## 0.2

- integrate Gabriel Scherer's `Generator` into `QCheck`
- add `|||`
- add `Prop.raises`
- print the faulty instance in case of error (if a printer is available)
- some combinators for `QCheck.Arbitrary`
- `QCheck.mk_test` takes more arguments

## 0.1

- oasis based build system
- source files
