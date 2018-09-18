# Changes

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
