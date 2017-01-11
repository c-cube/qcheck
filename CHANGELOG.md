# Changes

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
