/**
  {1 Rely backend for QCheck}

    We can also use environment variables for controlling QCheck here

    - [QCHECK_SEED] if an integer, will fix the seed
    - [QCHECK_LONG] is present, will trigger long tests

    @since NEXT_RELEASE
*/;

/** Convert a qcheck test into a rely test
    @param long whether to run long tests, (default: false)
    @param rand the random generator to use (default: [random_state ()])
    @since NEXT_RELEASE
*/

type qCheckRely = {
  make: (~long: bool=?, ~rand: Random.State.t=?, QCheck.Test.t) => unit,
  makeCell: 'a. (~long: bool=?, ~rand: Random.State.t=?, QCheck.Test.cell('a)) => unit,
}

let toRely:
  (
    Rely.Test.testFn('a),
  ) =>
  qCheckRely;
