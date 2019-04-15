/**
  {1 Rely backend for QCheck}

    We can also use environment variables for controlling QCheck here

    - [QCHECK_SEED] if an integer, will fix the seed
    - [QCHECK_LONG] is present, will trigger long tests

    @since 0.9
*/;

/** Convert a qcheck test into a rely test
    @param long whether to run long tests, (default: false)
    @param rand the random generator to use (default: [random_state ()])
    @since 0.9
*/
let to_rely:
  (
    ~long: bool=?,
    ~rand: Random.State.t=?,
    Rely.Test.testFn('a),
    QCheck.Test.t
  ) =>
  unit;
