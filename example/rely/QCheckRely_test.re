open TestFramework;
open QCheckRely;

let {describe, _} = extendDescribe(QCheckRely.Matchers.matchers);

describe("qcheck-rely", ({test, _}) => {
  test("passing test", ({expect}) => {
    let passing =
      QCheck.Test.make(
        ~count=1000,
        ~name="list_rev_is_involutive",
        QCheck.(list(small_int)),
        l =>
        List.rev(List.rev(l)) == l
      );
    expect.ext.qCheckTest(passing);
    ();
  });

  test("failing test", ({expect}) => {
    let failing =
      QCheck.Test.make(
        ~count=10, ~name="fail_sort_id", QCheck.(list(small_int)), l =>
        l == List.sort(compare, l)
      );

    expect.ext.qCheckTest(failing);
    ();
  });

  test("error test", ({expect}) => {
    exception Error;

    let error =
      QCheck.Test.make(~count=10, ~name="error_raise_exn", QCheck.int, _ =>
        raise(Error)
      );

    expect.ext.qCheckTest(error);
    ();
  });

  test("simple failure test", ({expect}) => {
    let simple_qcheck =
      QCheck.Test.make(
        ~name="fail_check_err_message", ~count=100, QCheck.small_int, _ =>
        QCheck.Test.fail_reportf("@[<v>this@ will@ always@ fail@]")
      );

    expect.ext.qCheckTest(simple_qcheck);
    ();
  });

  /** Alternate syntax, worse matcher output */
  let {make, _} = QCheckRely.toRely(test);

  let simpleFailingTest =
    QCheck.Test.make(
      ~name="fail_check_err_message", ~count=100, QCheck.small_int, _ =>
      QCheck.Test.fail_reportf("@[<v>this@ will@ always@ fail@]")
    );
  make(simpleFailingTest);
});

TestFramework.cli();
