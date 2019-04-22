open TestFramework;

let passing =
  QCheck.Test.make(
    ~count=1000, ~name="list_rev_is_involutive", QCheck.(list(small_int)), l =>
    List.rev(List.rev(l)) == l
  );

let failing =
  QCheck.Test.make(
    ~count=10, ~name="fail_sort_id", QCheck.(list(small_int)), l =>
    l == List.sort(compare, l)
  );

exception Error;

let error =
  QCheck.Test.make(~count=10, ~name="error_raise_exn", QCheck.int, _ =>
    raise(Error)
  );

let simple_qcheck =
  QCheck.Test.make(
    ~name="fail_check_err_message", ~count=100, QCheck.small_int, _ =>
    QCheck.Test.fail_reportf("@[<v>this@ will@ always@ fail@]")
  );

describe("qcheck-rely", ({test, _}) => {
  let qcheckTest = QCheckRely.toRely(test);
  let _ =
       List.map(
         qcheckTest.make,
         [passing, failing, error, simple_qcheck],
       );
     ();
  qcheckTest.make(
    QCheck.Test.make(
      ~count=1000, ~name="list_rev_is_involutive", QCheck.(list(small_int)), l =>
      List.rev(List.rev(l)) == l
    ),
  );
});

TestFramework.cli();
