module T = QCheck.Test;
module Rely = Rely;

let seed_ =
  lazy {
    let s =
      try (int_of_string @@ Sys.getenv("QCHECK_SEED")) {
      | _ =>
        Random.self_init();
        Random.int(1000000000);
      };
    /* Printf.printf("qcheck random seed: %d\n%!", s); */
    s;
  };

let default_rand = () =>
  /* random seed, for repeatability of tests */
  Random.State.make([|Lazy.force(seed_)|]);

let long_ =
  lazy (
    switch (Sys.getenv("QCHECK_LONG")) {
    | "1"
    | "true" => true
    | _ => false
    | exception Not_found => false
    }
  );

type qCheckRely = {
  make: (~long: bool=?, ~rand: Random.State.t=?, QCheck.Test.t) => unit,
  makeCell:
    'a.
    (~long: bool=?, ~rand: Random.State.t=?, QCheck.Test.cell('a)) => unit,

};

let toRely = (test: Rely.Test.testFn('a)) => {
  make: (~long=Lazy.force(long_), ~rand=default_rand(), t) => {
    let QCheck.Test.Test(cell) = t;
    let name = QCheck.Test.get_name(cell);
    test(
      name,
      _ => {
        QCheck.Test.check_cell_exn(cell, ~long, ~rand);
        ();
      },
    );
  },
  makeCell: (~long=Lazy.force(long_), ~rand=default_rand(), cell) => {
    let name = QCheck.Test.get_name(cell);
    test(
      name,
      _ => {
        QCheck.Test.check_cell_exn(cell, ~long, ~rand);
        ();
      },
    );
  },
};
