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

let to_rely =
    (
      ~long=Lazy.force(long_),
      ~rand=default_rand(),
      test: Rely.Test.testFn('a),
      t: T.t,
    ) => {
  let T.Test(cell) = t;
  let name = T.get_name(cell);
  test(name, _ =>
    T.check_cell_exn(cell, ~long, ~rand)
  );
};
