open QCheck

module Shrink = struct

  let trace_false shrinker x =
    let res = ref [] in
    shrinker x (fun x -> res := x::!res);
    List.rev !res

  let trace_true shrinker x =
    let rec loop x =
      match Iter.find (fun _ -> true) (shrinker x) with
      | None -> []
      | Some y -> y::loop y in
    loop x

  let alco_check typ func msg_suffix (msg,input,expected) =
    Alcotest.(check (list typ)) (msg ^ " - " ^ msg_suffix) expected (func input)

  let test_bool () =
    List.iter (alco_check Alcotest.bool (trace_false Shrink.bool) "on repeated failure")
      [ ("bool true",  true,  [false]);
        ("bool false", false, []) ];
    List.iter (alco_check Alcotest.bool (trace_true Shrink.bool) "on repeated success")
      [ ("bool true",  true,  [false]);
        ("bool false", false, []) ]

  let test_int () =
    List.iter (alco_check Alcotest.int (trace_false Shrink.int) "on repeated failure")
      [ ("int 100",   100,  [50; 75; 88; 94; 97; 99]);
        ("int 1000",  1000, [500; 750; 875; 938; 969; 985; 993; 997; 999]);
        ("int (-26)", -26,  [-13; -20; -23; -25]) ];
    List.iter (alco_check Alcotest.int (trace_true Shrink.int) "on repeated success")
      [ ("int 100",   100,  [50; 25; 13; 7; 4; 2; 1; 0]);
        ("int 1000",  1000, [500; 250; 125; 63; 32; 16; 8; 4; 2; 1; 0]);
        ("int (-26)", -26,  [-13; -7; -4; -2; -1; 0]) ]

  let test_int32 () =
    List.iter (alco_check Alcotest.int32 (trace_false Shrink.int32) "on repeated failure")
      [ ("int 100",   100l,  [50l; 75l; 88l; 94l; 97l; 99l]);
        ("int 1000",  1000l, [500l; 750l; 875l; 938l; 969l; 985l; 993l; 997l; 999l]);
        ("int (-26)", -26l,  [-13l; -20l; -23l; -25l]) ];
    List.iter (alco_check Alcotest.int32 (trace_true Shrink.int32) "on repeated success")
      [ ("int 100",   100l,  [50l; 25l; 13l; 7l; 4l; 2l; 1l; 0l]);
        ("int 1000",  1000l, [500l; 250l; 125l; 63l; 32l; 16l; 8l; 4l; 2l; 1l; 0l]);
        ("int (-26)", -26l,  [-13l; -7l; -4l; -2l; -1l; 0l]) ]

  let test_int64 () =
    List.iter (alco_check Alcotest.int64 (trace_false Shrink.int64) "on repeated failure")
      [ ("int 100",   100L,  [50L; 75L; 88L; 94L; 97L; 99L]);
        ("int 1000",  1000L, [500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L]);
        ("int (-26)", -26L,  [-13L; -20L; -23L; -25L]) ];
    List.iter (alco_check Alcotest.int64 (trace_true Shrink.int64) "on repeated success")
      [ ("int 100",   100L,  [50L; 25L; 13L; 7L; 4L; 2L; 1L; 0L]);
        ("int 1000",  1000L, [500L; 250L; 125L; 63L; 32L; 16L; 8L; 4L; 2L; 1L; 0L]);
        ("int (-26)", -26L,  [-13L; -7L; -4L; -2L; -1L; 0L]) ]

  let float_equal x y =
    (Float.is_nan x && Float.is_nan y)
    || x = y
    || abs_float (x -. y) < Float.epsilon
    ||
    (let rel_error =
       if abs_float x > abs_float y then abs_float ((x -. y) /. x) else abs_float ((x -. y) /. y) in
     rel_error <= 1e-5) (* allow 0.00001% error *)

  let alcotest_float = Alcotest.testable Fmt.float float_equal

  let test_float () =
    List.iter (alco_check (alcotest_float ) (trace_false Shrink.float) "on repeated failure")
      [ ("float max_float",    max_float,    [1.79769e+154; 1.79769e+231; 1.79769e+270; 1.79769e+289;
                                              1.79769e+299; 1.79769e+304; 1.79769e+306; 1.79769e+307;
                                              9e+307; 1.3e+308; 1.5e+308; 1.6e+308; 8.99e+307; 1.348e+308;
                                              1.573e+308; 1.685e+308; 1.741e+308; 1.769e+308; 1.783e+308;
                                              1.79e+308; 1.794e+308; 1.796e+308; 8.9885e+307; 1.34827e+308;
                                              1.57298e+308; 1.68534e+308; 1.74152e+308; 1.76961e+308;
                                              1.78365e+308; 1.79067e+308; 1.79418e+308; 1.79594e+308;
                                              1.79682e+308; 1.79726e+308; 1.79748e+308]);
        ("float min_float",    min_float,    [2.22507e-154; 2.22507e-231; 2.22507e-270; 2.22507e-289;
                                              2.22507e-299; 2.22507e-304; 2.22507e-306; 2.22507e-307;
                                              1.22507e-308; 1.1e-308; 1.7e-308; 2e-308; 2.1e-308;
                                              1.113e-308; 1.669e-308; 1.947e-308; 2.086e-308; 2.156e-308;
                                              2.191e-308; 2.208e-308; 2.217e-308; 2.221e-308; 2.223e-308;
                                              2.224e-308; 1.11254e-308; 1.66881e-308; 1.94694e-308;
                                              2.08601e-308; 2.15554e-308; 2.19031e-308; 2.20769e-308;
                                              2.21638e-308; 2.22073e-308; 2.2229e-308; 2.22399e-308;
                                              2.22453e-308; 2.2248e-308]);
        ("float 1e100",        1e100,        [1e+50; 1e+75; 1e+88; 1e+94; 1e+97; 1e+99]);
        ("float 3.3333e+33",   3.3333e+33,   [3.3333e+17; 3.3333e+25; 3.3333e+29; 3.3333e+31; 3.3333e+32;
                                              2.3333e+33; 1.7e+33; 2.5e+33; 2.9e+33; 3.1e+33; 3.2e+33;
                                              1.667e+33; 2.5e+33; 2.917e+33; 3.125e+33; 3.229e+33;
                                              3.281e+33; 3.307e+33; 3.32e+33; 3.327e+33; 3.33e+33;
                                              3.332e+33; 3.3329e+33]);
        ("float 1e10",         1e10,         [100000.; 1e+08; 1e+09]);
        ("float 5e6",          5e6,          [5000.; 500000.; 4e+06; 2.5e+06; 3.8e+06; 4.4e+06; 4.7e+06; 4.9e+06]);
        ("float 1e6",          1e6,          [1000.; 100000.]);
        ("float 1e5",          1e5,          [1000.; 10000.]);
        ("float 1e4",          1e4,          [100.; 1000.]);
        ("float 3.3333e+3",    3.3333e+3,    [333.33; 2333.3; 1700.; 2500.; 2900.; 3100.; 3200.; 1667.; 2500.;
                                              2917.; 3125.; 3229.; 3281.; 3307.; 3320.; 3327.; 3330.; 3332.;
                                              3332.9]);
        ("float 100.",         100.,         [10.]);
        ("float pi",           Float.pi,     [3.; 2.14159; 1.6; 2.4; 2.8; 3.; 1.571; 2.356; 2.749; 2.945;
                                              3.043; 3.092; 3.117; 3.129; 3.135; 3.138; 3.14; 1.5708;
                                              2.3562; 2.7489; 2.94525; 3.04342; 3.09251; 3.11705; 3.12932;
                                              3.13546; 3.13853; 3.14006; 3.14083; 3.14121]);
        ("float 1.",           1.,           []);
        ("float 0.1",          0.1,          [1.]);
        ("float 3.3333e-3",    3.3333e-3,    [0.033333; 0.0023333; 0.0017; 0.0025; 0.0029; 0.0031; 0.0032;
                                              0.001667; 0.0025; 0.002917; 0.003125; 0.003229; 0.003281;
                                              0.003307; 0.00332; 0.003327; 0.00333; 0.003332; 0.0033329]);
        ("float 1e-4",         1e-4,         [0.01; 0.001]);
        ("float 1e-5",         1e-5,         [0.001; 0.0001]);
        ("float 1e-6",         1e-6,         [0.001; 1e-05]);
        ("float 1e-10",        1e-10,        [1e-05; 1e-08; 1e-09]);
        ("float 3.3333e-33",   3.3333e-33,   [3.3333e-17; 3.3333e-25; 3.3333e-29; 3.3333e-31; 3.3333e-32;
                                              2.3333e-33; 1.7e-33; 2.5e-33; 2.9e-33; 3.1e-33; 3.2e-33;
                                              1.667e-33; 2.5e-33; 2.917e-33; 3.125e-33; 3.229e-33;
                                              3.281e-33; 3.307e-33; 3.32e-33; 3.327e-33; 3.33e-33;
                                              3.332e-33; 3.3329e-33]);
        ("float 0.",           0.,           []);
        ("float -0.",          -0.,          []);
        ("float -3.3333e-33",  -3.3333e-33,  [3.3333e-33; -3.3333e-17; -3.3333e-25; -3.3333e-29;
                                              -3.3333e-31; -3.3333e-32; -2.3333e-33; -1.7e-33; -2.5e-33;
                                              -2.9e-33; -3.1e-33; -3.2e-33; -1.667e-33; -2.5e-33;
                                              -2.917e-33; -3.125e-33; -3.229e-33; -3.281e-33; -3.307e-33;
                                              -3.32e-33; -3.327e-33; -3.33e-33; -3.332e-33; -3.3329e-33]);
        ("float -1e-10",       -1e-10,       [1e-10; -1e-05; -1e-08; -1e-09]);
        ("float -1e-6",        -1e-6,        [1e-06; -0.001; -1e-05]);
        ("float -1e-5",        -1e-5,        [1e-05; -0.001; -0.0001]);
        ("float -1e-4",        -1e-4,        [0.0001; -0.01; -0.001]);
        ("float -3.3333e+3",   -3.3333e+3,   [3333.3; -333.33; -2333.3; -1700.; -2500.; -2900.; -3100.;
                                              -3200.; -1667.; -2500.; -2917.; -3125.; -3229.; -3281.; -3307.;
                                              -3320.; -3327.; -3330.; -3332.; -3332.9]);
        ("float -1e10",        -1e10,        [1e+10; -100000.; -1e+08; -1e+09]);
        ("float -3.3333e+33",  -3.3333e+33,  [3.3333e+33; -3.3333e+17; -3.3333e+25; -3.3333e+29;
                                              -3.3333e+31; -3.3333e+32; -2.3333e+33; -1.7e+33; -2.5e+33;
                                              -2.9e+33; -3.1e+33; -3.2e+33; -1.667e+33; -2.5e+33;
                                              -2.917e+33; -3.125e+33; -3.229e+33; -3.281e+33; -3.307e+33;
                                              -3.32e+33; -3.327e+33; -3.33e+33; -3.332e+33; -3.3329e+33]);
        ("float infinity",     infinity,     []);
        ("float neg_infinity", neg_infinity, [infinity]);
        ("float nan",          nan,          []);
      ];
    List.iter (alco_check alcotest_float (trace_true Shrink.float) "on repeated success")
      [ ("float max_float",    max_float,    [1.79769e+154; 1.79769e+77; 1.79769e+39; 1.79769e+20;
                                              1.79769e+10; 179769.; 1797.69; 1797.; 179.7; 179.;
                                              17.9; 17.; 1.7; 1.]);
        ("float min_float",    min_float,    [2.22507e-154; 2.22507e-77; 2.22507e-39; 2.22507e-20;
                                              2.22507e-10; 2.22507e-05; 0.00222507; 0.0222507; 0.222507;
                                              2.22507; 2.; 1.]);
        ("float 1e100",        1e100,        [1e+50; 1e+25; 1e+13; 1e+07; 10000.; 100.; 10.; 1.]);
        ("float 3.3333e+33",   3.3333e+33,   [3.3333e+17; 3.3333e+09; 333330.; 3333.3; 333.33; 333.;
                                              33.3; 33.; 3.3; 3.; 2.; 1.]);
        ("float 1e10",         1e10,         [100000.; 1000.; 100.; 10.; 1.]);
        ("float 5e6",          5e6,          [5000.; 500.; 50.; 5.; 4.; 3.; 2.; 1.]);
        ("float 1e6",          1e6,          [1000.; 100.; 10.; 1.]);
        ("float 1e5",          1e5,          [1000.; 100.; 10.; 1.]);
        ("float 1e4",          1e4,          [100.; 10.; 1.]);
        ("float 3.3333e+3",    3.3333e+3,    [333.33; 333.; 33.3; 33.; 3.3; 3.; 2.; 1.]);
        ("float 100",          100.,         [10.; 1.]);
        ("float pi",           Float.pi,     [3.; 2.; 1.]);
        ("float 1.",           1.,           []);
        ("float 0.1",          0.1,          [1.]);
        ("float 3.3333e-3",    3.3333e-3,    [0.033333; 0.33333; 3.3333; 3.; 2.; 1.]);
        ("float 1e-4",         1e-4,         [0.01; 0.1; 1.]);
        ("float 1e-5",         1e-5,         [0.001; 0.01; 0.1; 1.]);
        ("float 1e-6",         1e-6,         [0.001; 0.01; 0.1; 1.]);
        ("float 1e-10",        1e-10,        [1e-05; 0.001; 0.01; 0.1; 1.]);
        ("float 3.3333e-33",   3.3333e-33,   [3.3333e-17; 3.3333e-09; 3.3333e-05; 0.0033333; 0.033333;
                                              0.33333; 3.3333; 3.; 2.; 1.]);
        ("float 0.",           0.,           []);
        ("float -0.",          -0.,          []);
        ("float -3.3333e-33",  -3.3333e-33,  [3.3333e-33; 3.3333e-17; 3.3333e-09; 3.3333e-05; 0.0033333;
                                              0.033333; 0.33333; 3.3333; 3.; 2.; 1.]);
        ("float -1e-10",       -1e-10,       [1e-10; 1e-05; 0.001; 0.01; 0.1; 1.]);
        ("float -1e-6",        -1e-6,        [1e-06; 0.001; 0.01; 0.1; 1.]);
        ("float -1e-5",        -1e-5,        [1e-05; 0.001; 0.01; 0.1; 1.]);
        ("float -1e-4",        -1e-4,        [0.0001; 0.01; 0.1; 1.]);
        ("float -3.3333e+3",   -3.3333e+3,   [3333.3; 333.33; 333.; 33.3; 33.; 3.3; 3.; 2.; 1.]);
        ("float -1e10",        -1e10,        [1e+10; 100000.; 1000.; 100.; 10.; 1.]);
        ("float -3.3333e+33",  -3.3333e+33,  [3.3333e+33; 3.3333e+17; 3.3333e+09; 333330.; 3333.3; 333.33;
                                              333.; 33.3; 33.; 3.3; 3.; 2.; 1.]);
        ("float infinity",     infinity,     []);
        ("float neg_infinity", neg_infinity, [infinity]);
        ("float nan",          nan,          []);
      ]

  let test_float_bound () =
    List.iter (fun (name,bound,arg,res) -> alco_check alcotest_float (trace_false (Shrink.float_bound bound)) "on repeated failure" (name,arg,res))
      [ ("float_bound max_float max_float",     max_float,   max_float,   [1.79769e+154; 1.79769e+231; 1.79769e+270; 1.79769e+289;
                                                                           1.79769e+299; 1.79769e+304; 1.79769e+306; 1.79769e+307;
                                                                           9e+307; 1.3e+308; 1.5e+308; 1.6e+308; 8.99e+307; 1.348e+308;
                                                                           1.573e+308; 1.685e+308; 1.741e+308; 1.769e+308; 1.783e+308;
                                                                           1.79e+308; 1.794e+308; 1.796e+308; 8.9885e+307; 1.34827e+308;
                                                                           1.57298e+308; 1.68534e+308; 1.74152e+308; 1.76961e+308;
                                                                           1.78365e+308; 1.79067e+308; 1.79418e+308; 1.79594e+308;
                                                                           1.79682e+308; 1.79726e+308; 1.79748e+308]);
        ("float_bound max_float pi",            max_float,   Float.pi,    [3.; 2.14159; 1.1; 2.1; 2.6; 2.9; 3.; 1.071; 2.106; 2.624;
                                                                           2.883; 3.012; 3.077; 3.109; 3.125; 3.133; 3.137; 3.139;
                                                                           3.14; 1.0708; 2.1062; 2.6239; 2.88275; 3.01217; 3.07688;
                                                                           3.10924; 3.12542; 3.13351; 3.13755; 3.13957; 3.14058;
                                                                           3.14109]);
        ("float_bound max_float 1.",            max_float,   1.,          [0.]);
        ("float_bound max_float 0.",            max_float,   0.,          []);
        ("float_bound 1e6 1e6",                 1e6,         1e6,         [999.001; 99999.1; 499999.; 799999.; 899999.]);
        ("float_bound 1e6 10.00001",            1e6,         10.00001,    [0.100001; 5.; 8.; 9.]);
        ("float_bound 1e6 9.00001",             1e6,         9.00001,     [1e-06; 4.; 7.; 8.]);
        ("float_bound 1e6 3.00003",             1e6,         3.00003,     [2.00003; 1.; 2.; 2.5; 2.8; 2.9; 1.00002; 2.00003; 2.50003;
                                                                           2.75003; 2.87503; 2.93753; 2.96878; 2.98441; 2.99222;
                                                                           2.99613; 2.99808; 2.99906; 2.99955]);
        ("float_bound 1e6 1.00001",             1e6,         1.00001,     [1e-05; 0.; 0.5; 0.8; 0.9; 1e-05; 0.50001; 0.75001; 0.87501;
                                                                           0.93751; 0.96876; 0.98439; 0.9922; 0.99611; 0.99806; 0.99904;
                                                                           0.99953; 0.99977]);
        ("float_bound 10. 10.",                 10.,         10.,         [0.1; 5.; 8.; 9.]);
        ("float_bound 1. 1.",                   1.,          1.,          [0.]);
        ("float_bound 1. 0.5",                  1.,          0.5,         [0.; 0.2; 0.4]);
        ("float_bound 1. 1e-3",                 1.,          1e-3,        [0.]);
        ("float_bound 1. 1e-6",                 1.,          1e-6,        []);
        ("float_bound 1. min_float",            1.,          min_float,   []);
        ("float_bound 0. 0.",                   0.,          0.,          []);
        ("float_bound 1. -.min_float",          1.,          -.min_float, []);
        ("float_bound -1. -1e-6",               -1.,         -1e-6,       []);
        ("float_bound -1. -1e-3",               -1.,         -1e-3,       [0.]);
        ("float_bound -1. -0.5",                -1.,         -0.5,        [0.; -0.2; -0.4]);
        ("float_bound -1. -1.",                 -1.,         -1.,         [0.]);
        ("float_bound -10. -10.",               -10.,        -10.,        [-0.1; -5.; -8.; -9.]);
        ("float_bound -1e6 -1.00001",           -1e6,        -1.00001,    [-1e-05; 0.; -0.5; -0.8; -0.9; -1e-05; -0.50001; -0.75001;
                                                                           -0.87501; -0.93751; -0.96876; -0.98439; -0.9922; -0.99611;
                                                                           -0.99806; -0.99904; -0.99953; -0.99977]);
        ("float_bound -1e6 -3.00003",           -1e6,        -3.00003,    [-2.00003; -1.; -2.; -2.5; -2.8; -2.9; -1.00002; -2.00003;
                                                                           -2.50003; -2.75003; -2.87503; -2.93753; -2.96878; -2.98441;
                                                                           -2.99222; -2.99613; -2.99808; -2.99906; -2.99955]);
        ("float_bound -1e6 -9.00001",           -1e6,        -9.00001,    [-1e-06; -4.; -7.; -8.]);
        ("float_bound -1e6 -10.00001",          -1e6,        -10.00001,   [-0.100001; -5.; -8.; -9.]);
        ("float_bound -1e6 -1e6",               -1e6,        -1e6,        [-999.001; -99999.1; -499999.; -799999.; -899999.]);
        ("float_bound -.max_float -1.",         -.max_float, -1.,         [0.]);
        ("float_bound -.max_float -.pi",        -.max_float, -.Float.pi,  [-3.; -2.14159; -1.1; -2.1; -2.6; -2.9; -3.; -1.071; -2.106;
                                                                           -2.624; -2.883; -3.012; -3.077; -3.109; -3.125; -3.133;
                                                                           -3.137; -3.139; -3.14; -1.0708; -2.1062; -2.6239; -2.88275;
                                                                           -3.01217; -3.07688; -3.10924; -3.12542; -3.13351; -3.13755;
                                                                           -3.13957; -3.14058; -3.14109]);
        ("float_bound -.max_float -.max_float", -.max_float, -.max_float, [-1.79769e+154; -1.79769e+231; -1.79769e+270; -1.79769e+289;
                                                                           -1.79769e+299; -1.79769e+304; -1.79769e+306; -1.79769e+307;
                                                                           -9e+307; -1.3e+308; -1.5e+308; -1.6e+308; -8.99e+307;
                                                                           -1.348e+308; -1.573e+308; -1.685e+308; -1.741e+308;
                                                                           -1.769e+308; -1.783e+308; -1.79e+308; -1.794e+308;
                                                                           -1.796e+308; -8.9885e+307; -1.34827e+308; -1.57298e+308;
                                                                           -1.68534e+308; -1.74152e+308; -1.76961e+308; -1.78365e+308;
                                                                           -1.79067e+308; -1.79418e+308; -1.79594e+308; -1.79682e+308;
                                                                           -1.79726e+308; -1.79748e+308]);
      ];
    List.iter (fun (name,bound,arg,res) -> alco_check alcotest_float (trace_true (Shrink.float_bound bound)) "on repeated success" (name,arg,res))
      [ ("float_bound max_float max_float",     max_float,   max_float,   [1.79769e+154; 1.79769e+77; 1.79769e+39; 1.79769e+20;
                                                                           1.79769e+10; 179768.; 1796.69; 1796.; 178.7; 178.; 16.9;
                                                                           16.; 0.7; 0.]);
        ("float_bound max_float pi",            max_float,   Float.pi,    [3.; 2.; 1.; 0.]);
        ("float_bound max_float 1.",            max_float,   1. ,         [0.]);
        ("float_bound max_float 0.",            max_float,   0.,          []);
        ("float_bound 1e6 1e6",                 1e6,         1e6,         [999.001; 99.0001; 9.00001; 1e-06]);
        ("float_bound 1e6 10.00001",            1e6,         10.00001,    [0.100001; 0.]);
        ("float_bound 1e6 9.00001",             1e6,         9.00001,     [1e-06]);
        ("float_bound 1e6 3.00003",             1e6,         3.00003,     [2.00003; 1.00003; 3e-05]);
        ("float_bound 1e6 1.00001",             1e6,         1.00001,     [1e-05]);
        ("float_bound 10. 10.",                 10.,         10.,         [0.1; 0.]);
        ("float_bound 1. 1.",                   1.,          1.,          [0.]);
        ("float_bound 1. 0.5",                  1.,          0.5,         [0.]);
        ("float_bound 1. 1e-3",                 1.,          1e-3,        [0.]);
        ("float_bound 1. 1e-6",                 1.,          1e-6,        []);
        ("float_bound 1. min_float",            1.,          min_float,   []);
        ("float_bound 0. 0.",                   0.,          0.,          []);
        ("float_bound 1. -.min_float",          1.,          -.min_float, []);
        ("float_bound -1. -1e-6",               -1.,         -1e-6,       []);
        ("float_bound -1. -1e-3",               -1.,         -1e-3,       [0.]);
        ("float_bound -1. -0.5",                -1.,         -0.5,        [0.]);
        ("float_bound -1. -1.",                 -1.,         -1.,         [0.]);
        ("float_bound -10. -10.",               -10.,        -10.,        [-0.1; 0.]);
        ("float_bound -1e6 -1.00001",           -1e6,        -1.00001,    [-1e-05]);
        ("float_bound -1e6 -3.00003",           -1e6,        -3.00003,    [-2.00003; -1.00003; -3e-05]);
        ("float_bound -1e6 -9.00001",           -1e6,        -9.00001,    [-1e-06]);
        ("float_bound -1e6 -10.00001",          -1e6,        -10.00001,   [-0.100001; 0.]);
        ("float_bound -1e6 -1e6",               -1e6,        -1e6,        [-999.001; -99.0001; -9.00001; -1e-06]);
        ("float_bound -.max_float -1.",         -.max_float, -1.,         [0.]);
        ("float_bound -.max_float -.pi",        -.max_float, -.Float.pi,  [-3.; -2.; -1.; 0.]);
        ("float_bound -.max_float -.max_float", -.max_float, -.max_float, [-1.79769e+154; -1.79769e+77; -1.79769e+39; -1.79769e+20;
                                                                           -1.79769e+10; -179768.; -1796.69; -1796.; -178.7; -178.;
                                                                           -16.9; -16.; -0.7; 0.]);
      ]

  let test_float_range () =
    List.iter
      (fun (name,low,high,arg,res) -> alco_check alcotest_float (trace_false (Shrink.float_range low high)) "on repeated failure" (name,arg,res))
      [ (* positive ranges *)
        ("float_range 0. max_float max_float",     0.,          max_float,   max_float,   [1.79769e+154; 1.79769e+231; 1.79769e+270; 1.79769e+289;
                                                                                           1.79769e+299; 1.79769e+304; 1.79769e+306; 1.79769e+307;
                                                                                           9e+307; 1.3e+308; 1.5e+308; 1.6e+308; 8.99e+307; 1.348e+308;
                                                                                           1.573e+308; 1.685e+308; 1.741e+308; 1.769e+308; 1.783e+308;
                                                                                           1.79e+308; 1.794e+308; 1.796e+308; 8.9885e+307; 1.34827e+308;
                                                                                           1.57298e+308; 1.68534e+308; 1.74152e+308; 1.76961e+308;
                                                                                           1.78365e+308; 1.79067e+308; 1.79418e+308; 1.79594e+308;
                                                                                           1.79682e+308; 1.79726e+308; 1.79748e+308]);
        ("float_range 0. max_float 1e6",           0.,          max_float,   1e6,         [999.001; 99999.1; 499999.; 799999.; 899999.]);
        ("float_range 0. max_float 10.",           0.,          max_float,   10.,         [0.1; 5.; 8.; 9.]);
        ("float_range 0. max_float 9.000001",      0.,          max_float,   9.000001,    [0.]);
        ("float_range 0. max_float 1.",            0.,          max_float,   1.,          [0.]);
        ("float_range 0. max_float min_float",     0.,          max_float,   min_float,   []);
        ("float_range 0. max_float 0.",            0.,          max_float,   0.,          []);
        ("float_range 10. 100. 100.",              10.,         100.,        100.,        [18.1; 90.; 55.; 78.; 89.; 95.; 98.; 99.]);
        ("float_range 10. 100. 50.",               10.,         100.,        50.,         [13.1; 40.; 30.; 40.; 45.; 48.; 49.]);
        ("float_range 10. 100. 11.000001",         10.,         100.,        11.000001,   [10.; 10.; 10.5; 10.8; 10.9]);
        ("float_range 10. 100. 10.",               10.,         100.,        10.,         []);
        (* ranges crossing 0. *)
        ("float_range -10. 10. 10.",               -10.,        10.,         10.,         [0.1; 5.; 8.; 9.]);
        ("float_range -10. 10. 1.",                -10.,        10.,         1.,          [0.]);
        ("float_range -10. 10. 0.",                -10.,        10.,         0.,          []);
        ("float_range -10. 10. -10.",              -10.,        10.,         -10.,        [8.; -9.]);
        ("float_range -10. 10. -1.",               -10.,        10.,         -1.,         []); (*triggers [float 0.] above*)
        (* negative ranges *)
        ("float_range -100. -10. -10.",            -100.,       -10.,        -10.,        []);
        ("float_range -100. -10. -50.",            -100.,       -10.,        -50.,        [-13.1; -40.; -30.; -40.; -45.; -48.; -49.]);
        ("float_range -100. -10. -100.",           -100.,       -10.,        -100.,       [-18.1; -90.; -55.; -78.; -89.; -95.; -98.; -99.]);
        ("float_range -.max_float 0. -1.",         -.max_float, 0.,          -1.,         [0.]);
        ("float_range -.max_float 0. -10.",        -.max_float, 0.,          -10.,        [-0.1; -5.; -8.; -9.]);
        ("float_range -.max_float 0. -1e6",        -.max_float, 0.,          -1e6,        [-999.001; -99999.1; -499999.; -799999.; -899999.]);
        ("float_range -.max_float 0. -.max_float", -.max_float, 0.,          -.max_float, [-1.79769e+154; -1.79769e+231; -1.79769e+270; -1.79769e+289;
                                                                                           -1.79769e+299; -1.79769e+304; -1.79769e+306; -1.79769e+307;
                                                                                           -9e+307; -1.3e+308; -1.5e+308; -1.6e+308; -8.99e+307;
                                                                                           -1.348e+308; -1.573e+308; -1.685e+308; -1.741e+308;
                                                                                           -1.769e+308; -1.783e+308; -1.79e+308; -1.794e+308;
                                                                                           -1.796e+308; -8.9885e+307; -1.34827e+308; -1.57298e+308;
                                                                                           -1.68534e+308; -1.74152e+308; -1.76961e+308; -1.78365e+308;
                                                                                           -1.79067e+308; -1.79418e+308; -1.79594e+308; -1.79682e+308;
                                                                                           -1.79726e+308; -1.79748e+308]);
      ];
    List.iter
      (fun (name,low,high,arg,res) -> alco_check alcotest_float (trace_true (Shrink.float_range low high)) "on repeated success" (name,arg,res))
      [ (* positive ranges *)
        ("float_range 0. max_float max_float",     0.,          max_float,   max_float,   [1.79769e+154; 1.79769e+77; 1.79769e+39; 1.79769e+20;
                                                                                           1.79769e+10; 179768.; 1796.69; 1796.; 178.7; 178.; 16.9;
                                                                                           16.; 0.7; 0.]);
        ("float_range 0. max_float 1e6",           0.,          max_float,   1e6,         [999.001; 99.0001; 9.00001; 1e-06]);
        ("float_range 0. max_float 10.",           0.,          max_float,   10.,         [0.1; 0.]);
        ("float_range 0. max_float 9.000001",      0.,          max_float,   9.000001,    [0.]);
        ("float_range 0. max_float 1.",            0.,          max_float,   1.,          [0.]);
        ("float_range 0. max_float min_float",     0.,          max_float,   min_float,   []);
        ("float_range 0. max_float 0.",            0.,          max_float,   0.,          []);
        ("float_range 10. 100. 100.",              10.,         100.,        100.,        [18.1; 18.; 17.; 16.; 15.; 14.; 13.; 12.; 11.; 10.]);
        ("float_range 10. 100. 50.",               10.,         100.,        50.,         [13.1; 13.; 12.; 11.; 10.]);
        ("float_range 10. 100. 11.000001",         10.,         100.,        11.000001,   [10.]);
        ("float_range 10. 100. 10.",               10.,         100.,        10.,         []);
        (* ranges crossing 0. *)
        ("float_range -10. 10. 10.",               -10.,        10.,         10.,         [0.1; 0.]);
        ("float_range -10. 10. 1.",                -10.,        10.,         1.,          [0.]);
        ("float_range -10. 10. 0.",                -10.,        10.,         0.,          []);
        ("float_range -10. 10. -10.",              -10.,        10.,         -10.,        [8.; 7.; 6.; 5.; 4.; 3.; 2.; 1.; 0.]);
        ("float_range -10. 10. -1.",               -10.,        10.,         -1.,         []); (*triggers [float 0.] above*)
        (* negative ranges *)
        ("float_range -100. -10. -10.",            -100.,       -10.,        -10.,        []);
        ("float_range -100. -10. -50.",            -100.,       -10.,        -50.,        [-13.1; -13.; -12.; -11.; -10.]);
        ("float_range -100. -10. -100.",           -100.,       -10.,        -100.,       [-18.1; -18.; -17.; -16.; -15.; -14.; -13.; -12.; -11.; -10.]);
        ("float_range -.max_float 0. -1.",         -.max_float, 0.,          -1.,         [0.]);
        ("float_range -.max_float 0. -10.",        -.max_float, 0.,          -10.,        [-0.1; 0.]);
        ("float_range -.max_float 0. -1e6",        -.max_float, 0.,          -1e6,        [-999.001; -99.0001; -9.00001; -1e-06]);
        ("float_range -.max_float 0. -.max_float", -.max_float, 0.,          -.max_float, [-1.79769e+154; -1.79769e+77; -1.79769e+39; -1.79769e+20;
                                                                                           -1.79769e+10; -179768.; -1796.69; -1796.; -178.7; -178.;
                                                                                           -16.9; -16.; -0.7; 0.]);
      ]

  let test_char () =
    List.iter (alco_check Alcotest.char (trace_false Shrink.char) "on repeated failure")
      [ ("char 'a'",   'a',  []);
        ("char 'z'",   'z',  ['n'; 't'; 'w'; 'y']);
        ("char 'A'",   'A',  ['Q'; 'I'; 'E'; 'C'; 'B']);
        ("char '~'",   '~',  ['p'; 'w'; '{'; '}']) ];
    List.iter (alco_check Alcotest.char (trace_true Shrink.char) "on repeated success")
      [ ("char 'a'",   'a',  []);
        ("char 'z'",   'z',  ['n'; 'h'; 'e'; 'c'; 'b'; 'a']);
        ("char 'A'",   'A',  ['Q'; 'Y'; ']'; '_'; '`'; 'a']);
        ("char '~'",   '~',  ['p'; 'i'; 'e'; 'c'; 'b'; 'a']); ]

  let test_char_numeral () =
    List.iter (alco_check Alcotest.char (trace_false Shrink.char_numeral) "on repeated failure")
      [ ("char '0'",   '0',  []);
        ("char '9'",   '9',  ['5'; '7'; '8']) ];
    List.iter (alco_check Alcotest.char (trace_true Shrink.char_numeral) "on repeated success")
      [ ("char '0'",   '0',  []);
        ("char '9'",   '9',  ['5'; '3'; '2'; '1'; '0']); ]

  let test_char_printable () =
    List.iter (alco_check Alcotest.char (trace_false Shrink.char_printable) "on repeated failure")
      [ ("char 'A'",   'A',  ['Q'; 'I'; 'E'; 'C'; 'B']);
        ("char 'a'",   'a',  []);
        ("char ' '",   ' ',  ['@'; '0'; '('; '$'; '"'; '!']);
        ("char '~'",   '~',  ['p'; 'w'; '{'; '}']);
        ("char '\\n'", '\n', ['p'; 'w'; '{'; '}']); ];
    List.iter (alco_check Alcotest.char (trace_true Shrink.char_printable) "on repeated success")
      [ ("char 'A'",   'A',  ['Q'; 'Y'; ']'; '_'; '`'; 'a']);
        ("char 'a'",   'a',  []);
        ("char ' '",   ' ',  ['@'; 'P'; 'X'; '\\'; '^'; '_'; '`'; 'a']);
        ("char '~'",   '~',  ['p'; 'i'; 'e'; 'c'; 'b'; 'a']);
        ("char '\\n'", '\n', ['p'; 'i'; 'e'; 'c'; 'b'; 'a']); ]

  let test_string () =
    List.iter (alco_check Alcotest.string (trace_false Shrink.string) "on repeated failure")
      [ ("string \"\"",     "",     []);
        ("string \"a\"",    "a",    [""]);
        ("string \"aa\"",   "aa",   [""; "a"]);
        ("string \"aaaa\"", "aaaa", ["aa"; "aa"; "aaa"]);
        ("string \"abcd\"", "abcd", ["ab"; "cd"; "acd"; "bcd"; "aacd"; "abbd"; "abcc"]);
        ("string \"E'*\"",  "E'*",  ["E'"; "*"; "E*"; "'*"; "S'*"; "L'*"; "H'*"; "F'*"; "ED*";
                                     "E5*"; "E.*"; "E**"; "E(*"; "E'E"; "E'7"; "E'0"; "E'-"; "E'+"]);
        ("string \"vi5x92xgG\"", "vi5x92xgG", (* A less exhaustive string shrinker would be preferable *)
         ["vi5x9"; "vi52xgG"; "vix92xgG"; "5x92xgG";
          "v5x92xgG"; "i5x92xgG"; "li5x92xgG"; "qi5x92xgG"; "ti5x92xgG"; "ui5x92xgG";
          "ve5x92xgG"; "vg5x92xgG"; "vh5x92xgG";
          "viKx92xgG"; "vi@x92xgG"; "vi:x92xgG"; "vi7x92xgG"; "vi6x92xgG";
          "vi5m92xgG"; "vi5s92xgG"; "vi5v92xgG"; "vi5w92xgG";
          "vi5xM2xgG"; "vi5xC2xgG"; "vi5x>2xgG"; "vi5x;2xgG"; "vi5x:2xgG";
          "vi5x9IxgG"; "vi5x9=xgG"; "vi5x97xgG"; "vi5x94xgG"; "vi5x93xgG";
          "vi5x92mgG"; "vi5x92sgG"; "vi5x92vgG"; "vi5x92wgG";
          "vi5x92xdG"; "vi5x92xfG";
          "vi5x92xgT"; "vi5x92xgM"; "vi5x92xgJ"; "vi5x92xgH"]);
        ("string \"~~~~\"", "~~~~", ["~~"; "~~"; "~~~"; "p~~~"; "w~~~"; "{~~~"; "}~~~"; "~p~~";
                                     "~w~~"; "~{~~"; "~}~~"; "~~p~"; "~~w~"; "~~{~"; "~~}~";
                                     "~~~p"; "~~~w"; "~~~{"; "~~~}"]); ];
    List.iter (alco_check Alcotest.string (trace_true Shrink.string) "on repeated success")
      [ ("string \"\"",     "",     []);
        ("string \"a\"",    "a",    [""]);
        ("string \"aa\"",   "aa",   [""]);
        ("string \"aaaa\"", "aaaa", ["aa"; ""]);
        ("string \"abcd\"", "abcd", ["ab"; ""]);
        ("string \"E'*\"",  "E'*",  ["E'"; ""]);
        ("string \"vi5x92xgG\"", "vi5x92xgG", ["vi5x9"; "vi5"; "vi"; ""]); ]

  let test_int_list () =
    List.iter (alco_check Alcotest.(list int) (trace_false (Shrink.list_spine)) "on repeated failure")
      [ ("list int [0]",       [0],       [[]]);
        ("list int [0;1]",     [0;1],     [[]; [0]; [1]]);
        ("list int [0;1;2]",   [0;1;2],   [[0; 1]; [2]; [0; 2]; [1; 2]]);
        ("list int [0;1;2;3]", [0;1;2;3], [[0; 1]; [2; 3]; [0; 2; 3]; [1; 2; 3]]);
        ("list int [0;0]",     [0;0],     [[]; [0]]);
        ("list int [0;0;0]",   [0;0;0],   [[0; 0]; [0]; [0; 0]]);
        ("list int [0;0;0;0]", [0;0;0;0], [[0; 0]; [0; 0]; [0; 0; 0]]); ];
    List.iter (alco_check Alcotest.(list int) (trace_true (Shrink.list_spine)) "on repeated success")
      [ ("list int [0]",       [0],       [[]]);
        ("list int [0;1]",     [0;1],     [[]]);
        ("list int [0;1;2]",   [0;1;2],   [[0; 1]; []]);
        ("list int [0;1;2;3]", [0;1;2;3], [[0; 1]; []]);
        ("list int [0;0]",     [0;0],     [[]]);
        ("list int [0;0;0]",   [0;0;0],   [[0; 0]; []]);
        ("list int [0;0;0;0]", [0;0;0;0], [[0; 0]; []]); ]

  let test_int32_list () = (* use int32 as a boxed type and List.map to force run-time allocations *)
    List.iter (alco_check Alcotest.(list int32) (trace_false (Shrink.list_spine)) "on repeated failure")
      [ ("list int32 [0l]",          List.map Int32.of_int [0],       [[]]);
        ("list int32 [0l;1l]",       List.map Int32.of_int [0;1],     [[]; [0l]; [1l]]);
        ("list int32 [0l;1l;2l]",    List.map Int32.of_int [0;1;2],   [[0l; 1l]; [2l]; [0l; 2l]; [1l; 2l]]);
        ("list int32 [0l;1l;2l;3l]", List.map Int32.of_int [0;1;2;3], [[0l; 1l]; [2l; 3l]; [0l; 2l; 3l]; [1l; 2l; 3l]]);
        ("list int32 [0l;0l]",       List.map Int32.of_int [0;0],     [[]; [0l]]);
        ("list int32 [0l;0l;0l]",    List.map Int32.of_int [0;0;0],   [[0l; 0l]; [0l]; [0l; 0l]]);
        ("list int32 [0l;0l;0l;0l]", List.map Int32.of_int [0;0;0;0], [[0l; 0l]; [0l; 0l]; [0l; 0l; 0l]]); ];
    List.iter (alco_check Alcotest.(list int32) (trace_true (Shrink.list_spine)) "on repeated success")
      [ ("list int [0l]",          List.map Int32.of_int [0],       [[]]);
        ("list int [0l;1l]",       List.map Int32.of_int [0;1],     [[]]);
        ("list int [0l;1l;2l]",    List.map Int32.of_int [0;1;2],   [[0l; 1l]; []]);
        ("list int [0l;1l;2l;3l]", List.map Int32.of_int [0;1;2;3], [[0l; 1l]; []]);
        ("list int [0l;0l]",       List.map Int32.of_int [0;0],     [[]]);
        ("list int [0l;0l;0l]",    List.map Int32.of_int [0;0;0],   [[0l; 0l]; []]);
        ("list int [0l;0l;0l;0l]", List.map Int32.of_int [0;0;0;0], [[0l; 0l]; []]); ]

  let test_list_spine_compare () =
    let run_test () = QCheck.Shrink.list_spine [pred;succ] ignore in
    Alcotest.(check unit) "doesn't compare elements" () @@ run_test ()

  let test_int_option () =
    List.iter (alco_check Alcotest.(option int) (trace_false Shrink.(option int)) "on repeated failure")
      [ ("option int Some 42",  Some 42,  [None; Some 21; Some 32; Some 37; Some 40; Some 41]);
        ("option int None",     None, []) ];
    List.iter (alco_check Alcotest.(option int) (trace_true Shrink.(option int)) "on repeated success")
      [ ("option int Some 42",  Some 42,  [None]);
        ("option int None",     None, []) ]

  let test_int_string_result () =
    List.iter (alco_check Alcotest.(result int string) (trace_false Shrink.(result int string)) "on repeated failure")
      [ ("result int string Ok 55",          Ok 55,  [Ok 28; Ok 42; Ok 49; Ok 52; Ok 54]);
        ("result int string Error \"oops\"", Error "oops", [Error "oo"; Error "ps"; Error "ops"; Error "hops";
                                                            Error "lops"; Error "nops"; Error "ohps"; Error "olps";
                                                            Error "onps"; Error "oois"; Error "ooms"; Error "ooos";
                                                            Error "oopj"; Error "oopo"; Error "oopq"; Error "oopr"]) ];
    List.iter (alco_check Alcotest.(result int string) (trace_true Shrink.(result int string)) "on repeated success")
      [ ("result int string Ok 55",          Ok 55,  [Ok 28; Ok 14; Ok 7; Ok 4; Ok 2; Ok 1; Ok 0]);
        ("result int string Error \"oops\"", Error "oops", [Error "oo"; Error ""]) ]

  let tests = ("Shrink", Alcotest.[
      test_case "bool"  `Quick test_bool;
      test_case "int"   `Quick test_int;
      test_case "int32" `Quick test_int32;
      test_case "int64" `Quick test_int64;
      test_case "float" `Quick test_float;
      test_case "float_bound" `Quick test_float_bound;
      test_case "float_range" `Quick test_float_range;
      test_case "char"  `Quick test_char;
      test_case "char_numeral"   `Quick test_char_numeral;
      test_case "char_printable" `Quick test_char_printable;
      test_case "string" `Quick test_string;
      test_case "int list" `Quick test_int_list;
      test_case "int32 list" `Quick test_int32_list;
      test_case "list_spine" `Quick test_list_spine_compare;
      test_case "int option"  `Quick test_int_option;
      test_case "(int,string) result" `Quick test_int_string_result;
    ])
end

module Check_exn = struct

  (* String.starts_with was introduced in 4.13.
     Include the below to support pre-4.13 OCaml. *)
  let string_starts_with ~prefix s =
    let prefix_len = String.length prefix in
    prefix_len <= String.length s
    && prefix = String.sub s 0 prefix_len

  let check_exn = Test.check_exn

  let test_pass_trivial () =
    let run_test () = check_exn QCheck.(Test.make int (fun _ -> true)) in
    Alcotest.(check unit) "Success-trivial" () @@ run_test ()

  let test_pass_random () =
    let run_test () =
      check_exn QCheck.(Test.make (list int) (fun l -> List.rev (List.rev l) = l)) in
    Alcotest.(check unit) "Success-random" () @@ run_test ()

  let test_fail_always () =
    let name = "will-always-fail" in
    try
      check_exn QCheck.(Test.make ~name int (fun _ -> false));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_fail (n,[c_ex_str])) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"0" c_ex_str)
        then
        Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  let test_fail_random () =
    let name = "list is own reverse" in
    try
      check_exn QCheck.(Test.make ~name (list int) (fun l -> List.rev l = l));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_fail (n,[c_ex_str])) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"[0; 1]" c_ex_str
                || string_starts_with ~prefix:"[0; -1]" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  exception MyError

  let test_error () =
    let name = "will-always-error" in
    try
      Printexc.record_backtrace false; (* for easier pattern-matching below *)
      check_exn QCheck.(Test.make ~name int (fun _ -> raise MyError));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_error (n,c_ex_str,MyError,"")) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"0" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  let test_negative_trivial () =
    let run_test () = check_exn QCheck2.(Test.make_neg Gen.int (fun _ -> false)) in
    Alcotest.(check unit) "Success-negative-trivial" () @@ run_test ()

  let test_negative_test_unexpected_success () =
    let name = "negative-trivial-test" in
    let run_test () = check_exn QCheck2.(Test.make_neg ~name Gen.int (fun _ -> true)) in
    try
      run_test ();
      Alcotest.failf "Negative test didn't raise expected exception."
    with
      Test.Test_unexpected_success n ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name

  let tests =
    ("Test.check_exn", Alcotest.[
         test_case "check_exn pass trivial" `Quick test_pass_trivial;
         test_case "check_exn pass random" `Quick test_pass_random;
         test_case "check_exn fail always" `Quick test_fail_always;
         test_case "check_exn fail random" `Quick test_fail_random;
         test_case "check_exn Error" `Quick test_error;
         test_case "check_exn negative pass trivial" `Quick test_negative_trivial;
         test_case "check_exn Unexpected success" `Quick test_negative_test_unexpected_success;
       ])
end

module TestCount = struct
  let test_count_n ?count expected =
    let (Test cell) = QCheck.(Test.make ?count int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck.Test.make ~count:%s |> get_count = %d"
                (Option.fold ~none:"None" ~some:string_of_int count) expected
    in
    Alcotest.(check int) msg expected (QCheck.Test.get_count cell)

  let test_count_10 () = test_count_n ~count:10 10

  let test_count_default () = test_count_n 100

  let test_count_env () =
    let () = Unix.putenv "QCHECK_COUNT" "5" in
    let (Test cell) = QCheck.(Test.make int (fun _ -> true)) in
    let actual = QCheck.Test.get_count cell in
    Alcotest.(check int) "default count is from QCHECK_COUNT" 5 actual

  let test_count_0 () = test_count_n ~count:0 0

  let test_count_negative_fail () =
    try
      let _ = test_count_n ~count:(-1) (-1) in
      Alcotest.fail "A negative count in a test should fail"
    with
    | _ -> ()

  let tests =
    ("Test.make ~count", Alcotest.[
         test_case "make with custom count" `Quick test_count_10;
         test_case "make with default count" `Quick test_count_default;
         test_case "make with env count" `Quick test_count_env;
         test_case "make with 0 count" `Quick test_count_0;
         test_case "make with negative count should fail"
           `Quick test_count_negative_fail;
       ])
end

module TestLongFactor = struct
  let test_long_factor_n ?long_factor expected =
    let (Test cell) = QCheck.(Test.make ?long_factor int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck.Test.make ~long_factor:%s |> long_factor = %d"
                (Option.fold ~none:"None" ~some:string_of_int long_factor) expected
    in
    Alcotest.(check int) msg expected (QCheck.Test.get_long_factor cell)

  let test_long_factor_10 () = test_long_factor_n ~long_factor:10 10

  let test_long_factor_default () = test_long_factor_n 1

  let test_long_factor_env () =
    let () = Unix.putenv "QCHECK_LONG_FACTOR" "5" in
    let (Test cell) = QCheck.(Test.make int (fun _ -> true)) in
    let actual = QCheck.Test.get_long_factor cell in
    Alcotest.(check int) "default long factor is from QCHECK_LONG_FACTOR" 5 actual

   let test_long_factor_0 () = test_long_factor_n ~long_factor:0 0
  
   let test_long_factor_negative_fail () =
     try
       let _ = test_long_factor_n ~long_factor:(-1) (-1) in
       Alcotest.fail "A negative long factor in a test should fail"
     with
     | _ -> ()
  
   let tests =
     ("Test.make ~long_factor", Alcotest.[
          test_case "make with custom long_factor" `Quick test_long_factor_10;
          test_case "make with default long_factor" `Quick test_long_factor_default;
          test_case "make with env long_factor" `Quick test_long_factor_env;
          test_case "make with 0 long_factor" `Quick test_long_factor_0;
          test_case "make with negative long_factor should fail"
            `Quick test_long_factor_negative_fail;
     ])
end

let () =
  Alcotest.run "QCheck"
    [
      Shrink.tests;
      Check_exn.tests;
      TestCount.tests;
      TestLongFactor.tests;
    ]
