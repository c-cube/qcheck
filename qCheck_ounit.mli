

val to_ounit_test : ?msg:string -> QCheck.test -> OUnit2.test

val to_ounit_suite : QCheck.suite -> OUnit2.test list
