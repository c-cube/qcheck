(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

module Make (QCheck_common : QCheck_base_runner_intf.QCHECK) : QCheck_base_runner_intf.QCHECK_BASE_RUNNER with module QCheck_common := QCheck_common
