open QCheck

let triple_same =
  Test.make ~name:"triples have same components"
    (triple int int int)
    (Log.shrinks Print.(triple int int int) @@ fun (i,j,k) -> i=j || j=k)
;;
try Test.check_exn triple_same with Test.Test_fail _ -> ()
