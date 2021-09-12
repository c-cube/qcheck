open QCheck2

let quad_same =
  Test.make ~name:"quadruples have same components"
    Gen.(quad int int int int)
    (Log.shrinks Print.(quad int int int int) @@
     fun (h,i,j,k) -> h=i || i=j || j=k)
;;
try Test.check_exn quad_same with Test.Test_fail _ -> ()
