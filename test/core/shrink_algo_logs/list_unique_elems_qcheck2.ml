open QCheck2

let list_unique_elems =
  Test.make ~name:"lists have unique elems"
    Gen.(list small_int)
    (Log.shrinks Print.(list int) @@
     fun xs -> let ys = List.sort_uniq Int.compare xs in
               List.length xs = List.length ys)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand list_unique_elems with Test.Test_fail _ -> ()
