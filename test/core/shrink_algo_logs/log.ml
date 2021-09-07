open QCheck2

let shrinks print law =
  fun input ->
    let pass = law input in
    Printf.printf "%s %s\n" (if pass then "holds" else "fails") (print input);
    pass

let shrinks_int = shrinks string_of_int

let shrinks_char = shrinks Print.char

let shrinks_string = shrinks Print.string

let length_printer xs =
  Printf.sprintf "[...] list length: %i" (List.length xs)

let shrinks_list_length law = shrinks length_printer law
