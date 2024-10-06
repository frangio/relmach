let opts = fun _ -> A | B | C | D

let _ =
  fresh x in
  x ~ opts Unit;
  x A
