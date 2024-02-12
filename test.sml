let fix =
  let f = fun x y -> y (fun z -> x x y z)
  in f f

let unary = fix (fun unary n ->
  | n ~ 0; Z
  | n ~ 1; S (unary 0)
  | n ~ 2; S (unary 1)
  | n ~ 3; S (unary 2)
  | n ~ 4; S (unary 3)
  | n ~ 5; S (unary 4)
  | n ~ 6; S (unary 5)
  | n ~ 7; S (unary 6)
  | n ~ 8; S (unary 7)
  | n ~ 9; S (unary 8))

let decimal = fix (fun decimal n ->
  | n ~ unary 0; 0
  | n ~ unary 1; 1
  | n ~ unary 2; 2
  | n ~ unary 3; 3
  | n ~ unary 4; 4
  | n ~ unary 5; 5
  | n ~ unary 6; 6
  | n ~ unary 7; 7
  | n ~ unary 8; 8
  | n ~ unary 9; 9)

let add = fix (fun add n m ->
  | n ~ Z; m
  | fresh n' in n ~ S n'; S (add n' m))

let gte = fun n m -> n ~ (fresh x in add m x)

let _ = gte (unary 3) (unary 3); True
