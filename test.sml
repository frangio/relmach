let fix =
  let f = fun x y -> y (fun z -> x x y z)
  in f f

let u0 = Z
let u1 = S u0
let u2 = S u1
let u3 = S u2
let u4 = S u3
let u5 = S u4
let u6 = S u5
let u7 = S u6
let u8 = S u7
let u9 = S u8

let unary = fix (fun unary n ->
  | n ~ 0; u0
  | n ~ 1; u1
  | n ~ 2; u2
  | n ~ 3; u3
  | n ~ 4; u4
  | n ~ 5; u5
  | n ~ 6; u6
  | n ~ 7; u7
  | n ~ 8; u8
  | n ~ 9; u0)

let decimal = fun n ->
  | n ~ u0; 0
  | n ~ u1; 1
  | n ~ u2; 2
  | n ~ u3; 3
  | n ~ u4; 4
  | n ~ u5; 5
  | n ~ u6; 6
  | n ~ u7; 7
  | n ~ u8; 8
  | n ~ u9; 9

let sub1 = fun n ->
  fresh n' in
  n ~ S n';
  n'

let ueq = fix (fun ueq n m ->
  | n ~ Z; m ~ Z; True
  | n ~ Z; sub1 m; False
  | m ~ Z; sub1 n; False
  | ueq (sub1 n) (sub1 m))

let eq = fun n m -> ueq (unary n) (unary m)

let add = fix (fun add n m -> 
  | n ~ Z; m
  | let n' = sub1 n in
    add n' (S m))

let mul = fun n ->
  let mul_n = fix (fun mul_n acc m ->
    | m ~ Z; acc
    | let m' = sub1 m in
      mul_n (add n acc) m')
  in mul_n Z

let square = fun n -> mul n n

let list_of = fun f ->
  let aux = fix (fun aux acc n ->
  | n ~ Z; acc Nil
  | let n' = sub1 n in
    fun x ->
      let fx = f x in
      aux (fun l -> acc (Cons fx l)) n')
  in aux (fun l -> l)

let head = fun xs -> fresh x _ in xs ~ Cons x _; x
let tail = fun xs -> fresh _ xs' in xs ~ Cons _ xs'; xs'

let drop = fix (fun drop n xs ->
  | n ~ Z; xs
  | let n' = sub1 n in
    drop n' (tail xs))

let take = fix (fun take n xs ->
  | n ~ Z; Nil
  | let n' = sub1 n in
    Cons (head xs) (take n' (tail xs)))

let range = fun start len xs -> take len (drop start xs)

let row = fun n -> range (mul n u9) u9

let map = fix (fun map f xs ->
  | xs ~ Nil; Nil
  | Cons (f (head xs)) (map f (tail xs)))

let cell = fun n ->
  | n ~ 0; (fresh x in x)
  | n ~ 1; n
  | n ~ 2; n
  | n ~ 3; n
  | n ~ 4; n
  | n ~ 5; n
  | n ~ 6; n
  | n ~ 7; n
  | n ~ 8; n
  | n ~ 9; n

let sudoku = list_of cell (square u9)
  9 0 5  4 0 0  1 0 7
  0 3 8  5 0 0  0 0 6
  0 4 0  3 2 7  0 0 0

  0 7 0  0 4 2  0 1 0
  1 6 2  0 0 0  8 0 0
  0 0 0  0 9 1  2 7 3

  2 0 0  9 3 8  0 0 4
  8 0 6  0 7 0  0 9 0
  0 0 0  0 0 0  7 8 1

let _ = row u2 sudoku
