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

let sub1 = fun n ->
  fresh n' in
  n ~ S n';
  n'

let lte = fix (fun lte n m ->
  | n ~ m
  | let m' = sub1 m in
    lte n m')

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

let head = fun xs -> fresh x _ in xs ~ Cons x _; x
let tail = fun xs -> fresh _ xs' in xs ~ Cons _ xs'; xs'

let drop = fix (fun drop n xs ->
  | n ~ Z; xs
  | sub1 n; xs ~ Nil; Nil
  | let n' = sub1 n in
    drop n' (tail xs))

let take = fix (fun take n xs ->
  | n ~ Z; Nil
  | let n' = sub1 n in
    Cons (head xs) (take n' (tail xs)))

let append = fix (fun append xs ys ->
  | xs ~ Nil; ys
  | Cons (head xs) (append (tail xs) ys))

let choose = fix (fun choose x ys ->
  | x ~ head ys; tail ys
  | let ys' = choose x (tail ys) in
    Cons (head ys) ys')

let choose_pairs = fix (fun choose_pairs xs ys ->
  | xs ~ Nil; ys ~ Nil
  | let ys' = choose (head xs) ys in
    choose_pairs (tail xs) ys')

let digits = list u9 1 2 3 4 5 6 7 8 9

let valid_group = fun g -> choose_pairs g digits

let row = fun n s -> take u9 (drop (mul n u9) s)

let col = fun n s ->
  let aux = fix (fun aux s ->
    | s ~ Nil; Nil
    | let x = head s in
      Cons x (aux (drop u9 s)))
  in aux (drop n s)

let box = fun n s ->
  let adjust = fix (fun adjust n s ->
    | let _ = lte n u2 in
      drop (mul u3 n) s
    | fresh n' in
      let _ = n ~ S (S (S n')) in
      adjust n' (drop (mul u9 u3) s))
  in
  let s = adjust n s in
  let l0 = take u3 s in
  let s = drop u9 s in
  let l1 = take u3 s in
  let s = drop u9 s in
  let l2 = take u3 s in
  append l0 (append l1 l2)

let solve = fun s ->
  valid_group (box u0 s); valid_group (box u1 s); valid_group (box u2 s);
  valid_group (row u0 s); valid_group (row u1 s); valid_group (row u2 s);
  valid_group (col u0 s); valid_group (col u1 s); valid_group (col u2 s);
  valid_group (box u3 s); valid_group (box u4 s); valid_group (box u5 s);
  valid_group (row u3 s); valid_group (row u4 s); valid_group (row u5 s);
  valid_group (col u3 s); valid_group (col u4 s); valid_group (col u5 s);
  valid_group (box u6 s); valid_group (box u7 s); valid_group (box u8 s);
  valid_group (row u6 s); valid_group (row u7 s); valid_group (row u8 s);
  valid_group (col u6 s); valid_group (col u7 s); valid_group (col u8 s)

let cell = fun n ->
  | n ~ 0; (fresh x in x)
  | let digit = (fun _ -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9) Unit in
    n ~ digit; n

let sudoku = list_of cell (square u9)
  0 5 0  0 0 8  1 6 0
  2 0 0  3 1 0  0 7 9
  6 7 1  5 0 0  0 4 0

  7 0 6  2 0 1  8 0 0
  0 2 0  9 6 5  0 0 0
  0 4 5  0 0 0  9 0 0

  9 0 0  0 3 0  2 0 0
  4 0 2  0 0 9  0 0 7
  0 0 0  6 2 7  0 9 1

let _ =
  solve sudoku;
  sudoku
