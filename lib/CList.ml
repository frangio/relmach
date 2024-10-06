(* Persistent output-restricted deques with O(1) amortized costs based on Okasaki's CatenableList. *)

type 'a t = Nil | Cat of 'a * 'a t Lazy.t PQueue.t

exception Empty

let empty = Nil

let is_empty = function
  | Nil -> true
  | Cat _ -> false

let link xs s =
  match xs with
  | Nil -> raise Empty
  | Cat (x, q) -> Cat (x, PQueue.snoc s q)

let rec link_all q =
  let lazy xs = PQueue.head q in
  let q' = PQueue.tail q in
  if PQueue.is_empty q' then
    xs
  else
    link xs (lazy (link_all q'))

let append xs ys =
  match xs, ys with
  | Nil, xs | xs, Nil -> xs
  | xs, ys -> link xs (lazy ys)

let snoc x xs = append xs (Cat (x, PQueue.empty))

let head = function
  | Nil -> raise Empty
  | Cat (x, _) -> x

let tail = function
  | Nil -> raise Empty
  | Cat (_, q) -> if PQueue.is_empty q then Nil else link_all q

let map f xs =
  let rec aux acc xs =
    match head xs, tail xs with
    | exception Empty -> acc
    | x, xs' -> aux (snoc (f x) acc) xs'
  in
  aux empty xs
