(* Persistent queues with O(1) amortized costs based on Okasaki's PhysicistsQueue. *)

type 'a t = {
  w : 'a list;
  f : 'a list Lazy.t;
  len_f : int;
  r : 'a list;
  len_r : int;
}

exception Empty

let empty = { w = []; f = lazy []; len_f = 0; r = []; len_r = 0 }

let is_empty { len_f; _ } = len_f = 0

let queue ~w ~f ~len_f ~r ~len_r =
  let check_w ~w ~f ~len_f ~r ~len_r =
    match w with
    | [] -> { w = Lazy.force f; f; len_f; r; len_r }
    | _ -> { w; f; len_f; r; len_r }
  in
  if len_r <= len_f then
    check_w ~w ~f ~len_f ~r ~len_r
  else
    let w = Lazy.force f in
    let f = lazy (w @ List.rev r) in
    let len_f = len_f + len_r in
    check_w ~w ~f ~len_f ~r:[] ~len_r:0

let snoc x { w; f; len_f; r; len_r } =
  let r = x :: r in
  let len_r = len_r + 1 in
  queue ~w ~f ~len_f ~r ~len_r

let head = function
  | { w = []; _ } -> raise Empty
  | { w = x :: _; _ } -> x

let tail = function
  | { w = []; _ } -> raise Empty
  | { w = _ :: w; f; len_f; r; len_r } ->
      let f = lazy (List.tl (Lazy.force f)) in
      let len_f = len_f - 1 in
      queue ~w ~f ~len_f ~r ~len_r

let map f xs =
  let rec aux acc xs =
    match head xs, tail xs with
    | exception Empty -> acc
    | x, xs' -> aux (snoc (f x) acc) xs'
  in
  aux empty xs
