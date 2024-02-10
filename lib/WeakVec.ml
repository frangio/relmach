(* todo: compact after gc *)

type 'a t = { mutable size : int; mutable contents : 'a Weak.t }

let create () = { size = 0; contents = Weak.create 0 }

let grow xs =
  let { contents; _ } = xs in
  let n = Weak.length contents in
  let contents' = Weak.create (max 1 (n * 2)) in
  Weak.blit contents 0 contents' 0 n;
  xs.contents <- contents'

let add x xs =
  let { size; _ } = xs in
  let capacity = Weak.length xs.contents in
  if capacity = size then
    grow xs;
  Weak.set xs.contents size (Some x);
  xs.size <- size + 1

let iter f { size; contents } =
  for i = 0 to size - 1 do
    Weak.get contents i |> Option.iter f
  done
