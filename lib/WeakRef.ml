type 'a t = 'a Weak.t

let create (x : 'a) : 'a t =
  let w = Weak.create 1 in
  Weak.set w 0 (Some x);
  w

let get (w : 'a t) = Weak.get w 0

let iter f w = Option.iter f (get w)
