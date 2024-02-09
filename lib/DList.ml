type 'a node =
  | Start of { mutable next : 'a node }
  | Node of {
    contents : 'a;
    mutable next : 'a node;
    mutable prev : 'a node;
  }

type 'a t = 'a node

type field = Next | Prev

let (.$()) n f =
  match f, n with
  | Next, (Node { next; _ } | Start { next }) -> next
  | Prev, Node { prev; _ } -> prev
  | _ -> invalid_arg "field error"

let (.$()<-) n f m =
  match f, n with
  | Next, Node n -> n.next <- m
  | Next, Start n -> n.next <- m
  | Prev, Node n -> n.prev <- m
  | Prev, Start _ -> ()

let create () =
  let rec start = Start { next = start } in
  start

let add x l =
  let n = Node { contents = x; next = l.$(Next); prev = l } in
  l.$(Next) <- n

let remove = function
  | Start _ -> invalid_arg "start node"
  | Node m as n ->
      m.next.$(Prev) <- m.prev;
      m.prev.$(Next) <- m.next;
      m.next <- n;
      m.prev <- n

let rec iter f = function
  | Start { next } -> iter f next
  | Node { contents; next; _ } -> f contents; iter f next

let stub () : 'a node =
  let rec node = Node { contents = Obj.magic (); next = node; prev = node } in
  node
