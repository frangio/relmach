open Hashcons

type tree = tree_node hash_consed
and tree_node =
  | Root of int
  | Tree of {
      max_leaf : int;
      max_leaf_edge : int;
      max_leaf_parent : tree;
      meet : tree;
      prev : tree Weak.t;
    }

let weak_get w = Option.get (Weak.get w 0)

let get_meet ({ node ; _ } as t) =
  match node with
  | Root _ -> t
  | Tree { meet; _ } -> meet

module TreeNode = struct
  type t = tree_node

  let equal t0 t1 =
    match t0, t1 with
    | Root id0, Root id1 -> id0 = id1
    | Root _, _ | _, Root _ -> false
    | Tree t0, Tree t1 ->
        t0.max_leaf = t1.max_leaf &&
        t0.max_leaf_edge = t1.max_leaf_edge &&
        weak_get t0.prev == weak_get t1.prev

  let hash = function
    | Root id -> Hashtbl.hash id
    | Tree t -> 
        Hashtbl.hash (t.max_leaf, t.max_leaf_edge, t.max_leaf_parent.hkey, (weak_get t.prev).hkey)
end

module HTree = Hashcons.Make(TreeNode)

let ht = HTree.create 251

let next_id = ref 0

let alloc_id () =
  let id = !next_id in
  next_id := id + 1;
  id

let init () = 
  let id = alloc_id () in
  HTree.hashcons ht (Root id)

type leaf = Leaf of { id : int; parent : tree; prev : tree Weak.t }

let extend t =
  let id = alloc_id () in
  let prev = Weak.create 1 in
  Weak.set prev 0 (Some t);
  Leaf { id; parent = get_meet t; prev }

let branch (Leaf { id; parent; prev }) =
  let max_leaf_edge = alloc_id () in
  let t = Tree {
    max_leaf = id;
    max_leaf_edge;
    max_leaf_parent = parent;
    meet = parent;
    prev;
  }
  in
  Hashcons.hashcons ht t

let tree_equal t0 t1 = try List.for_all2 (==) t0 t1 with Invalid_argument _ -> false

let rec make_tree p = if height p == 0 then [p] else p :: make_tree (tail p)

let rec join (t0 : tree) (t1 : tree) : tree option =
  match t0, t1 with
  | { node = Root id0; _ }, { node = Root id1; _ } ->
      if id0 = id1 then
        Some t0
      else
        raise (Invalid_argument "Disjoint trees")
  | t, { node = Root _; _ } | { node = Root _; _ }, t -> Some t
  | (Path { leaf = pl; _ } as p) :: t0', (Path { leaf = ql; _ } as q) :: t1' ->
      if pl = ql then
        if p != q then
          None
        else
          join t0' t1' |> Option.map (fun t -> p :: t)
      else if pl < ql then
        join t0  t1' |> Option.map (fun t -> q :: t)
      else                                               
        join t0' t1  |> Option.map (fun t -> p :: t)

let%test _ =
  let r = init () in
  let s = extend r in
  let rs = branch s in
  let t0 = extend rs in
  let rst0 = branch t0 in
  let t1 = extend rs in
  let rst1 = branch t1 in
  let u = extend r in
  let ru = branch u in
  let tree_rst = join (make_tree rst0) (make_tree rst1) |> Option.get in
  let tree_ru = make_tree ru in
  let j = join tree_rst tree_ru |> Option.get in
  tree_equal j [ru; rst1; rst0; rs; r]
