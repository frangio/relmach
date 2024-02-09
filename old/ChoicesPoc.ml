type path = Path of { leaf : int; height : int; parent : path }

type node = Node of { id : int; height : int; parent : path }

exception Empty

let height (Path { height; _ }) = height

let tail (Path { parent; _ } as p) = if parent == p then raise Empty else parent

let next_node_id = ref 0

let alloc_node_id () =
  let id = !next_node_id in
  next_node_id := id + 1;
  id

let make_root () = 
  let leaf = alloc_node_id () in
  let rec root = Path { leaf; height = 0; parent = root } in
  root

let make_succ parent =
  let id = alloc_node_id () in
  let height = 1 + height parent in
  Node { id; height; parent }

let make_edge (Node { id = leaf; height; parent }) =
  Path { leaf; height; parent }

type tree = path list

let tree_equal t0 t1 = try List.for_all2 (==) t0 t1 with Invalid_argument _ -> false

let rec make_tree p = if height p == 0 then [p] else p :: make_tree (tail p)

let rec join_trees (t0 : tree) (t1 : tree) : tree option =
  match t0, t1 with
  | t, [] | [], t -> Some t
  | (Path { leaf = pl; _ } as p) :: t0', (Path { leaf = ql; _ } as q) :: t1' ->
      if pl = ql then
        if p != q then
          None
        else
          join_trees t0' t1' |> Option.map (fun t -> p :: t)
      else if pl < ql then
        join_trees t0  t1' |> Option.map (fun t -> q :: t)
      else                                               
        join_trees t0' t1  |> Option.map (fun t -> p :: t)

let%test _ =
  let r = make_root () in
  let s = make_succ r in
  let rs = make_edge s in
  let t0 = make_succ rs in
  let rst0 = make_edge t0 in
  let t1 = make_succ rs in
  let rst1 = make_edge t1 in
  let u = make_succ r in
  let ru = make_edge u in
  let tree_rst = join_trees (make_tree rst0) (make_tree rst1) |> Option.get in
  let tree_ru = make_tree ru in
  let j = join_trees tree_rst tree_ru |> Option.get in
  tree_equal j [ru; rst1; rst0; rs; r]
