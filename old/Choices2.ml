type branch = { id : int }

module Branch = struct
  type t = branch
  let compare b0 b1 = Int.compare b0.id b1.id
  let equal b0 b1 = b0 == b1
  let hash b = Hashtbl.hash b.id
end

module BranchWeakMap = WeakMap.Make(Branch)

type choice = branch * int

type next_choices = (choice list * choice) list

type t = {
  stack : choice list;
  next : next_choices;
  all : int BranchWeakMap.t;
}

let root = { stack = []; next = []; all = BranchWeakMap.empty }

let next_id = ref 0

let alloc_id () =
  let id = !next_id in
  next_id := id + 1;
  id

let alloc_branch () =
  { id = alloc_id () }

let choice c =
  let b = alloc_branch () in
  fun () ->
    let i = alloc_id () in
    { c with stack = (b, i) :: c.stack; all = BranchWeakMap.insert b i c.all }

exception Not_joinable

let check_next n0 s1 a1 =
  match n0 with
  | (s, (b, i)) :: n0' when s == s1 ->
      let j = BranchWeakMap.get b a1 |> Option.get in
      if i != j then
        raise Not_joinable
      else
        n0'
  | n0 -> n0

let rec merge_next (n0 : next_choices) (n1 : next_choices) : next_choices =
  if n0 == n1 then
    n0
  else
    match n0, n1 with
    | [], [] -> n0
    | n, [] | [], n -> n
    | ((s0, _) as x0 :: n0'), ((s1, _) as x1 :: n1') ->
        let s0_gt_s1 =
          match s0, s1 with
          | [], [] -> failwith "broken invariant" (* should be guaranteed by check_next *)
          | _ :: _, [] -> true
          | [], _ :: _ -> false
          | (b0, _) :: _, (b1, _) :: _ -> b0.id > b1.id
        in
        if s0_gt_s1 then
          x0 :: merge_next n0' n1
        else
          x1 :: merge_next n0 n1'

type join_analysis = {
  stem : choice list;
  split0 : choice option;
  split1 : choice option;
  next0 : next_choices;
  next1 : next_choices;
}

let join_analyze c0 c1 : join_analysis =
  let rec find_split' s0 n0 s1 n1 =
    let n0 = check_next n0 s1 c1.all in
    let n1 = check_next n1 s0 c0.all in
    match s0, s1 with
    | [], [] -> { stem = s0; split0 = None; split1 = None; next0 = n0; next1 = n1 }
    | [k0], [] -> { stem = s1; split0 = Some k0; split1 = None; next0 = n0; next1 = n1 }
    | [], [k1] -> { stem = s0; split0 = None; split1 = Some k1; next0 = n0; next1 = n1 }
    | _ :: s0', [] -> find_split' s0' n0 s1 n1
    | [], _ :: s1' -> find_split' s0 n0 s1' n1
    | ((b0, _) as k0 :: s0'), ((b1, _) as k1 :: s1') ->
        if s0' == s1' then
          { stem = s0'; split0 = Some k0; split1 = Some k1; next0 = n0; next1 = n1 }
        else if b0.id < b1.id then
          find_split' s0' n0 s1 n1
        else
          find_split' s0 n0 s1' n1
  in
  find_split' c0.stack c0.next c1.stack c1.next

let joinable c0 c1 =
  match join_analyze c0 c1 with
  | { split0 = Some (b0, i0); split1 = Some (b1, i1); _ } ->
      if b0 == b1 then
        i0 = i1
      else
        Option.fold (BranchWeakMap.get b1 c0.all) ~some:(fun i -> i == i1) ~none:true &&
        Option.fold (BranchWeakMap.get b0 c1.all) ~some:(fun i -> i == i0) ~none:true
  | _ -> true

module JoinCache = Ephemeron.K2.Make(Branch)(Branch)

let join_cache = JoinCache.create 251

let join c0 c1 =
  let all = BranchWeakMap.merge c0.all c1.all in
  match join_analyze c0 c1 with
  | { split0 = Some _; split1 = None; _ } -> c0
  | { split0 = None; _ } -> c1
  | { split0 = Some (b0, i0); split1 = Some (b1, i1); _ } as a ->
      if b0 == b1 && i0 <> i1 then
        raise Not_joinable
      else
        let b =
          try JoinCache.find join_cache (b0, b1)
          with Not_found ->
            let b = alloc_branch () in
            JoinCache.add join_cache (b0, b1) b;
            b
        in
        let i = alloc_id () in
        let stack = (b, i) :: a.stem in
        let next = merge_next a.next0 a.next1 in
        let all = BranchWeakMap.insert b i all in
        { stack; next; all }

let print_choice c =
  Printf.printf "stack length = %i\n%!" (List.length c.stack);
  match c.stack with
  | [] -> print_endline "no choices taken"
  | [(b, i)] -> Printf.printf "branch %i option %i\n%!" b.id i; 
  | _ -> failwith "not a final choice"
