open AllocHash

module type Params = sig
  type clos_body
end

module Make (P : Params) = struct
  type unifiable =
    | Sym of int * string
    | Struct of int * string * unifiable list
    | Clos of int * string * P.clos_body * unifiable_env

  and unifiable_env = unifiable list

  module Unifiable = struct
    type t = unifiable
    let equal = (==)
    let hash = function Sym (h, _) | Struct (h, _, _) | Clos (h, _, _, _) -> h
  end

  let make_symbol lbl =
    let h = alloc_hash () in
    Sym (h, lbl)

  let make_closure var body env =
    let h = alloc_hash () in
    Clos (h, var, body, env)

  let make_struct c =
    let h = Hashtbl.hash c in
    Struct (h, c, [])

  let cons_struct s v =
    match s with
    | Struct (h, c, vs) ->
        let h' = Hashtbl.hash (h, Unifiable.hash v) in
        Struct (h', c, v :: vs)
    | _ -> raise (Invalid_argument "not a struct")

  type unifier = (unifiable, node) WeakMap._t
  and node = { rank : int; parent : unifiable }

  module Unifier = struct
    module M = WeakMap.Make(Unifiable)

    type t = unifier

    let empty = M.empty

    let rec root u v =
      match M.get v u with
      | Some { parent = Sym _ as p; _ } when p != v -> root u p
      | n -> n

    let find u v =
      match root u v with
      | None -> v
      | Some { parent; _ } -> parent

    let rec subst u v =
      match v with
      | Sym _ -> find u v
      | Struct (_, c, vs) ->
          let s = make_struct c in
          List.fold_left cons_struct s (List.map (subst u) vs)
      | Clos (h, x, t, e) ->
          Clos (h, x, t, List.map (subst u) e)

    let occurs u x v =
      let x = find u x in
      let rec occurs' = function
        | Sym _ as y -> x == find u y
        | Struct (_, _, vs) | Clos (_, _, _, vs) -> List.exists occurs' vs
      in
      occurs' v

    let unify u v w =
      let rec unify v w u =
        match v, w with
        | Sym _, _ | _, Sym _ -> unify_sym v w u
        | Clos _, Clos _ -> if v == w then Some u else None
        | Struct (_, c, vs), Struct (_, d, ws) -> if c = d then unify_list vs ws u else None
        | _, _ -> None

      and unify_list vs ws u =
        match vs, ws with
        | [], [] -> Some u
        | v :: vs', w :: ws' -> Option.bind (unify v w u) (unify_list vs' ws')
        | _, _ -> None

      and unify_sym v w u =
        let nv = root u v in
        let nw = root u w in
        let eq =
          match nv, nw with
          | Some nv, Some nw -> nv == nw
          | _ -> v == w
        in
        if eq then
          Some u
        else
          let nv = match nv with Some nv -> nv | None -> { rank = 0; parent = v } in
          let nw = match nw with Some nw -> nw | None -> { rank = 0; parent = w } in
          match nv, nw with
          | { parent = Sym _; _ }, { parent = Sym _; _ } ->
              let n0, n1 = if nv.rank > nw.rank then nv, nw else nw, nv in
              let p = n0.parent in
              let u = M.set n1.parent { n1 with parent = p } u in
              if n0.rank = n1.rank then
                Some (M.set n0.parent { n0 with rank = n0.rank + 1 } u)
              else
                Some u
          | ({ parent = Sym _; _ } as nx), nv | nv, ({ parent = Sym _; _ } as nx) ->
              let x = nx.parent in
              let v = nv.parent in
              if occurs u x v then
                None
              else
                Some (M.set x { nx with parent = v } u)
          | _ ->
              unify nv.parent nw.parent u

      in unify v w u

    let merge u0 u1 =
      let q = Queue.create () in
      let f a b = Queue.add (a.parent, b.parent) q; a in
      let u = M.merge_with f u0 u1 in
      let rec resolve u =
        match Queue.take q with
        | exception Queue.Empty -> Some u
        | a, b -> Option.bind (unify u a b) resolve
      in
      resolve u
  end
end
