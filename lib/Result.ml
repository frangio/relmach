open Types

let value v = Result (v, CList.empty)

let stuck v sk = Result (v, CList.(snoc sk empty))

let is_stuck (Result (_, skq)) = not (CList.is_empty skq)

let rec map_result f (Result (v, skq)) =
  let map_stuck_cont = function
    | SLeftOf (op, r) -> SLeftOf (op, map_result f r)
    | SRightOf (op, r) -> SRightOf (op, map_result f r)
  in
  Result (f v, CList.map map_stuck_cont skq)

let extend1 (Result (v, skq)) sk = Result (v, CList.snoc sk skq)
let extend (Result (v, skq1)) skq2 = Result (v, CList.append skq1 skq2)

module H = Hashtbl.Make(Unifiable)

let symbol_idx symbols v =
  try
    H.find symbols v
  with Not_found ->
    let i = H.length symbols in
    H.add symbols v i;
    i

let decode r =
  let rec decode_value symbols v =
    match v with
    | Sym _ -> Var (symbol_idx symbols v)
    | Struct (_, c, vs) -> List.fold_right (fun r l -> Bin (App, l, decode_value symbols r)) vs (Const c)
    | Clos (_, x, t, e) -> Lam (x, List.map (decode_term ~free:1 symbols e) t)

  and decode_term ?(free = 0) symbols e t =
    match t with
    | Const c -> Const c
    | Var i -> if i < free then Var i else decode_value symbols (List.nth e (i - free))
    | Bin (op, t, u) -> Bin (op, decode_term ~free symbols e t, decode_term ~free symbols e u)
    | Lam (x, t) -> Lam (x, List.map (decode_term ~free:(free + 1) symbols e) t)
    | Nu (x, t) -> Nu (x, decode_term ~free:(free + 1) symbols e t)
  in

  let symbols = H.create 0 in

  let rec decode' (Result (v, kq)) =
    aux kq (decode_value symbols v)
  and aux q t =
    match CList.(head q, tail q) with
    | exception CList.Empty -> t
    | SLeftOf (op, r), q' -> aux q' (Bin (op, t, decode' r))
    | SRightOf (op, r), q' -> aux q' (Bin (op, decode' r, t))
  in

  let t = decode' r in
  let symbols = H.to_seq symbols |> Array.of_seq in
  Array.sort (fun a b -> compare (snd a) (snd b)) symbols;
  Array.fold_left
    (fun t (x, i) ->
      match x with
      | Sym (_, x) -> Nu (Printf.sprintf "%s%d" x i, t)
      | _ -> failwith "not a symbol")
    t symbols
