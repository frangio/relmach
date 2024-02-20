module Make (H : Hashtbl.HashedType) = struct
  module HT = Hashtbl.Make(H)

  type key = H.t
  type 'a t = 'a HT.t

  let empty = (HT.create 0 |> Obj.magic)

  let size t = HT.length t

  let get k t = HT.find_opt t k

  let set k v t =
    let t = HT.copy t in
    HT.add t k v;
    t

  let merge_with f t0 t1 =
    let t = HT.copy t0 in
    HT.iter
      (fun k v1 ->
        let v =
          match HT.find t0 k with
          | exception Not_found -> v1
          | v0 -> f v0 v1
        in
          HT.replace t k v)
      t1;
    t

  let merge t0 t1 = merge_with (fun _ _ -> failwith "duplicate keys") t0 t1
end

let%test_unit _ =
  let module W = Make(String) in
  let a, b, c, d, e = "a", "b", "c", "d", "e" in
  let t0 = W.empty in
  let t1 = W.set a b t0 in
  assert (W.get a t1 = Some b);
  let t2 = W.set b c t1 in
  let t2 = W.set c d t2 in
  let t2 = W.set d e t2 in
  assert (W.get a t2 = Some b);
  assert (W.get b t2 = Some c);
  assert (W.get c t2 = Some d);
  assert (W.get d t2 = Some e)

let%test_unit _ =
  let module W = Make(String) in
  let a, b, c, d, e = "a", "b", "c", "d", "e" in
  let t0 = W.empty |> W.set a b |> W.set b c in
  let t1 = W.empty |> W.set c d |> W.set d e in
  let t = W.merge t0 t1 in
  assert (W.get a t = Some b);
  assert (W.get b t = Some c);
  assert (W.get c t = Some d);
  assert (W.get d t = Some e)

let%test_unit _ =
  let module W = Make(String) in
  let module H = Hashtbl.Make(String) in
  let arb = QCheck.(list (pair small_string int)) in
  let prop ps =
    let h = H.of_seq (List.to_seq ps) in
    let m = H.fold W.set h W.empty in
    H.iter (fun k v ->
      match W.get k m with
      | None ->
          failwith (Printf.sprintf "key %s not found" k)
      | Some wv when wv <> v ->
          failwith (Printf.sprintf "key %s got %d expected %d" k wv v)
      | _ -> ()
    ) h;
    true
  in
  QCheck.Test.(check_exn (make arb prop))

let%test_unit _ =
  let module W = Make(String) in
  let module H = Hashtbl.Make(String) in
  let arb = QCheck.(let l = list (pair small_string int) in pair l l) in
  let prop (ps0, ps1) =
    let h0 = H.of_seq (List.to_seq ps0) in
    let h1 = H.of_seq (List.to_seq ps1 |> Seq.filter (fun (k, _) -> not (H.mem h0 k))) in
    let m0 = H.fold W.set h0 W.empty in
    let m1 = H.fold W.set h1 W.empty in
    let m = W.merge m0 m1 in
    H.to_seq h0 |> Seq.append (H.to_seq h1) |> Seq.iter (fun (k, v) ->
      match W.get k m with
      | None ->
          failwith (Printf.sprintf "key %s not found" k)
      | Some wv when wv <> v ->
          failwith (Printf.sprintf "key %s got %d expected %d" k wv v)
      | _ -> ()
    );
    true
  in
  QCheck.Test.(check_exn (make arb prop))
