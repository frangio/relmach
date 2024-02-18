let width_bits = 3
let width = 1 lsl width_bits (* width must be power of 2 *)

let bucket_size = 8

module PArray = struct
  let push a x =
    let l = Array.length a in
    let a' = Array.make (1 + l) x in
    Array.blit a 0 a' 0 l;
    a'

  let set a i x =
    let a' = Array.copy a in
    Array.set a' i x;
    a'
end

module Weak0 : sig
  val empty : 'a Weak.t
end = struct
  (* Cast into polymorphic array. Should be okay because it's empty. *)
  let empty = Weak.create 0 |> Obj.magic
end

module Make (H : Hashtbl.HashedType) = struct
  module HT = Hashtbl.Make(H)

  let ht_replace_with t f k v =
    let v' = HT.find_opt t k |> Option.fold ~none:v ~some:(f k v) in
    HT.replace t k v'

  let eph_query k e =
    match Ephemeron.get_key e with
    | k' when H.equal k k' -> Ephemeron.get_data_opt e
    | _ | exception Ephemeron.Empty -> None

  let eph_has k e =
    match Ephemeron.get_key e with
    | k' when H.equal k k' -> true
    | _ | exception Ephemeron.Empty -> false

  let weak_of x0 x1 =
    let w = Weak.create 2 in
    Weak.set w 0 (Some x0);
    Weak.set w 1 (Some x1);
    w

  let weak_any w x =
    let len = Weak.length w in
    let rec weak_any_from i =
      if i >= len then
        false
      else
        match Weak.get w i with
        | Some y when x == y -> true
        | _ -> weak_any_from (i + 1)
    in
    weak_any_from 0

  type key = H.t

  type 'a bucket = (key, 'a) Ephemeron.t array
  type 'a t =
    | Leaf of int * 'a bucket * 'a t Weak.t
    | Node of int * 'a t array * 'a t Weak.t

  let empty = Leaf (1, [||], Weak0.empty)

  let dimensions = function
    | Leaf _ -> 0, 1
    | Node (h, c, _) -> h, Array.length c

  let size t =
    let height, root_size = dimensions t in
    root_size * (1 lsl (width_bits * height))

  let get k t =
    let kh = H.hash k in
    let rec get' t =
      match t with
      | Leaf (_, b, _) -> Array.find_map (eph_query k) b
      | Node (h, c, _) ->
          let i = (kh lsr (h * width_bits)) mod Array.length c in
          get' (Array.get c i)
    in
    get' t

  let append t0 t1 =
    let w = weak_of t0 t1 in
    match t0, t1 with
    | Leaf _, Leaf _ ->
        Node (0, [|t0; t1|], w)
    | Node (h0, c0, _), Node (h1, c1, _) when h0 = h1 && Array.(length c0 = length c1) ->
        if Array.length c0 < width then
          Node (h0, Array.append c0 c1, w)
        else
          Node (h0 + 1, [|t0; t1|], w)
    | _ ->
        failwith "may only append equal size maps"

  let rec grow t factor = if factor = 1 then t else grow (append t t) (factor / 2)

  let set k v t =
    let root = t in
    let kh = H.hash k in
    let e = Ephemeron.make k v in
    let rec set' t =
      match t with
      | Node (h, c, w) ->
          let i = (kh lsr (h * width_bits)) mod Array.length c in
          let si', r = set' (Array.get c i) in
          Node (h, PArray.set c i si', w), r
      | Leaf (u, b, w) ->
          begin
            let i = 
              match Array.find_index (eph_has k) b with
              | Some _ as i -> i
              | None -> Array.find_index Ephemeron.is_empty b
            in
            match i with
            | Some i -> Leaf (u, PArray.set b i e, w), 0
            | None ->
                let s = Array.length b in
                if s < bucket_size then
                  Leaf (u, PArray.push b e, w), 0
                else
                  let curr_size = size root in
                  let resize = u = curr_size in
                  let size = if resize then curr_size * 2 else curr_size in
                  let z kh = (kh mod size) / u in
                  let zk = z kh in
                  let b' =
                    Array.fold_left
                      (fun b' e' ->
                        match Ephemeron.get_key_opt e' with
                        | Some k' when zk = z (H.hash k') -> e' :: b'
                        | _ -> b')
                      [e] b
                  in
                  let b' = Array.of_list b' in
                  let r = if resize then 1 + zk else 0 in
                  Leaf (size, b', w), r
          end
    in
    let t', r = set' t in
    match r with
    | 0 -> t'
    | 1 -> append t' t
    | _ -> append t t'

  let merge_buckets f b0 b1 =
    let f k e0 e1 = Ephemeron.(make k (f (get_data e0) (get_data e1))) in
    let t = HT.create (2 * bucket_size) in
    Array.iter
      (fun e1 ->
        match Ephemeron.get_key_opt e1 with
        | Some k -> HT.add t k e1
        | None -> ())
      b1;
    Array.iter
      (fun e0 ->
        match Ephemeron.get_key_opt e0 with
        | Some k -> ht_replace_with t f k e0
        | None -> ())
      b0;
    let b = Array.make (HT.length t) (Ephemeron.create ()) in
    Seq.iteri
      (fun i e -> Array.set b i e)
      (HT.to_seq_values t);
    b

  let merge_history = function Leaf (_, _, w) | Node (_, _, w) -> w

  let merge_with f t0 t1 =
    let merge_checked merge t0 t1 =
      if t0 == t1 || weak_any (merge_history t0) t1 then
        t0
      else if weak_any (merge_history t1) t0 then
        t1
      else
        merge t0 t1
    in
    let rec merge_same_size t0 t1 =
      let w = weak_of t0 t1 in
      match t0, t1 with
      | Leaf (u0, b0, _), Leaf (u1, b1, _) ->
          let u = min u0 u1 in
          let b = merge_buckets f b0 b1 in
          Leaf (u, b, w)
      | Node (h, cs0, _), Node (_, cs1, _) ->
          let cs = Array.map2 (merge_checked merge_same_size) cs0 cs1 in
          Node (h, cs, w)
      | _ -> assert false
    in
    let merge_resized t0 t1 =
      let s0 = size t0 in
      let s1 = size t1 in
      let t0 = if s0 >= s1 then t0 else grow t0 (s1 / s0) in
      let t1 = if s1 >= s0 then t1 else grow t1 (s0 / s1) in
      merge_same_size t0 t1
    in
    merge_checked merge_resized t0 t1

  let merge t0 t1 = merge_with (fun _ _ -> failwith "duplicate keys") t0 t1

  let print_weakmap =
    let rec print_weakmap pp_kv ~prefix = function
      | Leaf (u, b, _) ->
          Printf.printf "%s[" prefix;
          Array.iteri
            (fun i e ->
              if i > 0 then Printf.printf "; ";
              match Ephemeron.(get_key e, get_data e) with
              | exception _ -> Printf.printf "(-)"
              | kv -> Printf.printf "%a" pp_kv kv)
            b;
          Printf.printf "] (u=%d)\n" u
      | Node (h, cs, _) ->
          Printf.printf "%s┳ (h=%d)\n" prefix h;
          let last = Array.length cs - 1 in
          Array.iteri
            (fun i c ->
              let prefix = prefix ^ if i = last then "┗ " else "┣ " in
              print_weakmap pp_kv ~prefix c)
            cs
    in
    print_weakmap ~prefix:""
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

let%test_unit _ =
  let module S = struct
    type t = String.t ref
    let hash s = String.hash !s
    let equal = (==)
  end in
  let module W = Make(S) in
  let module H = Hashtbl.Make(S) in
  let arb = QCheck2.Gen.(let l = small_list (pair (map ref (small_string ~gen:char)) int) in pair l l) in
  let prop (ps0, ps1) =
    let h0 = H.of_seq (List.to_seq ps0) in
    let h1 = H.of_seq (List.to_seq ps1) in
    let m0 = H.fold W.set h0 W.empty in
    let m1 = H.fold W.set h1 W.empty in
    let m = W.merge_with max m0 m1 in
    let h = H.copy h0 in
    H.iter (fun k v ->
      let v' = match H.find h k with exception _ -> v | w -> max v w in
      H.replace h k v'
    ) h1;
    H.to_seq h |> Seq.iter (fun (k, v) ->
      match W.get k m with
      | None ->
          failwith (Printf.sprintf "key '%s' not found" !k)
      | Some wv when wv <> v ->
          failwith (Printf.sprintf "key '%s' got %d expected %d" !k wv v)
      | _ -> ()
    );
    true
  in
  QCheck2.Test.(check_exn (make arb prop))
