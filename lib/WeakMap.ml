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

type ('k, 'a) _bucket = ('k, 'a) Ephemeron.t array
type ('k, 'a) _t =
  | Leaf of int * ('k, 'a) _bucket
  | Node of int * ('k, 'a) _t array

module Make (H : Hashtbl.HashedType) = struct
  module HT = Hashtbl.Make(H)

  let ht_replace_with t f k v =
    let v' = HT.find_opt t k |> Option.fold ~none:v ~some:(f v) in
    HT.replace t k v'

  type key = H.t
  type 'a t = (key, 'a) _t

  let empty = Leaf (1, [||])

  let dimensions = function
    | Leaf _ -> 0, 1
    | Node (h, c) -> h, Array.length c

  let size t =
    let height, root_size = dimensions t in
    root_size * (1 lsl (width_bits * height))

  let get k t =
    let kh = H.hash k in
    let rec get' t =
      match t with
      | Leaf (_, b) -> Array.find_map (Ephemeron.query k) b
      | Node (h, c) ->
          let i = (kh lsr (h * width_bits)) mod Array.length c in
          get' (Array.get c i)
    in
    get' t

  let append t0 t1 =
    match t0, t1 with
    | Leaf _, Leaf _ ->
        Node (0, [|t0; t1|])
    | Node (h0, c0), Node (h1, c1) when h0 = h1 && Array.(length c0 = length c1) ->
        if Array.length c0 < width then
          Node (h0, Array.append c0 c1)
        else
          Node (h0 + 1, [|t0; t1|])
    | _ ->
        failwith "may only append equal size maps"

  let set k v t =
    let root = t in
    let kh = H.hash k in
    let e = Ephemeron.make k v in
    let rec set' t =
      match t with
      | Node (h, c) ->
          let i = (kh lsr (h * width_bits)) mod Array.length c in
          let si', r = set' (Array.get c i) in
          Node (h, PArray.set c i si'), r
      | Leaf (u, b) ->
          begin
            let i = 
              match Array.find_index (Ephemeron.has k) b with
              | Some _ as i -> i
              | None -> Array.find_index Ephemeron.empty b
            in
            match i with
            | Some i -> Leaf (u, PArray.set b i e), 0
            | None ->
                let s = Array.length b in
                if s < bucket_size then
                  Leaf (u, PArray.push b e), 0
                else
                  let root_height, root_size = dimensions root in
                  let size = root_size * (1 lsl (width_bits * root_height)) in
                  let z kh = (kh / u) mod (1 lsl (size / u)) in
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
                  if u < size then
                    Leaf (size, b'), 0
                  else
                    Leaf (size * 2, b'), 1 + zk
          end
    in
    let t', r = set' t in
    match r with
    | 0 -> t'
    | 1 -> append t' t
    | _ -> append t t'

  type ('a, 'b) merge_directive = Either | Left of 'a | Right of 'a | Other of 'b

  let bucket_items b len =
    let t = HT.create (2 * bucket_size) in
    for i = 0 to len - 1 do
      let e = Array.get b i in
      match Ephemeron.get_key_opt e with
      | Some k -> HT.add t k e
      | None -> ()
    done;
    t

  let merge_fold acc next l r f =
    match acc, next with
    | Either, Left _ -> Left l
    | Either, Right _ -> Right r
    | Left _, Left _ | Right _, Right _ | _, Either -> acc
    | (Other _ | Left _), Right _ | (Other _ | Right _), Left _ | _, Other _ -> f ()

  let merge_buckets f b0 b1 =
    let f k e0 e1 = Ephemeron.(make k (f (get_data e0) (get_data e1))) in
    let n0 = Array.length b0 in
    let n1 = Array.length b1 in
    let rec aux i acc =
      if i >= n0 && i >= n1 then
        acc
      else
        let next = 
          if i < n0 && i < n1 then
            let e0 = Array.get b0 i in
            let e1 = Array.get b1 i in
            match Ephemeron.(empty e0, empty e1) with
            | true, true -> Either
            | false, true -> Left e0
            | true, false -> Right e1
            | false, false ->
                if e0 == e1 then
                  Either
                else
                  match Ephemeron.(get_key e0, get_key e1) with
                  | k0, k1 when k0 == k1 -> Other [f k0 e0 e1]
                  | _ -> Other [e0; e1]
          else if i < n0 then
            let e0 = Array.get b0 i in
            if Ephemeron.empty e0 then Either else Left e0
          else if i < n1 then
            let e1 = Array.get b1 i in
            if Ephemeron.empty e1 then Either else Right e1
          else
            Either
        in
        let acc' = merge_fold acc next b0 b1 @@ fun () ->
          let es =
            match acc with
            | Other acc -> acc
            | Left b | Right b -> bucket_items b i
            | Either -> bucket_items (if n0 > n1 then b0 else b1) i
          in
          match next with
          | Left e | Right e ->
              let k = Ephemeron.get_key e in
              ht_replace_with es (f k) k e;
              Other es
          | Other es' ->
              List.iter
                (fun e ->
                  let k = Ephemeron.get_key e in
                  ht_replace_with es (f k) k e)
                es';
              Other es
          | Either -> assert false
        in
        aux (i + 1) acc'
    in
    match aux 0 Either with
    | Other es -> Other (Array.of_seq (HT.to_seq_values es))
    | Left _ -> Left ()
    | Right _ -> Right ()
    | Either -> Either

  let merge_with f t0 t1 =
    let rec resize t0 t1 =
      let s0 = size t0 in
      let s1 = size t1 in
      if s0 < s1 then
        resize (append t0 t0) t1
      else if s0 > s1 then
        resize t0 (append t1 t1)
      else
        t0, t1
    in
    let rec merge' t0 t1 =
      if t0 == t1 then
        Either
      else
        match t0, t1 with
        | Leaf (u0, b0), Leaf (u1, b1) ->
            begin
              match merge_buckets f b0 b1 with
              | Other b -> Other (Leaf (min u0 u1, b))
              | Left _ -> Left t0
              | Right _ -> Right t1
              | Either -> Either
            end
        | Node (h, cs0), Node (_, cs1) ->
            begin
              match merge_children cs0 cs1 with
              | Other cs -> Other (Node (h, cs))
              | Left _ -> Left t0
              | Right _ -> Right t1
              | Either -> Either
            end
        | _ -> assert false
    and merge_children cs0 cs1 =
      Seq.fold_left2
        (fun acc (i, c0) c1 ->
          let next = merge' c0 c1 in
          merge_fold acc next cs0 cs1 @@ fun () ->
            let cs =
              match acc with
              | Other cs -> cs
              | Left cs | Right cs -> Array.copy cs
              | Either -> Array.copy cs0
            in
            let c =
              match next with
              | Right c | Left c | Other c -> c
              | Either -> assert false
            in
            Array.set cs i c;
            Other cs)
        Either (Array.to_seqi cs0) (Array.to_seq cs1)
    in
    let t0, t1 = resize t0 t1 in
    match merge' t0 t1 with
    | Left t | Right t | Other t -> t
    | Either -> t0

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
