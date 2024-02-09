open Relmach
open AllocHash
open Unifier.Value

let (let*) = Option.bind

type binop = App | Seq | Unif

type term =
  | Var of int
  | Const of string
  | Bin of binop * term * term
  | Lam of string * term list
  | Nu of string * term

(* todo: rename Unifier.Value._value -> Unifier.Unifiable.t ? *)
type value = (term list, symbol_meta) _value

and env = value list

and unifier = value Unifier._t

and universe = {
  mutable unifier : unifier;
  mutable dead : bool;
  mutable frontier : cont;
  mutable crossings : (cont * universe list) list;
  log : (universe, unit) WeakMap._t;
  hash : int;
}

and cont = Ret | Left of frame | Right of frame

and frame = {
  op : binop;
  parent : cont;
  depth : int;
  mutable left : (value * universe) list;
  mutable right : (value * universe) list;
}

and symbol_meta = {
  (* fixme: ephemeral on cont is wrong. suspended may be only ref. frame doesn't work either because siblings may be waiting ready in the frame *)
  suspended : (cont, universe) EphemeronList.t;
  mutable instanced : universe list;
}

module U = Unifier.Make(struct type b = term list type m = symbol_meta end)

module Universe = struct
  type t = universe
  let equal = (==)
  let hash u = u.hash
end

module UM = WeakMap.Make(Universe)

type job = term * env * cont * universe
type state = job Queue.t

let init t : state =
  let s = Queue.create () in
  let k = Ret in
  let u = {
    frontier = k;
    unifier = U.empty;
    dead = false;
    crossings = [];
    log = UM.empty;
    hash = alloc_hash ();
  } in
  Queue.add (t, [], k, u) s;
  s

let get_symbol_meta = function
  | Sym (_, _, m) -> m
  | _ -> raise (Invalid_argument "not a symbol")

let depth = function
  | Ret -> 0
  | Left f | Right f -> 1 + f.depth

let rec join_crossings c0 c1 =
  match c0, c1 with
  | _ when c0 == c1 -> c0
  | c, [] | [], c -> c
  | ((k0, us0) as kus0) :: c0', ((k1, us1) as kus1) :: c1' ->
      if k0 == k1 then
        (k0, List.rev_append us0 us1) :: (join_crossings c0' c1')
      else
        let d0 = depth k0 in
        let d1 = depth k1 in
        assert (d0 <> d1);
        if d0 > d1 then
          kus0 :: (join_crossings c0' c1)
        else
          kus1 :: (join_crossings c0 c1')

let joint_frontier u0 u1 =
  let crossings_tail u k = function
    | (ck, cus) :: l' when ck == k ->
        if List.for_all (fun cu -> Option.is_some (UM.get cu u.log)) cus then
          Some l'
        else
          None
    | l -> Some l
  in
  let rec find_root k0 k1 c0 c1 =
    match k0, k1, depth k0, depth k1 with
    | (Left f0 | Right f0), _, d0, d1 when d0 > d1 ->
        let* c0' = crossings_tail u1 k0 c0 in
        find_root f0.parent k1 c0' c1
    | _, (Left f1 | Right f1), d0, d1 when d1 > d0 ->
        let* c1' = crossings_tail u0 k1 c1 in
        find_root k0 f1.parent c0 c1'
    | (Left f0 | Right f0), (Left f1 | Right f1), _, _ when f0 != f1 ->
        let* c0' = crossings_tail u1 k0 c0 in
        let* c1' = crossings_tail u0 k1 c1 in
        find_root f0.parent f1.parent c0' c1'
    | Left f, Right _, _, _ | Right f, Left _, _, _ ->
        let* c0' = crossings_tail u1 k0 c0 in
        let* c1' = crossings_tail u0 k1 c1 in
        Some (f.parent, c0', c1')
    | _ -> None
  in
  find_root u0.frontier u1.frontier u0.crossings u1.crossings

let rec joinable u0 u1 =
  Option.is_some (joint_frontier u0 u1)

let rec join u0 u1 =
  match joint_frontier u0 u1 with
  | None -> None
  | Some (_k, c0, c1) ->
      let _c = join_crossings c0 c1 in
      failwith "todo"

let run (s : state) : value list =
  let try_apply_cont op k l r lu ru =
    let rec apply_cont op k l r u =
      match op with
      | Seq ->
          Queue.add (Var 0, [r], k, u) s
      | Unif ->
          begin match U.unify u.unifier l r with
          | Some (g, instanced) ->
              List.iter
                (fun (x, v) ->
                  (* todo: initialize sibling jobs for potential conflicts *)
                  let mx = get_symbol_meta x in
                  (* for each joinable universe with an instance of x: *)
                  (*  create exclusive joint universe and rerun unification in that universe *)
                  (*    if this instances new variables, process the same way but ignore those that were instanced in u *)
                  mx.instanced <- u :: mx.instanced;
                  let handle_suspended = 
                    match v with
                    | Sym (_, _, mv) ->
                        fun e su ->
                          (*  if same universe: move to mv.suspended *)
                          (*  if joinable universe: copy to mv.suspended *)
                          (*  if not joinable: keep *)
                          if joinable u su then
                            EphemeronList.pushe e mv.suspended
                    | _ ->
                        fun e su -> 
                          (*  if same universe: resume and remove from suspended *)
                          (*  if joinable universe: resume in the joint universe and do not remove *)
                          (*  if not joinable: ignore and keep *)
                          match join u su with
                          | None -> ()
                          | Some u' ->
                              let sk = Ephemeron.get_key e in
                              Queue.add (Var 0, [v], sk, u') s
                  in
                  EphemeronList.filter_inplace
                    (fun e ->
                      let su = Ephemeron.get_data e in
                      handle_suspended e su;
                      u != su)
                    mx.suspended)
                instanced;
              u.unifier <- g;
              Queue.add (Const "ok", [], k, u) s
          | None ->
              u.dead <- true
          end
      | App ->
          match l with
          | Sym _ ->
              begin match U.find u.unifier l with
              | Sym (_, _, m) ->
                  EphemeronList.push k u m.suspended
              | l' ->
                  apply_cont op k l' r u
              end
          | Struct _ ->
              let v = cons_struct l r in
              Queue.add (Var 0, [v], k, u) s
          | Clos (_, _, [body], e) ->
              Queue.add (body, r :: e, k, u) s
          | Clos (_, _, bodies, e) ->
              let app body =
                let u' = { u with frontier = k } in
                Queue.add (body, r :: e, k, u') s in
              let count = List.fold_left (fun i body -> app body; i + 1) 0 bodies in
              ignore count
    in
    match join lu ru with
    | None -> ()
    | Some u -> apply_cont op k l r u
  in
  let rec run' res : value list =
    if Queue.is_empty s then
      res
    else
      match Queue.take s with
      | Bin (op, l, r), e, k, u ->
          let f = {
            op;
            parent = k;
            depth = depth k;
            left = [];
            right = [];
          } in
          Queue.add (l, e, Left f, u) s;
          Queue.add (r, e, Right f, u) s;
          run' res
      | Lam (x, t), e, k, u ->
          let v = make_closure x t e in
          Queue.add (Var 0, [v], k, u) s;
          run' res
      | Nu (x, t), e, k, u ->
          let x = make_symbol x { suspended = EphemeronList.create (); instanced = [] } in
          Queue.add (t, x :: e, k, u) s;
          run' res
      | Const c, _, k, u ->
          let v = make_struct c in
          Queue.add (Var 0, [v], k, u) s;
          run' res
      | Var i, e, (Left ({ op; parent; right; _ } as fr) as k), lu ->
          let l = List.nth e i in
          List.iter (fun (r, ru) -> try_apply_cont op parent l r lu ru) right;
          fr.left <- (l, lu) :: fr.left;
          if lu.frontier == k then lu.frontier <- parent;
          run' res
      | Var i, e, (Right ({ op; parent; left; _ } as fr) as k), ru ->
          let r = List.nth e i in
          List.iter (fun (l, lu) -> try_apply_cont op parent l r lu ru) left;
          fr.right <- (r, ru) :: fr.right;
          if ru.frontier == k then ru.frontier <- parent;
          run' res
      | Var i, e, Ret, { unifier = u; _ } ->
          let v = List.nth e i in
          run' (U.subst u v :: res)
  in
  run' []

let rec decode_value v =
  match v with
  | Sym _ -> failwith "open term"
  | Struct (_, c, vs) -> List.fold_right (fun r l -> Bin (App, l, decode_value r)) vs (Const c)
  | Clos (_, x, t, e) -> Lam (x, List.map (decode_term ~free:1 e) t)

and decode_term ?(free = 0) e t =
  match t with
  | Const c -> Const c
  | Var i -> if i >= free then decode_value (List.nth e (i - free)) else Var i
  | Bin (op, t, u) -> Bin (op, decode_term e t ~free, decode_term e u ~free)
  | Lam (x, t) -> Lam (x, List.map (decode_term e ~free:(free + 1)) t)
  | Nu (x, t) -> Nu (x, decode_term e t ~free:(free + 1))

let rec print_term ?(vars : string list = []) = function
  | Var i -> print_string (List.nth vars i)
  | Bin (App, Lam (x, t), Bin (App, u, s)) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_string "(";
      print_term (Bin (App, u, s)) ~vars;
      print_string ")"
  | Bin (App, Lam (x, t), u) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_term u ~vars
  | Bin (App, t, Bin (App, u, s)) ->
      print_term t ~vars;
      print_string "(";
      print_term (Bin (App, u, s)) ~vars;
      print_string ")"
  | Bin (App, t, u) ->
      print_term t ~vars;
      print_term u ~vars
  | Bin (Seq, t, u) ->
      print_term t ~vars;
      print_string ";";
      print_term u ~vars
  | Lam (x, t) ->
      Printf.printf "λ%s." x;
      List.iteri
        (fun i t0 ->
          if i > 0 then print_string "|";
          print_term t0 ~vars:(x :: vars))
        t
  | _ -> failwith "todo"

let () = Printexc.record_backtrace true
let id = Lam ("x", [ Var 0 ])
let const = Lam ("x", [ Lam ("y", [ Var 1 ]) ])
let dup = Lam ("x", [ Var 0; Var 0 ])
(* let small_omega = Lam ("x", App (Var 0, Var 0)) *)
(* let big_omega = App (small_omega, small_omega) *)

let () =
  let id2 = Bin (App, dup, id) in
  let prog = Bin (App, dup, id2) in
  print_term prog;
  print_newline ();
  print_newline ();
  let vs = run (init prog) in
  List.iter
    (fun v ->
      print_term (decode_value v);
      print_newline ())
    vs

(* let () = *)
(*   print_term big_omega; *)
(*   print_newline (); *)
(*   let s = init big_omega in *)
(*   Queue.add (id, [], Ret) s; *)
(*   let c = run s in *)
(*   print_term (decode c) *)
