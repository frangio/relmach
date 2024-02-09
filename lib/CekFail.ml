type term = Var of int | App of term * term | Lam of string * term list | Fail

let bodies = function Lam (_, ts) -> ts | _ -> failwith "not lambda value"

type clos = Clos of term * env
and env = clos list

type cont = Ret | Left of frame | Right of frame

and frame = {
  mutable left : clos list;
  mutable right : clos list;
  parent : cont;
}

let rec decode (Clos (t, e)) : term =
  let rec decode' t e offset =
    match t with
    | Fail -> Fail
    | Var i -> if i >= offset then decode (List.nth e (i - offset)) else Var i
    | App (t, u) -> App (decode' t e offset, decode' u e offset)
    | Lam (x, t) -> Lam (x, List.map (fun t0 -> decode' t0 e (offset + 1)) t)
  in
  decode' t e 0

let rec print_term ?(vars : string list = []) = function
  | Fail -> print_string "fail"
  | Var i -> print_string (List.nth vars i)
  | App (Lam (x, t), App (u, s)) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_string "(";
      print_term (App (u, s)) ~vars;
      print_string ")"
  | App (Lam (x, t), u) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_term u ~vars
  | App (t, App (u, s)) ->
      print_term t ~vars;
      print_string "(";
      print_term (App (u, s)) ~vars;
      print_string ")"
  | App (t, u) ->
      print_term t ~vars;
      print_term u ~vars
  | Lam (x, t) ->
      Printf.printf "λ%s." x;
      List.iteri
        (fun i t0 ->
          if i > 0 then print_string "|";
          print_term t0 ~vars:(x :: vars))
        t

type job = term * env * cont
type branch = job Queue.t
type state = branch Queue.t

let run (bs : state) : clos list =
  let add_app f fe a ae parent =
    (* TODO: create branch only if >1 bodies *)
    let b = Queue.create () in
    Queue.add b bs;
    Queue.add (f, Clos (a, ae) :: fe, parent) b
  in
  let add_apps f fe a ae parent =
    List.iter (fun f0 -> add_app f0 fe a ae parent) (bodies f)
  in
  let rec run' res : clos list =
    if Queue.is_empty bs then res
    else
      let b = Queue.take bs in
      let res =
        match Queue.take b with
        | Fail, _, _ ->
            (* TODO: kill orphan frames ? *)
            Queue.clear b;
            res
        | App (t, u), e, k ->
            let f = { left = []; right = []; parent = k } in
            Queue.add (t, e, Left f) b;
            Queue.add (u, e, Right f) b;
            res
        | Var idx, e, k ->
            let (Clos (t, e')) = List.nth e idx in
            Queue.add (t, e', k) b;
            res
        | (Lam _ as t), e, Left ({ right; parent; _ } as f) ->
            List.iter (fun (Clos (u, e')) -> add_apps t e u e' parent) right;
            f.left <- Clos (t, e) :: f.left;
            res
        | (Lam _ as u), e', Right ({ left; parent; _ } as f) ->
            List.iter (fun (Clos (t, e)) -> add_apps t e u e' parent) left;
            f.right <- Clos (u, e') :: f.right;
            res
        | (Lam _ as t), e, Ret -> Clos (t, e) :: res
      in
      if not (Queue.is_empty b) then Queue.add b bs;
      run' res
  in
  run' []

let init t : state =
  let bs = Queue.create () in
  let b = Queue.create () in
  Queue.add b bs;
  Queue.add (t, [], Ret) b;
  bs

let () = Printexc.record_backtrace true
let fail = Fail
let id = Lam ("x", [ Var 0 ])
let const = Lam ("x", [ Lam ("y", [ Var 1 ]) ])
let dup = Lam ("x", [ Var 0; Var 0 ])
let dup' = Lam ("x", [ Var 0; Var 0; fail ])
(* let small_omega = Lam ("x", App (Var 0, Var 0)) *)
(* let big_omega = App (small_omega, small_omega) *)

(* let () =
  let prog = App (dup, App (dup, id)) in
  print_term prog;
  print_newline ();
  print_newline ();
  let cs = run (init prog) in
  List.iter
    (fun c ->
      print_term (decode c);
      print_newline ())
    cs *)

(* let () = *)
(*   print_term big_omega; *)
(*   print_newline (); *)
(*   let s = init big_omega in *)
(*   Queue.add (id, [], Ret) s; *)
(*   let c = run s in *)
(*   print_term (decode c) *)
