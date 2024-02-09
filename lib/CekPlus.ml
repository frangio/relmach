type term = Var of int | App of term * term | Lam of string * term list

let bodies = function Lam (_, ts) -> ts | _ -> failwith "not lambda value"

type clos = Clos of term * env
and env = clos list

type cont = Ret | Left of frame | Right of frame

and frame = {
  mutable left : clos list;
  mutable right : clos list;
  parent : cont;
}

type job = term * env * cont
type state = job Queue.t

let run (s : state) : clos list =
  let add_apps f fe a ae parent =
    List.iter
      (fun t0 -> Queue.add (t0, Clos (a, ae) :: fe, parent) s)
      (bodies f)
  in
  let rec run' res : clos list =
    if Queue.is_empty s then res
    else
      match Queue.take s with
      | App (t, u), e, k ->
          let f = { left = []; right = []; parent = k } in
          Queue.add (t, e, Left f) s;
          Queue.add (u, e, Right f) s;
          run' res
      | Var idx, e, k ->
          let (Clos (t, e')) = List.nth e idx in
          Queue.add (t, e', k) s;
          run' res
      | t, e, Left ({ right; parent; _ } as f) ->
          List.iter (fun (Clos (u, e')) -> add_apps t e u e' parent) right;
          f.left <- Clos (t, e) :: f.left;
          run' res
      | u, e', Right ({ left; parent; _ } as f) ->
          List.iter (fun (Clos (t, e)) -> add_apps t e u e' parent) left;
          f.right <- Clos (u, e') :: f.right;
          run' res
      | t, e, Ret -> run' (Clos (t, e) :: res)
  in
  run' []

let init t : state =
  let s = Queue.create () in
  Queue.add (t, [], Ret) s;
  s

let rec decode (Clos (t, e)) : term =
  let rec decode' t e offset =
    match t with
    | Var i -> if i >= offset then decode (List.nth e (i - offset)) else Var i
    | App (t, u) -> App (decode' t e offset, decode' u e offset)
    | Lam (x, t) -> Lam (x, List.map (fun t0 -> decode' t0 e (offset + 1)) t)
  in
  decode' t e 0

let rec print_term ?(vars : string list = []) = function
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

let () = Printexc.record_backtrace true
let id = Lam ("x", [ Var 0 ])
let const = Lam ("x", [ Lam ("y", [ Var 1 ]) ])
let dup = Lam ("x", [ Var 0; Var 0 ])
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
