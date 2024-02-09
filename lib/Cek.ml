type term = Var of int | App of term * term | Lam of string * term

type clos = Clos of term * env
and env = clos list

type stack = Arg of term * env * stack | Fun of term * env * stack | Ret
type state = term * env * stack

let rec cek : state -> clos = function
  | App (t, u), e, s -> cek (t, e, Arg (u, e, s))
  | Lam (_, t), e, Arg (u, e', s) -> cek (u, e', Fun (t, e, s))
  | Lam (x, t), e, Fun (u, e', s) -> cek (u, Clos (Lam (x, t), e) :: e', s)
  | Var i, e, s ->
      let (Clos (t, e')) = List.nth e i in
      cek (t, e', s)
  | t, e, Ret -> Clos (t, e)

let init t : state = (t, [], Ret)

let rec decode (Clos (t, e)) : term =
  let rec decode' t e offset =
    match t with
    | Var i -> if i >= offset then decode (List.nth e (i - offset)) else Var i
    | App (t, u) -> App (decode' t e offset, decode' u e offset)
    | Lam (x, t) -> Lam (x, decode' t e (offset + 1))
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
      print_term t ~vars:(x :: vars)

let () = Printexc.record_backtrace true

(* let () =
  let id = Lam ("x", Var 0) in
  print_term id;
  print_newline ();
  let prog = App (id, id) in
  print_term prog;
  print_newline ();
  let c = cek (init prog) in
  print_term (decode c) *)
