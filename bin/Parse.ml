open Angstrom

module Term = Relmach.Types

let ( let* ) = ( >>= )
let ( let+ ) = ( >>| )
let ( and+ ) = both

let is_ws = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let is_alpha = function
  | 'A'..'Z' | 'a'..'z' -> true
  | _ -> false

let is_lower = function
  | 'a'..'z' -> true
  | _ -> false

let is_upper = function
  | 'A'..'Z' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alphanum c = is_digit c || is_alpha c

let ws = skip_while is_ws

type token = Keyword of string | Ident of string | Const of string | Symbol of string

let keywords = ["let"; "fun"; "fresh"]

let token_keyword =
  let+ kw = choice (List.map string keywords) in
  Keyword kw

let token_ident =
  let* a = take_while1 is_lower
  and+ b = take_while is_alphanum
  in
  let s = a ^ b in
  if List.mem s keywords then
    fail "invalid identifier"
  else
    return (Ident s)

let token_const_ident =
  let+ a = take_while1 is_upper
  and+ b = take_while is_alphanum
  in Const (a ^ b)

let token_const_num =
  let* n = take_while1 is_digit
  and+ p = peek_char
  in match p with
  | Some c when is_alphanum c -> fail "invalid number"
  | _ -> return (Const n)

let token_symbol =
  let+ s = choice (List.map string ["="; "->"; "("; ")"; "|"; "~"; ";"]) in
  Symbol s

let token =
  ws *> choice [
    token_ident;
    token_const_ident;
    token_const_num;
    token_symbol;
    token_keyword;
  ]

let keyword kw =
  let* t = token in
  match t with
  | Keyword kw' when kw = kw' -> return kw
  | _ -> fail (Printf.sprintf "expected keyword %s" kw)

let ident =
  let* t = token in
  match t with
  | Ident id -> return id
  | _ -> fail "expected identifier"

let const =
  let* t = token in
  match t with
  | Const c -> return c
  | _ -> fail "expected constant"

let symbol s =
  let* t = token in
  match t with
  | Symbol s' when s = s' -> return s
  |  _ -> fail (Printf.sprintf "expected symbol %s" s)

let expr = fix @@ fun expr ->
  let expr_paren = symbol "(" *> expr <* symbol ")"

  and expr_var =
    let+ x = ident in
    fun env -> Term.Var (List.find_index ((=) x) env |> Option.get)

  and expr_const =
    let+ c = const in
    fun _ -> Term.Const c

  in

  let expr_simple =
    choice [
      expr_paren;
      expr_var;
      expr_const;
    ]
  in

  let expr_app =
    let+ ts = many1 expr_simple in
    let t, ts = List.(hd ts, tl ts) in
    fun env ->
      List.fold_left
        (fun t u -> Term.Bin (App, t, u env))
        (t env)
        ts
  in

  let expr_unif =
    let+ t = expr_app
    and+ _ = symbol "~"
    and+ u = expr_app
    in fun env -> Term.Bin (Unif, t env, u env)
  in

  let expr_seq =
    let+ ts = sep_by1 (symbol ";") (expr_unif <|> expr_app) in
    let t, ts = List.(hd ts, tl ts) in
    fun env ->
      List.fold_left
        (fun t u -> Term.Bin (Seq, t, u env))
        (t env)
        ts

  and expr_fun =
    let+ _ = keyword "fun"
    and+ xs = many1 ident
    and+ _ = symbol "->"
    and+ ts = option "" (symbol "|") *> sep_by1 (symbol "|") expr
    in fun env ->
      let ts = List.map (fun t -> t (List.rev_append xs env)) ts in
      List.hd @@ List.fold_right
        (fun x t -> [Term.Lam (x, t)])
        xs
        ts

  and expr_fresh =
    let+ _ = keyword "fresh"
    and+ xs = many1 ident
    and+ _ = symbol "->"
    and+ t = expr
    in List.fold_right
      (fun x t env -> Term.Nu (x, t (x :: env)))
      xs
      t

  in

  choice [
    expr_fun;
    expr_fresh;
    expr_seq;
  ]

let let_stmt =
  let+ _ = keyword "let"
  and+ x = ident
  and+ _ = symbol "="
  and+ t = expr
  in
  (x, t)

let program =
  let+ s = many1 let_stmt
  and+ _ = ws
  in
  List.fold_right
    (fun (x, t) p env ->
      Term.Bin (
        App,
        Term.Lam (x, [p (x :: env)]),
        t env
      ))
    s
    (fun _ -> Term.Var 0)
    []


let parse s = parse_string ~consume:Consume.All program s
