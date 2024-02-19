open Types

let print_term =
  let print_op = function
    | App -> print_string " "
    | Unif -> print_string " ~ "
    | Seq -> print_string "; "
  in

  let rec print_binders vars = function
    | Nu (x, (Nu _ as t)) | Lam (x, [Lam _ as t]) ->
        Printf.printf "%s " x;
        print_binders (x :: vars) t
    | Nu (x, t) ->
        Printf.printf "%s in " x;
        (x :: vars), [t]
    | Lam (x, t) ->
        Printf.printf "%s -> " x;
        (x :: vars), t
    | _ -> invalid_arg "not a binder"
  in

  let with_parens print x =
    print_string "(";
    print x;
    print_string ")"
  in

  let rec print_term ?(sub = true) vars = function
    | Var i ->
        print_string (List.nth vars i)
    | Const c ->
        Printf.printf "%s" c
    | Nu _ as t ->
        if sub then print_string "(";
        print_string "fresh ";
        let vars, u = print_binders vars t in
        print_term ~sub:false vars (List.hd u);
        if sub then print_string ")"
    | Lam _ as t ->
        print_string "fun ";
        let vars, u = print_binders vars t in
        List.iteri
          (fun i u0 ->
            if i > 0 then print_string " | ";
            print_term ~sub:false vars u0)
          u;
    | Bin (App, Lam (x, [u]), t) when not sub ->
        Printf.printf "let %s = " x;
        print_term ~sub:false vars t;
        print_string " in ";
        print_term ~sub:false (x :: vars) u
    | Bin (op, t, u) ->
        begin match t with
        | Lam _ -> with_parens (print_term ~sub:false vars) t
        | _ -> print_term vars t
        end;
        print_op op;
        begin match u with
        | Lam _ | Bin (_, _, _) -> with_parens (print_term ~sub:false vars) u
        | _ -> print_term vars u
        end
  in
  print_term ~sub:false []
