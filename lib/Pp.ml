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
        Printf.printf "%s. " x;
        (x :: vars), [t]
    | Lam (x, t) ->
        Printf.printf "%s. " x;
        (x :: vars), t
    | _ -> invalid_arg "not a binder"
  in

  let rec print_term vars = function
    | Var i ->
        print_string (List.nth vars i)
    | Const c ->
        Printf.printf "'%s'" c
    | Nu _ as t ->
        print_string "ν";
        let vars, u = print_binders vars t in
        print_term vars (List.hd u)
    | Lam _ as t ->
        print_string "λ";
        let vars, u = print_binders vars t in
        List.iteri
          (fun i u0 ->
            if i > 0 then print_string " | ";
            print_term vars u0)
          u;
    | Bin (App, Lam (x, t), Bin (App, u, s)) ->
        print_string "(";
        print_term vars (Lam (x, t));
        print_string ")";
        print_string "(";
        print_term vars (Bin (App, u, s));
        print_string ")"
    | Bin (op, (Lam _ | Nu _ as t), (Bin _ as u)) ->
        print_string "(";
        print_term vars t;
        print_string ")";
        print_op op;
        print_string "(";
        print_term vars u;
        print_string ")"
    | Bin (op, (Lam _ | Nu _ as t), u) ->
        print_string "(";
        print_term vars t;
        print_string ")";
        print_op op;
        print_term vars u
    | Bin (App, t, (Bin _ as u)) ->
        print_term vars t;
        print_op App;
        print_string "(";
        print_term vars u;
        print_string ")"
    | Bin (op, t, u) ->
        print_term vars t;
        print_op op;
        print_term vars u
  in
  print_term []
