open Relmach
open Relmach.Types
open Relmach.Machine
open Relmach.Pp

let () = Printexc.record_backtrace true

let prog =
  Nu ("x",
    Bin (Seq,
      Bin (Unif, Var 0, Const "c"),
      Bin (App,
        Lam ("y", [
          Bin (App, Var 0, Const "d");
          Bin (App, Var 0, Const "e")
        ]),
        Var 0)))

let () =
  print_term prog;
  print_newline ();
  print_newline ();
  let rs = run (init prog) in
  Seq.iter
    (fun r ->
      print_term (Result.decode r);
      print_newline ())
    rs
