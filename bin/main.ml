open Relmach

let () = Printexc.record_backtrace true

let read_file file = In_channel.with_open_text file In_channel.input_all

let () =
  let source = read_file "test.sml" in
  match Parse.parse source with
  | Ok p ->
      Pp.print_term p;
      print_newline ();
      let rs = Machine.run (Machine.init p) in
      Seq.iter
        (fun r ->
          Pp.print_term (Result.decode r);
          print_newline ())
        rs
  | Error e -> Printf.printf "Error: %s\n%!" e
