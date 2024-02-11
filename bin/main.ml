open Relmach

let read_file file = In_channel.with_open_text file In_channel.input_all

let file_path = try Sys.argv.(1) with _ -> failwith "missing input file"

let () = Printexc.record_backtrace true

let () =
  let source = read_file file_path in
  match Parse.parse source with
  | Ok p ->
      let rs = Machine.run (Machine.init p) in
      Seq.iter
        (fun r ->
          Pp.print_term (Result.decode r);
          print_newline ())
        rs
  | Error e -> Printf.printf "Error: %s\n%!" e
