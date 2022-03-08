open Globals

let main () =
  let _ = Arguments.parse_arguments () in
  if !Globals.enable_dig_instr then
    let _ = print_endline "Instrumentation for dynamic analysis with DIG" in
    Tinstr.vtrace_instr !Globals.input_file
  else if !Globals.enable_validate_instr then
    let _ = print_endline "Instrumentation for static validation with Ultimate" in
    Vinstr.validate_instr !Globals.input_file !Globals.input_csv_file
  else
    failwith "No option provided!"

let _ =
  let () = Printexc.record_backtrace true in
  try main () with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    Printf.eprintf "There was an error: %s\n%s\n" msg stack;
    exit 1
;;
  