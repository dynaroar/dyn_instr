open Globals

let rec print_usage () =
  let arguments = arguments_raw in
  let welcome_msg =
    let prog_exe = Sys.argv.(0) in
    Printf.sprintf "Usage: %s [options] <input file>\n\n" prog_exe in
  let args_msg =
    List.fold_left
      (fun acc (flags, doc, _) ->
        let opt = Printf.sprintf "  %-20s" (String.concat ", " flags) in
        let opt =
          if String.length opt > 22
          then opt ^ Printf.sprintf "\n  %-20s" ""
          else opt in
        let opt = opt ^ Printf.sprintf "  %s\n" doc in
        acc ^ opt)
      "" arguments in
  let usage_msg = "\n" ^ welcome_msg ^ args_msg in
  let _ = print_endline usage_msg in
  exit 0

and arguments_raw =
  [
    ( [ "-dig"; "--dig" ],
      "Enable instrumentation for dynamic analysis with DIG",
      Arg.Set enable_dig_instr );
    ( [ "-nopre"; "--nopre" ],
      "Disable instrumentation for precondition analysis",
      Arg.Clear enable_pre_instr );
    ( [ "-all"; "--all" ],
      "Dynamic instrumentation without restrictions",
      Arg.Set enable_instr_all );
    ( [ "-nonla"; "--no-print-nla" ],
      "Disable NLA printing",
      Arg.Clear enable_print_nla );
    ( [ "-val"; "--validate" ],
      "Enable instrumentation for static validation with Ultimate",
      Arg.Set enable_validate_instr );
    ( [ "-inv"; "--invariants" ],
      "Provide potential invariants to be validated in the CSV format",
      Arg.Set_string input_csv_inv_file );
    ( [ "-pre"; "--pre" ],
      "Add a precondition to the validation",
      Arg.Set_string input_precond );
    ( [ "-case"; "--case" ],
      "Indicate the only case to keep in the validation",
      Arg.Set_string input_case_label );
    ( [ "-lia"; "--lia" ],
      "Provide LIA conditions in the CSV format to plug into the program",
      Arg.Set_string input_csv_lia_file );
    [ "-h"; "-help"; "--help" ], "Print all options", Arg.Unit print_usage
  ]

let parse_arguments () : unit =
  try
    let all_input_files = ref [] in
    let collect_input_file arg = all_input_files := arg :: !all_input_files in
    let arguments =
      List.fold_left
        (fun acc (flags, doc, spec) ->
          let args = List.map (fun flag -> flag, spec, doc) flags in
          acc @ args)
        [] arguments_raw in
    let _ = Arg.parse_argv Sys.argv arguments collect_input_file "" in
    match !all_input_files with
    | [] -> failwith "Input file is undefined!"
    | [ file ] -> input_file := file
    | _ -> failwith "Too many input files. Only one is allowed!"
  with
  | Arg.Bad _ -> failwith ("unknown option: " ^ Sys.argv.(!Arg.current))
  | Arg.Help msg -> raise Exit
;;
