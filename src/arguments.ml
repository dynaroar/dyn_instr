open Globals

let arguments_raw =
  [
    ( [ "-dig"; "--dig" ],
      "Enable instrumentation for dynamic analysis with DIG",
      Arg.Set enable_dig_instr );
    ( [ "-nopre"; "--nopre" ],
      "Disable instrumentation for precondition analysis",
      Arg.Clear enable_pre_instr );
    ( [ "-nonla"; "--no-print-nla" ],
      "Disable NLA printing",
      Arg.Clear enable_print_nla );
    ( [ "-val"; "--validate" ],
      "Enable instrumentation for static validation with Ultimate",
      Arg.Set enable_validate_instr );
    ( [ "-inv"; "--invariants" ],
      "Provide potential invariants to be validated in the CSV format",
      Arg.Set_string input_csv_file )
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
