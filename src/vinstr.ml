open Cil
open Common
open Lexing

let csv_sep = ';'

let print_error_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_exp_with_error lexbuf =
  try Iparser.inv Ilexer.read lexbuf 
  with
  | Ilexer.SyntaxError msg as e ->
    Printf.fprintf stderr "%a: %s\n" print_error_position lexbuf msg;
    raise e
  | Iparser.Error as e ->
    Printf.fprintf stderr "%a: syntax error\n" print_error_position lexbuf;
    raise e

class add_inv_for_complex_exp_visitor ast inv_tbl opt_case fd = object(self)
  inherit nopCilVisitor

  method private mk_vtrace label loc =
    let fd_vars = fd.sformals @ fd.slocals in
    let params, _ = List.partition (fun vi -> not (is_cil_tmp vi.vname)) fd_vars in
    let vtrace_param_types = L.map (fun vi -> (vi.vname, vi.vtype)) params in
    let vtrace_fun_typ = mk_fun_typ voidType vtrace_param_types in
    let vtrace_name = mk_vtrace_name loc label in
    let vtrace_fd = mk_fundec vtrace_name vtrace_fun_typ in
    let vtrace_global = GFun (vtrace_fd, loc) in
    let () = ast.globals <- [vtrace_global] @ ast.globals in
    let vtrace_args = List.map (fun v -> vi2e v) vtrace_fd.sformals in
    let vtrace_call = mk_Call vtrace_name vtrace_args in
    vtrace_call

  method private find_and_parse_inv ?(if_inv=true) loc label =
    let missing_appx = if if_inv then Cil.zero else Cil.one in
    let vtrace_name = mk_vtrace_name loc label in
    match H.find_opt inv_tbl vtrace_name with
    | None -> missing_appx
    | Some appx ->
      (try parse_exp_with_error (Lexing.from_string appx) with
      | _ -> missing_appx)

  method private mk_case_var ?(too_big=true) loc label =
    let case_label =
      if too_big then "too_big"
      else "too_small"
    in
    let vname = mk_var_name loc (label ^ "_" ^ case_label) in
    let vi = makeLocalVar fd vname intType in
    vi

  method private mk_error_block_with_label ?(too_big=true) loc label =
    let label_vi = self#mk_case_var loc label ~too_big:too_big in
    let mk_block vi = 
      let label_assignment = mkStmtOneInstr (Set (var label_vi, one, loc)) in
      let err_block = mk_error_block () in
      err_block.bstmts <- label_assignment::err_block.bstmts;
      err_block
    in
    let mk_assume_false_block _ =
      let assume_false_stmt = mk_assume (Cil.BinOp (Eq, zero, one, intType)) in
      let empty_block = mk_empty_block () in
      empty_block.bstmts <- assume_false_stmt::empty_block.bstmts;
      empty_block
    in
    match opt_case with
    | None -> mk_block label_vi
    | Some lbl ->
      try
        let case_loc = int_of_string (String.split_on_char '_' lbl |> List.rev |> List.hd) in
        let _ = print_endline (string_of_int case_loc) in
        if case_loc != loc.line || String.compare label_vi.vname lbl != 0 then 
          (print_endline (label_vi.vname);
          print_endline (lbl);
          mk_assume_false_block ())
        else mk_block label_vi
      with _ -> 
        Printf.fprintf stderr "Cannot get location from label %s. Ignore it.\n" lbl;
        mk_block label_vi

  method vstmt (s: stmt) =
    let action s =
      match s.skind with
      | If (if_cond, if_block, else_block, loc) ->
        if is_complex_exp if_cond then
          let if_appx_exp = self#find_and_parse_inv loc if_label in
          let else_appx_exp = self#find_and_parse_inv ~if_inv:false loc else_label in
          (* Errormsg.log "if_appx_exp: %a\n" d_exp if_appx_exp;
          Errormsg.log "else_appx_exp: %a\n" d_exp else_appx_exp; *)
          let if_instr_stmt =
            let else_error_stmt = mkStmt (If (neg if_appx_exp, self#mk_error_block_with_label loc if_label ~too_big:false, mk_empty_block (), loc)) in
            mkStmt (If (else_appx_exp, self#mk_error_block_with_label loc else_label ~too_big:true, mkBlock [else_error_stmt], loc)) in
          (* Errormsg.log "if_instr_stmt: %a\n" d_stmt if_instr_stmt; *)
          let else_instr_stmt =
            let else_error_stmt = mkStmt (If (neg else_appx_exp, self#mk_error_block_with_label loc else_label ~too_big:false, mk_empty_block (), loc)) in
            mkStmt (If (if_appx_exp, self#mk_error_block_with_label loc if_label ~too_big:true, mkBlock [else_error_stmt], loc)) in
          (* Errormsg.log "else_instr_stmt: %a\n" d_stmt else_instr_stmt; *)
          if_block.bstmts <- [if_instr_stmt] @ if_block.bstmts;
          else_block.bstmts <- [else_instr_stmt] @ else_block.bstmts;
          s
        else s
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)

end

let add_inv_for_complex_exp ast ?(opt_inv_tbl=None) opt_pre opt_case fd _ =
  let () =
    match opt_pre with
    | None -> ()
    | Some pre ->
      if not (is_main (fname_of_fundec fd)) then ()
      else
        let assume_stmt = mk_assume pre in
        fd.sbody.bstmts <- assume_stmt::fd.sbody.bstmts
  in
  match opt_inv_tbl with
  | None -> ()
  | Some tbl ->
    let visitor = new add_inv_for_complex_exp_visitor ast tbl opt_case fd in
    ignore (visitCilFunction (visitor :> nopCilVisitor) fd)

let validate_instr src csv pre case = 
  begin
    initCIL();
    Cil.lineDirectiveStyle := None; (* reduce code, remove all junk stuff *)
    Cprint.printLn := false; (* do not print line *)
    (* for Cil to retain &&, ||, ?: instead of transforming them to If stmts *)
    Cil.useLogicalOperators := true;

    let fn = Filename.remove_extension src in
    let ext = Filename.extension src in

    let ast = Frontc.parse src () in

    let opt_pre =
      if pre = "" then None
      else
        try Some (parse_exp_with_error (Lexing.from_string pre))
        with _ ->
          Printf.fprintf stderr "Cannot parse the given precondition. Ignore it.\n"; 
          None
    in
    
    let opt_inv_tbl = 
      if csv = "" then None
      else
        let tbl = H.create 10 in
      let () = L.iter (fun str_lst ->
        match str_lst with
        | lbl::inv::[] -> H.add tbl lbl inv
        | _ -> E.s (E.error "Invalid csv row: %s" (S.concat "; " str_lst))
        ) (Csv.load ~separator:csv_sep ~strip:true csv) 
      in
      Some tbl
    in

    let opt_case = 
      if case = "" then None
      else Some case
    in
    
    iterGlobals ast (only_functions (add_inv_for_complex_exp ast ~opt_inv_tbl:opt_inv_tbl opt_pre opt_case));
    (* write_stdout ast *)
    write_src (fn ^ "_validate" ^ ext) ast
  end