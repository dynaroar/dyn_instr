open Cil
open Common

let csv_sep = ';'

class add_inv_for_complex_exp_visitor ast inv_tbl fd = object(self)
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

  method vstmt (s: stmt) =
    let action s =
      match s.skind with
      | If (if_cond, if_block, else_block, loc) ->
        if is_complex_exp if_cond then
          let vtrace_if_name = mk_vtrace_name loc vtrace_if_label in
          let vtrace_else_name = mk_vtrace_name loc vtrace_else_label in
          let if_appx = H.find inv_tbl vtrace_if_name in
          let else_appx = H.find inv_tbl vtrace_else_name in
          print_endline if_appx;
          print_endline else_appx;
          (* let vtrace_if_call = self#mk_vtrace vtrace_if_label loc in
          let vtrace_else_call = self#mk_vtrace vtrace_else_label loc in
          let () = if_block.bstmts <- [mkStmtOneInstr vtrace_if_call] @ if_block.bstmts in
          let () = else_block.bstmts <- [mkStmtOneInstr vtrace_else_call] @ else_block.bstmts in *)
          s
        else s
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)

end

let add_inv_for_complex_exp ast inv_tbl fd _ =
  let visitor = new add_inv_for_complex_exp_visitor ast inv_tbl fd in
  ignore (visitCilFunction (visitor :> nopCilVisitor) fd)

let () = 
  begin
    initCIL();
    Cil.lineDirectiveStyle := None; (* reduce code, remove all junk stuff *)
    Cprint.printLn := false; (* do not print line *)
    (* for Cil to retain &&, ||, ?: instead of transforming them to If stmts *)
    Cil.useLogicalOperators := true;

    let src = Sys.argv.(1) in
    let csv = Sys.argv.(2) in
    let fn = Filename.remove_extension src in
    let ext = Filename.extension src in
    
    let inv_tbl = H.create 10 in
    let () = L.iter (fun str_lst ->
      match str_lst with
      | lbl::inv::[] -> H.add inv_tbl lbl inv
      | _ -> E.s (E.error "Invalid csv row: %s" (S.concat "; " str_lst))
      ) (Csv.load ~separator:csv_sep ~strip:true csv) 
    in
    
    let ast = Frontc.parse src () in
    iterGlobals ast (only_functions (add_inv_for_complex_exp ast inv_tbl));
    (* write_stdout ast *)
    write_src (fn ^ "_validate" ^ ext) ast
  end