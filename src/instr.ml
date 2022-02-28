open Cil
open Common

let mk_instrumenting_functions ast =
  let main_fd = find_fun ast (fun fd -> fd.svar.vname = main_name) in
  let local_vars = main_fd.slocals in
  let tmp_vars, main_param_vars = List.partition (fun v -> is_cil_tmp v.vname) local_vars in
  let mainQ_type = mk_fun_typ voidType (List.map (fun v -> (v.vname, v.vtype)) main_param_vars) in
  let mainQ_args = main_param_vars in

  (** Create mainQ function *)
  let mainQ_fd = mk_fundec mainQ_prefix mainQ_type in
  let mainQ_call_stmt = mkStmtOneInstr (mk_Call ~ftype:mainQ_type mainQ_prefix (List.map vi2e mainQ_args)) in
  mainQ_fd.slocals <- mainQ_fd.slocals @ tmp_vars;
  mainQ_fd.sbody <- visitCilBlock (new create_void_return_visitor) main_fd.sbody;
  let main_return_stmt =
    let rt, _, _, _ = splitFunctionType main_fd.svar.vtype in
    match rt with
    | TVoid _ -> mkStmt (Return(None, !currentLoc))
    | _ -> mkStmt (Return(Some zero, !currentLoc))
  in
  main_fd.sbody <- mkBlock [mainQ_call_stmt; main_return_stmt];
  let mainQ_global = GFun(mainQ_fd, !currentLoc) in
  ast.globals <- add_global_before_func main_name mainQ_global ast.globals;
  write_stdout ast

let () = 
  begin
    initCIL();
    Cil.lineDirectiveStyle := None; (* reduce code, remove all junk stuff *)
    Cprint.printLn := false; (* do not print line *)
    (* for Cil to retain &&, ||, ?: instead of transforming them to If stmts *)
    Cil.useLogicalOperators := true;

    let src = Sys.argv.(1) in
    let ast = Frontc.parse src () in
    mk_instrumenting_functions ast;
  end