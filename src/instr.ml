open Cil
open Common

let rec is_var_exp (e: exp) =
  match e with
  | Lval (Var _, _) -> true
  | UnOp (_, e1, _) -> is_var_exp e1
  | BinOp (_, e1, e2, _) -> is_var_exp e1 || is_var_exp e2
  | CastE (_, e1) -> is_var_exp e1
  | _ -> false

let rec is_nla_exp (e: exp) =
  match e with
  | BinOp (bop, e1, e2, _) -> 
    if is_nla_exp e1 || is_nla_exp e2 then true
    else 
      (match bop with
      | Mult | Div | Mod | Shiftlt | Shiftrt -> is_var_exp e1 && is_var_exp e2
      | _ -> false)
  | UnOp (_, e1, _) -> is_nla_exp e1
  | CastE (_, e1) -> is_nla_exp e1
  | _ -> false

let vtrace_if_label = "if"
let vtrace_else_label = "else"

class add_vtrace_for_complex_exp_visitor ast fd = object(self)
  inherit nopCilVisitor

  method private mk_vtrace label params loc =
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
        if is_nla_exp if_cond then
          let fd_vars = fd.sformals @ fd.slocals in
          let params, _ = List.partition (fun vi -> not (is_cil_tmp vi.vname)) fd_vars in
          let vtrace_if_call = self#mk_vtrace vtrace_if_label params loc in
          let vtrace_else_call = self#mk_vtrace vtrace_else_label params loc in
          let () = if_block.bstmts <- [mkStmtOneInstr vtrace_if_call] @ if_block.bstmts in
          let () = else_block.bstmts <- [mkStmtOneInstr vtrace_else_call] @ else_block.bstmts in
          s
        else s
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)
end

let add_vtrace_for_complex_exp ast fd _ =
  let visitor = new add_vtrace_for_complex_exp_visitor ast fd in
  ignore (visitCilFunction (visitor :> nopCilVisitor) fd)

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
  ast.globals <- add_global_before_func main_name mainQ_global ast.globals

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
    iterGlobals ast (only_functions (add_vtrace_for_complex_exp ast));
    write_stdout ast
  end