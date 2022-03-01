open Cil
open Common

let extern_empty_body_funcs = ["reach_error"]

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

let is_complex_exp (e: exp) =
  is_nla_exp e

let vtrace_if_label = "if"
let vtrace_else_label = "else"
let vtrace_assign_label = "assign"

class add_vtrace_for_complex_exp_visitor ast fd = object(self)
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
          let vtrace_if_call = self#mk_vtrace vtrace_if_label loc in
          let vtrace_else_call = self#mk_vtrace vtrace_else_label loc in
          let () = if_block.bstmts <- [mkStmtOneInstr vtrace_if_call] @ if_block.bstmts in
          let () = else_block.bstmts <- [mkStmtOneInstr vtrace_else_call] @ else_block.bstmts in
          s
        else s
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)

  method vinst (i: instr) =
    let n_instr_list =
      match i with
      | Set (_, exp, loc) ->
        if is_complex_exp exp then
          let vtrace_call = self#mk_vtrace vtrace_assign_label loc in
          [i; vtrace_call]
        else [i]
      | Call (Some lhs, _, args, loc) ->
        if L.exists (fun arg -> is_complex_exp arg) args then
          let vtrace_call = self#mk_vtrace vtrace_assign_label loc in
          [i; vtrace_call]
        else [i]
      | _ -> [i]

    in 
    ChangeTo(n_instr_list)

end

let add_vtrace_for_complex_exp ast fd _ =
  let visitor = new add_vtrace_for_complex_exp_visitor ast fd in
  ignore (visitCilFunction (visitor :> nopCilVisitor) fd)

class collect_initialized_local_vars_visitor = object(self)
  inherit nopCilVisitor
  
  val mutable initialized_vars = []

  method vinst (i: instr) =
    (match i with
    | Call (v_opt, _, es, _) ->
      (match v_opt with
      | Some (Var v, _) when not (List.mem v.vname initialized_vars) ->
        initialized_vars <- initialized_vars @ [v.vname]
      | _ -> ())
    | Set ((Var v, _), e, _) when not (List.mem v.vname initialized_vars) ->
      initialized_vars <- initialized_vars @ [v.vname]
    | _ -> ());
    DoChildren

  method get_initialized_vars () =
    initialized_vars
end

let collect_uninitialized_local_vars (fd: fundec) =
  let culv = new collect_initialized_local_vars_visitor in
  ignore (visitCilBlock (culv :> nopCilVisitor) fd.sbody);
  let initialized_vars = culv#get_initialized_vars () in
  List.filter (fun vi -> not (List.mem vi.vname initialized_vars)) fd.slocals

let atoi_func_name = "atoi"
let argv_name = "argv"
let argc_name = "argc"
let atoi_func_type =
  mk_fun_typ intType [("str", charConstPtrType)]

let mk_argv_assignment v main_argv argv_index =
  (** arg <- *(argv + index) *)
  let arg = v2e (mkMem (increm (vi2e main_argv) argv_index) NoOffset) in
  mkStmtOneInstr (mk_Call ~ftype:atoi_func_type ~av:(Some (var v)) atoi_func_name [arg])

let mk_instrumenting_functions ast =
  let main_fd = find_fun ast (fun fd -> fd.svar.vname = main_name) in

  (** Create mainQ function 
    Assume that uninitialized vars are mainQ's arguments *)
  let mainQ_args = collect_uninitialized_local_vars main_fd in
  let mainQ_arg_names = L.map (fun vi -> vi.vname) mainQ_args in
  let mainQ_locals = L.filter (fun vi -> not (L.mem vi.vname mainQ_arg_names)) main_fd.slocals in
  let mainQ_type = mk_fun_typ voidType (List.map (fun v -> (v.vname, v.vtype)) mainQ_args) in
  let mainQ_fd = mk_fundec mainQ_prefix mainQ_type in
  let mainQ_call_stmt = mkStmtOneInstr (mk_Call ~ftype:mainQ_type mainQ_prefix (List.map vi2e mainQ_args)) in
  mainQ_fd.slocals <- mainQ_locals;
  mainQ_fd.sbody <- visitCilBlock (new create_void_return_visitor) main_fd.sbody;
  let main_return_stmt =
    let rt, _, _, _ = splitFunctionType main_fd.svar.vtype in
    match rt with
    | TVoid _ -> mkStmt (Return(None, !currentLoc))
    | _ -> mkStmt (Return(Some zero, !currentLoc))
  in
  main_fd.sformals <- [];
  let _ = makeFormalVar main_fd argc_name intType in
  let main_argv = makeFormalVar main_fd argv_name (TPtr (charPtrType, [])) in
  let _, argv_assignments = List.fold_left (
    fun (i, acc) arg -> 
      (i + 1, acc @ [mk_argv_assignment arg main_argv i]))
    (1, []) mainQ_args in
  main_fd.slocals <- mainQ_args;
  main_fd.sbody <- mkBlock (argv_assignments @ [mainQ_call_stmt; main_return_stmt]);
  let mainQ_global = GFun(mainQ_fd, !currentLoc) in
  ast.globals <- add_global_before_func main_name mainQ_global ast.globals

class add_empty_body_extern_visitor = object(self)
  inherit nopCilVisitor

  method vglob (g: global) =
    let n_globs = 
      match g with
      | GVarDecl (vi, loc) when (vi.vstorage = Extern && L.mem vi.vname extern_empty_body_funcs) ->
        (match vi.vtype with
        | TFun (rtype, atypes, _, attrs) ->
          let ftype = mk_fun_typ ~attrs:attrs rtype (L.map (fun (s, t, _) -> (s, t)) (argsToList atypes)) in
          let fd = mk_fundec vi.vname ftype in
          [GFun (fd, loc)]
        | _ -> [g])
      | _ -> [g]
    in
    ChangeTo(n_globs)
end

let () = 
  begin
    initCIL();
    Cil.lineDirectiveStyle := None; (* reduce code, remove all junk stuff *)
    Cprint.printLn := false; (* do not print line *)
    (* for Cil to retain &&, ||, ?: instead of transforming them to If stmts *)
    Cil.useLogicalOperators := true;

    let src = Sys.argv.(1) in
    let fn = Filename.remove_extension src in
    let ext = Filename.extension src in
    let ast = Frontc.parse src () in
    mk_instrumenting_functions ast;
    iterGlobals ast (only_functions (add_vtrace_for_complex_exp ast));
    visitCilFile (new add_empty_body_extern_visitor) ast;
    ast.globals <- (GText "#include \"stdlib.h\""):: ast.globals;
    (* write_stdout ast *)
    write_src (fn ^ "_instr" ^ ext) ast
  end