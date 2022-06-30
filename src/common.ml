open Cil
module E = Errormsg
module S = String
module L = List
module H = Hashtbl

let cil_tmp_prefix = "__cil_tmp"
let main_name = "main"
let mainQ_prefix = "mainQ"
let vtrace_prefix = "vtrace"
let if_label = "if"
let else_label = "else"
let vtrace_assign_label = "assign"
let error_func_name = "reach_error"

let assume_func_name = "__VERIFIER_assume"

let nondet_func_names = ["__VERIFIER_nondet_int"; "nondet"]

let atoi_func_name = "atoi"
let argv_name = "argv"
let argc_name = "argc"

let builtin_functions = 
  [main_name; mainQ_prefix; vtrace_prefix; error_func_name; assume_func_name; atoi_func_name] @
  nondet_func_names

let contains_prefix s prefix =
  let prefix_len = S.length prefix in
  S.length s >= prefix_len &&
  S.sub s 0 prefix_len = prefix

let contains s ss =
  let re = Str.regexp_string ss in
  try (ignore (Str.search_forward re s 0); true)
  with Not_found -> false

let find_or_create_local_var vname fd =
  match (List.find_opt (fun vi -> String.equal vname vi.vname) fd.slocals) with
  | Some vi -> vi
  | None -> makeLocalVar fd vname intType

let is_cil_tmp vname =
  contains_prefix vname cil_tmp_prefix

let is_main fname =
  contains_prefix fname main_name

let is_mainQ fname =
  contains_prefix fname mainQ_prefix

let is_builtin_function fname =
  L.exists (fun prefix -> contains_prefix fname prefix) builtin_functions

let fname_of_fundec fd =
  fd.svar.vname

let vi_of_var_exp e =
  match e with
  | Lval (Var vi, _) -> vi
  | _ -> E.s (E.warn "vi_of_var_exp: %a is not a var exp" d_exp e)


let vi_opt_of_var_exp e =
  match e with
  | Lval (Var vi, _) -> Some vi
  | _ -> None

let fname_of_call e =
  match vi_opt_of_var_exp e with
  | Some vi -> vi.vname
  | _ -> E.s (E.error "Cannot get function name from: %a" d_exp e)

let is_nondet_tmp_var vi =
  let desrc = Pretty.sprint ~width:80 vi.vdescr in
  List.exists (fun nd -> contains desrc nd) nondet_func_names

let boolType =
  match intType with
  | TInt (_, attrs) -> TInt (IBool, attrs)
  | _ -> intType

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

let mk_var_name ?(prefix=None) loc label = 
  (match prefix with | None | Some "" -> "" | Some p -> p ^ "_")
  ^ label ^ "_" ^ (string_of_int loc.line)

let mk_vtrace_name loc label =
  mk_var_name ~prefix:(Some vtrace_prefix) loc label

let v2e (v: lval): exp = Lval v

let vi2e (vi: varinfo): exp = Lval (var vi)

let neg (e: exp) =
  UnOp (LNot, e, boolType)

let mk_fun_typ ?(is_var_arg=false) ?(attrs=[]) (rt : typ) (args : (string * typ) list): typ =
  TFun(rt, Some (L.map (fun a -> (fst a, snd a, [])) args), is_var_arg, attrs)

let mk_fundec (fname: string) (ftype: typ): fundec =
  let fd = emptyFunction fname in  
  let () = setFunctionTypeMakeFormals fd ftype in
  fd

let mk_Call ?(ftype=TVoid []) ?(av=None) (fname: string) args : instr = 
  let fvar = makeVarinfo true fname ftype in
  Call(av, vi2e fvar, args, !currentLoc)

let atoi_func_type =
  mk_fun_typ intType [("str", charConstPtrType)]  

let mk_error_func_call () = 
  mkStmtOneInstr (mk_Call error_func_name [])

let mk_assume cond =
  mkStmtOneInstr (mk_Call assume_func_name [cond])

let mk_empty_block () = mkBlock []

let mk_error_block _ =
  let block = mk_empty_block () in
  block.bstmts <- (mk_error_func_call ())::block.bstmts;
  block

let only_functions (fn : fundec -> location -> unit) (g : global) : unit = 
  match g with
  | GFun(f, loc) -> fn f loc
  | _ -> ()

let find_fun (ast: file) (check: fundec -> bool): fundec = 
  let fd = 
    foldGlobals ast 
      (fun r g -> match r, g with 
        | Some _, _ -> r
        | None, GFun (f, _) when check f -> Some f
        | _ -> r
      ) None in
  match fd with
  | Some f -> f
  | None -> E.s (E.error "fun not found in '%s'!" ast.fileName)

let rec add_global_before_func func_name g gs =
  match gs with
  | [] -> [g]
  | ((GFun (fd, _)) as hd)::tl when fd.svar.vname = func_name -> g::hd::tl
  | hd::tl -> hd :: (add_global_before_func func_name g tl)

(** Visitors *)
class create_void_return_visitor = object(self)
  inherit nopCilVisitor

  method vstmt (s: stmt) =
    let action s =
      match s.skind with
      | Return (Some _, loc) -> 
        s.skind <- Return (None, loc);
        s
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)
end

class collect_vars_visitor = object(self)
  inherit nopCilVisitor

  val mutable vars = []

  method vexpr (e: exp) =
    (match e with
    | Lval (Var v, _) -> vars <- vars @ [v]
    | _ -> ());
    DoChildren

  method get_vars () =
    vars
end

let vars_of_exp e =
  let cvv = new collect_vars_visitor in
  ignore (visitCilExpr (cvv :> nopCilVisitor) e);
  cvv#get_vars ()

(** Output *)
let write_src ?(use_stdout:bool=false) (filename:string) (ast:file): unit = 
  let df oc = dumpFile defaultCilPrinter oc filename ast in
  if use_stdout then df stdout
  else (
    let fout = open_out filename in
    df fout;
    close_out fout;
    Printf.printf "write: %s\n" filename
  )

let write_stdout ast = write_src ~use_stdout:true "" ast
