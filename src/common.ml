open Cil
module E = Errormsg
module S = String
module L = List

let cil_tmp_prefix = "__cil_tmp"
let main_name = "main"
let mainQ_prefix = "mainQ"
let vtrace_prefix = "vtrace"

let contains s prefix =
  let prefix_len = S.length prefix in
  S.length s >= prefix_len &&
  S.sub s 0 prefix_len = prefix

let is_cil_tmp vname =
  contains vname cil_tmp_prefix

let mk_vtrace_name loc label =
  vtrace_prefix ^ "_" ^ label ^ "_" ^ (string_of_int loc.line)

let v2e (v: lval): exp = Lval v

let vi2e (vi: varinfo): exp = Lval (var vi)

let mk_fun_typ ?(is_var_arg=false) ?(attrs=[]) (rt : typ) (args : (string * typ) list): typ =
  TFun(rt, Some (L.map (fun a -> (fst a, snd a, [])) args), is_var_arg, attrs)

let mk_fundec (fname: string) (ftype: typ): fundec =
  let fd = emptyFunction fname in  
  let () = setFunctionTypeMakeFormals fd ftype in
  fd

let mk_Call ?(ftype=TVoid []) ?(av=None) (fname: string) args : instr = 
  let fvar = makeVarinfo true fname ftype in
  Call(av, vi2e fvar, args, !currentLoc)

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
