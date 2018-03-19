(** This module implements a compiler from Kontix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Kontix
module S = Source.AST
module Target = Jakix
module T = Target.AST

type environment = {
  nextvar          : int;
  variables        : (S.identifier * T.var * bool) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
  tableswitch      : (T.label * int) list;
  mutable box_nextval: bool ref;
}

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
  tableswitch      = [];
  box_nextval      = ref true;
}

(** [lookup_function_label f env] returns the label of [f] in [env]. *)
let lookup_function_label f env =
  List.assoc f env.function_labels

(** [lookup_function_formals f env] returns the formal arguments of
    [f] in [env]. *)
let lookup_function_formals f env =
  List.assoc f env.function_formals

(** [fresh_function_label f] returns a fresh label starting with [f]
    that will be used for the function body instructions. *)
let fresh_function_label =
  let r = ref 0 in
  fun f ->
    incr r;
    T.Label (f ^ "_body_" ^ string_of_int !r)

let lookup_tableswitch l env =
  List.assoc l env.tableswitch

(** Variables *)

(** [bind_variable env x] associates Fopix variable x to the next
    available Javix variable, and return this variable and the updated
    environment *)
let bind_variable env x b =
  let v = T.Var env.nextvar in
  v,
  b,
  { env with
    nextvar = env.nextvar + 1;
    variables = (x,v,b) :: env.variables }

let clear_all_variables env = {env with variables = []; nextvar = 0}

let find_variable env v =
  let rec aux_find_variable l v =
    match l with
    | [] -> None
    | (fv, jv, bv)::t ->
      if fv = v then Some(jv, bv)
      else aux_find_variable t v
  in aux_find_variable env.variables v


(** Functions *)

let bind_function env f l =
  { env with
    function_labels = (f,l) :: env.function_labels }

let bind_formals env func fo =
  { env with
    function_formals = (func,fo) :: env.function_formals }

(** Environment *)

let env_set_flag env b =
  env.box_nextval := b

  (** For return addresses (or later higher-order functions),
      we encode some labels as numbers. These numbers could then
      be placed in the stack, and will be used in a final tableswitch
      We arbitrarily start these coding numbers at 1000, in order to
      easily distinguish them in javix code, adapt your tableswitch
      accordingly to use 1000 as base value. *)

module Labels :
 sig
  val encode : T.label -> int
  val all_encodings : unit -> (int * T.label) list
 end
 =
 struct
 let nextcode = ref 1000
  let allcodes = ref ([]:(int * T.label) list)
  let encode lab =
    let n = !nextcode in
    incr nextcode;
    allcodes := (n,lab) :: !allcodes;
    n
   let all_encodings () = !allcodes
 end


let rec translate (p : S.t) env = (failwith "TODO" : T.t * environment)
  let rec program env defs =
    let code, env = List.fold_left translate_definition ([], env) defs in
    code @ (translate_exit env), env

    (* proper exit in javix *)
  and translate_exit env =
    let v = T.Var(env.nextvar -1) in
    (load_var v true) @ ((None, T.Ireturn) :: [])

    (* store variable in javix *)
  and store_var v b =
    if b then
      (None, T.Box) :: (None, T.Astore(v)) :: []
    else
      (None, T.Astore(v)) :: []

    (* load variable in javix *)
  and load_var v b =
    if b then
      (None, T.Aload(v)) :: (None, T.Unbox) :: []
    else
      (None, T.Aload(v)) :: []

  and insert_fun label fi code =
    (Some(label),T.Comment("Starting " ^ fi ^ " function")) :: code

  and translate_definition (o_code, env) = function
  | S.DefFun (fi, fo, e) -> def_fun fi fo e env
  | S.DefCont (fi, fe, i, tail) -> failwith "TODO: "

  and def_val (o_code, env) (i, e) =
    let n_code = translate_expr env e in
    let v, b, nenv = bind_variable env i !(env.box_nextval) in
    let _ = env_set_flag nenv true in
    let vstore = store_var v b in
    o_code @ n_code @ vstore, nenv

  and def_fun fi fo e env =
    let n_code = translate_expr env e in
    let f_label = fresh_function_label fi in
    let nenv = bind_function env fi f_label in
    let nenv = bind_formals nenv fi fo in
    let _ = (f_label,Labels.encode f_label) :: env.tableswitch in
    insert_fun f_label fi n_code, nenv

  and translate_expr env = failwith "TODO translate"

(* --- *)
