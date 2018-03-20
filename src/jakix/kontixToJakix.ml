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


 (*
   Generate labels for if-then-else instruction
   Creates two fresh labels, then<n>, endif<n> n ∈ [ 0 , +∞ [

 *)
 let fresh_iflabel: unit -> T.label * T.label =
   let r = ref 0 in
   (fun _ ->
     incr r;
     let rr = !r in
     ( T.Label("then" ^ "<" ^ string_of_int rr ^ ">"),
       T.Label("endif" ^ "<" ^ string_of_int rr ^ ">") )
   )

let basic_program code =
  { T.classname = "Kontix";
    T.code = code;
    T.varsize = 100;
    T.stacksize = 10000; }


let rec translate (p : S.t) (env : environment) =
  let rec program env (defs : S.t) =
    let dl, tail = defs in
    let code, nenv = List.fold_left translate_definition ([], env) dl in
    let tcode = translate_tailexpr nenv tail in
    basic_program (code @ tcode @ (translate_exit nenv)), nenv

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
  | S.DefCont (fi, fe, i, tail) -> def_cont (o_code, env) fi fe i tail

  and def_cont (o_code, env) fi fe i texpr =
    let n_code = translate_tailexpr env texpr in
    let v, b, nenv = bind_variable env i !(env.box_nextval) in
    let _ = env_set_flag nenv true in
    let vstore = store_var v b in
    o_code @ n_code @ vstore, nenv

  and def_val (o_code, env) (i, e) =
    let n_code = translate_basicexpr env e in
    let v, b, nenv = bind_variable env i !(env.box_nextval) in
    let _ = env_set_flag nenv true in
    let vstore = store_var v b in
    o_code @ n_code @ vstore, nenv

  and def_fun fi fo texpr env =
    let n_code = translate_tailexpr env texpr in
    let f_label = fresh_function_label fi in
    let nenv = bind_function env fi f_label in
    let nenv = bind_formals nenv fi fo in
    let _ = (f_label,Labels.encode f_label) :: env.tableswitch in
    insert_fun f_label fi n_code, nenv

  and translate_tailexpr env = function
    | S.TLet(i, bexpr, texpr) ->
      let ncode, nenv = def_val ([], env) (i, bexpr) in
      ncode @ (translate_tailexpr env texpr)

    | S.TIfThenElse (S.BinOp(op, a1, a2), te1, te2) when is_arith op = false ->
      let terms  = translate_basicexpr env a1 @ translate_basicexpr env a2 in
      translate_tail_if env (translate_cmp op) terms te1 te2

    | S.TIfThenElse (S.BinOp(op, a1, a2), te1, te2) ->
      let bcomp = S.BinOp(S.Eq, S.BinOp(op, a1, a2), S.Num(1)) in
      translate_tailexpr env ( S.TIfThenElse(bcomp, te1, te2) )

    | S.TIfThenElse (cond, te1, te2) ->
      let bcomp = S.BinOp(S.Eq, cond, S.Num(1)) in
      translate_tailexpr env ( S.TIfThenElse(bcomp, te1, te2) )

    | S.TPushCont(fi, idl, texpr) -> failwith "TODO PushCont"
    | S.TFunCall(bexpr, bl) -> failwith "TODO FunCall"
    | S.TContCall(bexpr) -> translate_basicexpr env bexpr

  and translate_basicexpr env = function
    | S.Num(x) -> (None, T.Bipush(x)) :: []

    | S.FunName fn -> failwith "FunName: What should I do?"

    | S.Var v ->
      (match (find_variable env v) with
       | Some(jv, bv) -> load_var jv bv
       | None -> failwith "No Jakix variable binded to this kontix var")

    | S.Let(i, bexpr1, bexpr2) -> (* ... *)
      let ncode, nenv = def_val ([], env) (i, bexpr1) in
      ncode @ (translate_basicexpr env bexpr2)

    | S.IfThenElse (S.BinOp(op, a1, a2), e1, e2) when is_arith op = false ->
      let terms  = translate_basicexpr env a1 @ translate_basicexpr env a2 in
      translate_basic_if env (translate_cmp op) terms e1 e2

    | S.IfThenElse (S.BinOp(op, a1, a2), e1, e2) ->
      let bcomp = S.BinOp(S.Eq, S.BinOp(op, a1, a2), S.Num(1)) in
      translate_basicexpr env ( S.IfThenElse(bcomp, e1, e2) )

    | S.IfThenElse (cond, e1, e2) ->
      let bcomp = S.BinOp(S.Eq, cond, S.Num(1)) in
      translate_basicexpr env ( S.IfThenElse(bcomp, e1, e2) )

    | S.BinOp(op, e1, e2) ->
      if is_arith op
      then generate_arith env (op, e1, e2)
      else generate_comp env (op, e1, e2)

    | S.BlockNew e ->
      let b = translate_basicexpr env e in
      let _ = env_set_flag env false in
      b @ (None, T.Comment "Creating block") :: (None, T.Anewarray) :: []

    | S.BlockGet (e1,e2) ->
      let b = translate_basicexpr env e1 in
      let i = translate_basicexpr env e2 in
      b @ i @ (None, T.Comment "Getting from block") :: (None, T.AAload) :: []

    | S.BlockSet (e1,e2,e3) ->
      let b = translate_basicexpr env e1 in
      let i = translate_basicexpr env e2 in
      let v = translate_basicexpr env e3 in
      (* Adding a T.Bipush(0) as a return value for the S.DefVal *)
      b @ i @ v @ (None, T.Comment "Setting block") :: (None, T.AAstore) ::
      (None, T.Bipush(0)) :: []

    | S.Print s -> (None, T.Print(s)) :: []


    (* Functions related to Binary operations *)
    (* Check if an operator is arithmetic - '+', '-', '*', '/', ... *)
    and is_arith =
      function
      | S.Add | S.Sub | S.Mul | S.Div | S.Mod -> true
      | _ -> false

    and translate_basic_if env ifcomp terms e1 e2 =
      let ethen  = translate_basicexpr env e1 in
      let eelse  = translate_basicexpr env e2 in
      translate_if_aux env ifcomp terms ethen eelse

    and translate_tail_if env ifcomp terms e1 e2 =
      let ethen  = translate_tailexpr env e1 in
      let eelse  = translate_tailexpr env e2 in
      translate_if_aux env ifcomp terms ethen eelse

    and translate_if_aux env ifcomp terms ethen eelse =
      let thlab, endlab = fresh_iflabel () in
      terms @ [(None, T.If_icmp(ifcomp, thlab))] @ eelse @
      [(None, T.Goto(endlab))] @ label_if thlab ethen @
      [Some(endlab), T.Comment("endif")]

    and label_if labelif =
      function
      | (None, i)::q -> (Some(labelif), i)::q
      | (Some(_), _)::_ as l -> l
      | [] -> assert false (* I cannot get an empty then-block or else-block *)

    and translate_cmp =
      function
      | S.Eq -> T.Eq
      | S.Le -> T.Le
      | S.Lt -> T.Lt
      | S.Ge -> T.Ge
      | S.Gt -> T.Gt
      | _ -> failwith "Binop: invalid operation"

    (*
    In the following functions:
    - generate_arith
    - translate_op
    - translate_arith

    pre-condition: op is an arithmetic operator
    *)
    and generate_arith env (op, e1, e2) =
      let c1 = translate_basicexpr env e1 in
      let c2 = translate_basicexpr env e2 in
      c1 @ c2 @ (translate_op env op)

    (*
      pre-condition: op is an arithmetic operator
    *)
    and translate_op env op =
      match (translate_arith op) with
      | Some(v) -> [(None, T.Binop(v))]
      | None -> failwith "Binop: invalid operation"

    and translate_arith =
      function
      | S.Add -> Some(T.Add)
      | S.Sub -> Some(T.Sub)
      | S.Mul -> Some(T.Mul)
      | S.Div -> Some(T.Div)
      | S.Mod -> Some(T.Rem)
      | _ -> None

    (*
    In the following functions:
    - generate_comp
    - translate_cmp

    pre-condition: op is a comparison operator
    *)
    and generate_comp env (op, e1, e2) =
      let if_comp = S.IfThenElse(S.BinOp(op, e1, e2), S.Num(1), S.Num(0) ) in
      translate_basicexpr env if_comp

  in program env p
(* --- *)
