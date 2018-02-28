(** This module implements a compiler from Fopix to Javix. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Javix

module S = Source.AST
module T = Target.AST

(** We will need the following pieces of information to be carrying
    along the translation: *)
type environment = {
  nextvar          : int;
  variables        : (S.identifier * T.var * bool) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
  mutable box_nextval: bool ref;
}

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
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

(* Create a new fresh label *)
let fresh_label: string -> T.label =
  let r = ref 0 in
  (fun str -> incr r; T.Label(str ^ string_of_int !r))

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
  { T.classname = "Fopix";
    T.code = code;
    T.varsize = 100;
    T.stacksize = 10000; }

(** [translate p env] turns a Fopix program [p] into a Javix program
    using [env] to retrieve contextual information. *)
let rec translate p env : T.t * environment =
  let rec program env defs =
    let code, env = List.fold_left translate_definition ([],env) defs
    in basic_program (code @ (translate_exit env)), env

  (* proper exit in javix *)
  and translate_exit env =
    let v = T.Var(env.nextvar -1) in (load_var v true) @ ((None, T.Ireturn) :: [])

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

  (* insert Javix function *)
  and insert_fun label fi code =
    (Some(label),T.Comment("Starting " ^ fi ^ " function")) :: code

  and translate_definition (o_code, env) = function
    | S.DefVal (i, e) -> def_val (o_code, env) (i, e)
    | S.DefFun (fi, fo, e) -> def_fun fi fo e env

  (*
     Each time you define a variable, take the integer bound to it at
     the top of the stack, box it, and store it in a variable indexed by v
  *)
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
    insert_fun f_label fi n_code, nenv

  and translate_expr env = function
    | S.Num i -> (None, T.Bipush(i))::[]
    | S.FunName fn ->
      (*let _ = { nextvar = env.nextvar; variables = env.variables;
                   function_labels = (fn, T.Label(fn))::env.function_labels;
                   function_formals = env.function_formals} in
                   None, T.Comment(";what should I put?")*)
      failwith "FunName - Students! this is your job!"

    | S.Var v ->
      (match (find_variable env v) with
      | Some(jv, bv) -> load_var jv bv
      | None -> failwith "No Javix variable binded to this Fopix var")

    | S.Let (i, e1, e2) ->
      let code, nenv = translate_definition ([], env) (S.DefVal(i,e1)) in
      code @ (translate_expr nenv e2)

    | S.IfThenElse (S.BinOp(op, a1, a2), e1, e2) when is_arith op = false ->
      let terms  = translate_expr env a1 @ translate_expr env a2 in
      translate_if env (translate_cmp op) terms e1 e2

    | S.IfThenElse (S.BinOp(op, a1, a2), e1, e2) ->
      let bcomp = S.BinOp(S.Eq, S.BinOp(op, a1, a2), S.Num(1)) in
      translate_expr env (S.IfThenElse (bcomp, e1, e2))

    | S.IfThenElse (cond, e1, e2) ->
      let bcomp = S.BinOp(S.Eq, cond, S.Num(1)) in
      translate_expr env (S.IfThenElse (bcomp, e1, e2))

    | S.BinOp(op, e1, e2) ->
      if is_arith op
      then generate_arith env (op, e1, e2)
      else generate_comp env (op, e1, e2)

    | S.BlockNew e ->
      let b = translate_expr env e in
      let _ = env_set_flag env false in
      b @ (None, T.Comment "Creating block") :: (None, T.Anewarray) :: []

    | S.BlockGet (e1,e2) ->
      let b = translate_expr env e1 in
      let i = translate_expr env e2 in
      b @ i @ (None, T.Comment "Getting from block") :: (None, T.AAload) :: []

    | S.BlockSet (e1,e2,e3) ->
      let b = translate_expr env e1 in
      let i = translate_expr env e2 in
      let v = translate_expr env e3 in
      (* Adding a T.Bipush(0) as a return value for the S.DefVal *)
      b @ i @ v @ (None, T.Comment "Setting block") :: (None, T.AAstore) ::
      (None, T.Bipush(0)) :: []

    | S.FunCall (e, el) ->
      let p = funcall_prologue env e el in
      let c = funcall_call env e el in
      let e = funcall_epilogue env e el in
      p @ c @ e :: []

    (* Récupéré d'un merge request. Quelle utilité ? Je ne sais pas encore... *)
    | S.Print s -> (None, T.Print(s)) :: []

    (* Functions related to Binary operations *)

    (* Check if an operator is arithmetic - '+', '-', '*', '/', ... *)
    and is_arith =
      function
      | S.Add | S.Sub | S.Mul | S.Div | S.Mod -> true
      | _ -> false

    (*
      In the following functions:
      - generate_arith
      - translate_op
      - translate_arith

      pre-condition: op is an arithmetic operator
    *)
    and generate_arith env (op, e1, e2) =
      let c1 = translate_expr env e1 in
      let c2 = translate_expr env e2 in
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
      translate_expr env if_comp

    (* Functions related to IfThenElse*)

    and translate_if env ifcomp terms e1 e2 =
      let ethen  = translate_expr env e1 in
      let eelse  = translate_expr env e2 in
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

    (* FunCall related functions *)

    and funcall_prologue env e el =
      save_vars env e el
      (*  Then push return address
          Then push args into stack
          Then Goto F via Calcul Goto Dispatch *)

    (*  It should:
        - pop args from stack into vars
        - execute core code
        - place final result at the top of the stack *)
    and funcall_call env e el =
      failwith "Students! This is our job (FunCall Call)."

    and save_vars env e el =
      failwith "Students! This is our job (FunCall Saving Vars)"

    (*  It should: 
        X Swap
        - Restore Vars
        - Goto Dispatch *)
    and funcall_epilogue env e el =
      let s = (None, T.Swap) :: [] in
      let r = restore_vars env e el in
      failwith "Students! This is our job (FunCall Epilogue)"

    and restore_vars env e el =
      failwith "Students! This is our job (FunCall Restore Vars)"

  in program env p

(** Remarks:
  - When using this compiler from fopix to javix, flap will
    produce some .j files.
    + Compile them to .class via: jasmin Foobar.j
    + Run them with: java -noverify Foobar

  - Final answer:
    your code should contain a final [Ireturn] that should
    return the value of the last DefVal (supposed to be
    an Integer).

  - Function Call Convention:
    + When a function starts, the stack should contain the
      return address (a label encoded as a number, see Labels.encode)
      then the n arguments of the function.
    + The function could freely use an modify any variable. So at least
      the variables that are reused after this call should have
      their contents saved in stack before the call and restored
      afterwards.
    + The function starts by moving its arguments from the stack to
      some variables.
    + When the function returns, the result should be on the top
      of the stack.

  - Boxing:
    The stack could contain both unboxed elements (Java int)
    or boxed elements (Java objects such as Integer or java arrays).
    We place into variables or in array cells only boxed values.
    The arithmetical operations (iadd, if_icmpeq, ...) only works
    on unboxed numbers.
    Conversion between int and Integer is possible via the
    Box and Unboxed pseudo-instructions (translated into correct
    calls to some ad-hoc methods we provide). You may try to
    do some obvious optimisations such as removing [Box;Unbox] or
    [Unbox;Box].

  - Tail-recursive calls : if the body of f ends with a call to
    another function g (which may be f itself in case of recursion),
    no need to save any variables, nor to push a new return address:
    just reuse the return address of the current call to f when
    jumping to g !

  - Variable size and stack size
    Your code should determine the number of variables used by the
    produced code. You might also try to compute the maximum
    stack size when the code is non-recursive or 100% tail-recursive.

*)
