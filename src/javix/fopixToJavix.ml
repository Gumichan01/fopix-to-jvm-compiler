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
  variables        : (S.identifier * T.var) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
}

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
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
let bind_variable env x =
  let v = T.Var env.nextvar in
  v,
  { env with
    nextvar = env.nextvar + 1;
    variables = (x,v) :: env.variables }

let clear_all_variables env = {env with variables = []; nextvar = 0}

let find_variable env v =
  let rec aux_find_variable l v =
    match l with
    | [] -> None
    | (fv, jv)::t ->
      if fv = v then Some(jv)
      else aux_find_variable t v
  in aux_find_variable env.variables v

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
    let v = T.Var(env.nextvar -1) in (load_var v) @ ((None, T.Ireturn) :: [])

  (* store variable in javix *)
  and store_var v = (None, T.Box) :: (None, T.Astore(v)) :: []

  (* load variable in javix *)
  and load_var v = (None, T.Aload(v)) :: (None, T.Unbox) :: []

  and translate_definition (o_code, env) = function
    | S.DefVal (i, e) ->
      let n_code  = translate_expr env e in
      let v, nenv = bind_variable env i in
      (*
         Each time you define a variable, take the integer bound to it at
         the top of the stack, box it, and store it in a variable indexed by v
      *)
      let vstore = store_var v in
      o_code @ n_code @ vstore, nenv
    | S.DefFun (fi, fo, e) -> failwith "DefFun - Students! This is your job!"

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
      | Some(jv) -> load_var jv
      | None -> failwith "No Javix variable binded to this Fopix var")

    | S.Let (i, e1, e2) ->
      let code, nenv = translate_definition ([], env) (S.DefVal(i,e1)) in
      code @ (translate_expr nenv e2)

    | S.IfThenElse (cond, e1, e2) ->
      failwith "If then else - Students! this is your job!"

    | S.BinOp(op, e1, e2) ->
      let c1 = translate_expr env e1 in
      let c2 = translate_expr env e2 in
      c1 @ c2 @ (translate_op op)

    (* Currently, it isn't working properly because Anewarray pushes
       an address and DefVal boxes it. Javix doesn't seem to like it ! *)

    | S.BlockNew e ->
      let b = translate_expr env e in
      b @ (None, T.Comment "Creating block")::(None, T.Anewarray)::[] 

    | S.BlockGet (e1,e2) ->
      let b = translate_expr env e1 in
      let i = translate_expr env e2 in
      b @ i @ (None, T.Comment "Getting from block")::(None, T.AAload)::[]

    | S.BlockSet (e1,e2,e3) ->
      let b = translate_expr env e1 in
      let i = translate_expr env e2 in
      let v = translate_expr env e3 in
      (* Adding a T.Bipush(0) as a return value for the S.DefVal *)
      b @ i @ v @ (None, T.Comment "Setting block")::(None, T.AAstore)::(None, T.Bipush(0))::[]

    | S.FunCall _ ->
      failwith "FunCall - Students! this is your job!"

    and translate_op op  = [(None, T.Binop(translate_op_aux op))]

    and translate_op_aux =
      function
      | S.Add -> T.Add
      | S.Sub -> T.Sub
      | S.Mul -> T.Mul
      | S.Div -> T.Div
      | S.Mod -> T.Rem
      | _ -> failwith "Binop: invalid operation"
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
