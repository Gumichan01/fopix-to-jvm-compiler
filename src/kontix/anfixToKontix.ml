(** This module implements a compiler from Anfix to Kontix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Anfix
module S = Source.AST
module Target = Kontix
module T = Target.AST

type environment = unit (* TODO *)

let initial_environment () = () (* TODO *)

(* Create a fresh T.function_identifier *)
let fresh_function_identifier =
  let r = ref 0 in
  fun str -> incr r; let s = "_fun_" ^ str ^ (string_of_int !r) in s


let rec translate (p : S.t) (env : environment) =
  let ldeflist, fdeflist = retrieve_definitions p in
  (* Just to test *)
  print_string("hello "^ string_of_int(List.length ldeflist) ^" - "
               ^ string_of_int(List.length fdeflist));
  print_endline("");
  let compiled_defs  = translate_defs env ldeflist in
  let compiled_funcs = translate_funs env fdeflist in
  ((compiled_funcs, compiled_defs), env)

  and retrieve_definitions p =
    let rec aux_retrieve p (defl, funcl) =
      match p with
      | [] -> (defl, funcl)

      | (S.DefVal(_) as dv)::q ->
        let ndefl = defl @ [dv] in aux_retrieve q (ndefl, funcl)

      | (S.DefFun(_) as df)::q ->
        let nfuncl = funcl @ [df] in aux_retrieve q (defl, nfuncl)

    in aux_retrieve p ([], [])

  (* Translation of definitions *)
  and translate_defs env deflist =
    match deflist with
    | [] -> T.TContCall(T.Num(1))

    | S.DefVal(i, e)::q ->
      (match (translate_expr_tobasic env e) with
      | Some(te) -> T.TLet(i, te, (translate_defs env q))
      | None -> failwith "translate_defs: funcall")

    | _ -> assert(false) (* pre-condition : list of S.DefVal *)

  (* Translation of functions *)
  and translate_funs env funclist =
    match funclist with
    | [] -> []

    | (S.DefFun(fi, argv, e))::q ->
      let kdef = T.DefFun(fi , argv, translate_expr env e) in
      kdef :: (translate_funs env q)

    | _ -> assert(false) (* pre-condition : list of S.DefFun *)


  (* Should I return a pair <T.tailexpr, environment> instead of T.tailexpr? *)
  and translate_expr env =
    function
    | S.Simple(sexpr) -> T.TContCall(translate_simple sexpr)

    | S.Let(id, S.FunCall(S.FunName(fi), argv), e2) ->
      let e1 = S.FunCall(S.FunName(fi), argv) in
      let aux_e = fresh_function_identifier id in
      T.TPushCont(aux_e, [], (translate_expr env e1))

    | S.Let(id, S.FunCall(_, argv), e2) -> assert(false) (* FunCall must contain FunName *)

    | S.Let(id, e1, e2) ->
      (match translate_expr_tobasic env e1 with
      | Some(bexpr1) ->
        (match translate_expr_tobasic env e2 with
         | Some(bexpr2) -> T.TContCall(T.Let(id, bexpr1, bexpr2))
         | None -> T.TLet(id, bexpr1, (translate_expr env e2))
        )
      | None -> assert(false)) (* It is a FunCall -> I already checked this, WTF? *)

    | S.IfThenElse(c, e1, e2) ->
      T.TIfThenElse((translate_simple c), (translate_expr env e1), (translate_expr env e2))

    | S.BinOp(_)
    | S.BlockNew(_)
    | S.BlockGet(_)
    | S.BlockSet(_)
    | S.Print(_) as instr ->
      (match translate_expr_tobasic env instr with
       | Some(i) -> T.TContCall(i)
       | _ -> assert(false) (* non-sense *))

    | S.FunCall(_) -> failwith "TODO FunCall"

  (* Should I return a pair <T.basicexpr, environment> instead of T.basicexpr? *)
  and translate_expr_tobasic env e : T.basicexpr option =
    match e with
    | S.Simple(sexpr) -> Some(translate_simple sexpr)

    | S.Let(i, e, c) ->
      (match (translate_expr_tobasic env e), (translate_expr_tobasic env c) with
       | Some(e1), Some(e2) -> Some(T.Let(i, e1, e2))
       | _ -> None)

    | S.IfThenElse(cond, t, f) ->
      let kc = (translate_simple cond) in
      let kt = (translate_expr_tobasic env t) in
      let kf = (translate_expr_tobasic env f) in
      (match kt, kf with
       | Some(tr), Some(fs) -> Some(T.IfThenElse(kc, tr, fs))
       | _ -> None)

    | S.BinOp(o, e1, e2) ->
      let ke1 = translate_simple e1 in
      let ke2 = translate_simple e2 in
      Some(T.BinOp(o, ke1, ke2))

    | S.BlockNew(b) ->
      Some(T.BlockNew((translate_simple b)))

    | S.BlockGet(a, i) ->
      Some(T.BlockGet((translate_simple a), (translate_simple i)))

    | S.BlockSet(a, i, v) ->
      Some(T.BlockSet((translate_simple a), (translate_simple i),
           (translate_simple v)))

    | S.FunCall(_) -> None

    | S.Print(s) -> Some(T.Print(s))

  and translate_simple =
    function
    | S.Num(x) -> T.Num(x)
    | S.FunName(s) -> T.FunName(s)
    | S.Var(s) -> T.Var(s)
