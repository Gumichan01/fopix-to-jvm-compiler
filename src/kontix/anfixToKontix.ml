(** This module implements a compiler from Anfix to Kontix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Anfix
module S = Source.AST
module Target = Kontix
module T = Target.AST

type environment = unit (* TODO *)

let initial_environment () = () (* TODO *)

let rec translate (p : S.t) (env : environment) = (* TODO translate *)
  let ldef, fdef = retrieve_definitions p in
  (* Just to test *)
  print_string("hello "^ string_of_int(List.length ldef) ^" - "
               ^ string_of_int(List.length fdef)); print_endline("");
               (([],T.TContCall(T.Print("exit"))), env)

  and retrieve_definitions p =
    let rec aux_retrieve p (d, f) =
      match p with
      | [] -> (d, f)
      | (S.DefVal(_) as dv)::q ->
        let l = d @ [dv] in aux_retrieve q (l, f)
      | (S.DefFun(_) as df)::q ->
        let l = f @ [df] in aux_retrieve q (d, l)
    in aux_retrieve p ([], [])

  and translate_defs env dl =
    match dl with
    | [] -> []
    | h::q ->
      let kdef = (translate_defv env h) in
      kdef :: (translate_defs env q)

  (* I should do something with env *)
  and translate_defv env dv = failwith "TODO definition of value"

  and translate_funs env fl =
    match fl with
    | [] -> []
    | h::q ->
      let kdef = (translate_deff env h) in
      kdef :: (translate_funs env q)

  (* I should do something with env *)
  and translate_deff env f = failwith "TODO definition of function"

  and translate_expr env e : T.basicexpr =
    match e with
    | S.Simple(sexpr)  -> translate_simple sexpr
    | S.Let(_,_,_) -> failwith "TODO Let" (* put the id in an environment *)
    | S.IfThenElse(_,_,_) -> failwith "TODO IfThenElse"
    | S.BinOp(o, e1, e2) ->
      (*let ko = translate_op o in*)
      let ke1 = translate_simple e1 in
      let ke2 = translate_simple e2 in
      T.BinOp(o, ke1, ke2)

    | S.BlockNew(b) ->
      T.BlockNew((translate_simple b))

    | S.BlockGet(a, i) ->
      T.BlockGet((translate_simple a), (translate_simple i))

    | S.BlockSet(a, i, v) ->
      T.BlockSet((translate_simple a), (translate_simple i), (translate_simple v))

    | S.FunCall(_,_) -> failwith "TODO FunCall" (* hum... *)

    | S.Print(s) -> T.Print(s)

  (* This fucntion can be unused because S.binop = T.binop = FopixAST.binop *)
  and translate_op (* : T.binop *) =
    function
    | S.Add -> T.Add
    | S.Sub -> T.Sub
    | S.Mul -> T.Mul
    | S.Div -> T.Div
    | S.Mod -> T.Mod
    | S.Eq  -> T.Eq
    | S.Le  -> T.Le
    | S.Lt  -> T.Lt
    | S.Ge  -> T.Ge
    | S.Gt  -> T.Gt

  and translate_simple (* : T.basicexpr *) =
    function
    | S.Num(x) -> T.Num(x)
    | S.FunName(s) -> T.FunName(s)
    | S.Var(s) -> T.Var(s)
