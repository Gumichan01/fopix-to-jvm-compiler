(** This module implements a compiler from Anfix to Kontix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Anfix
module S = Source.AST
module Target = Kontix
module T = Target.AST

type environment = unit (* TODO *)

let initial_environment () = () (* TODO *)

let rec translate (p : S.t) (env : environment) = (*failwith "TODO translate" : T.t * environment*)
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

  and translate_defv env dv = failwith "TODO definition of value"

  and translate_funs env fl =
    match fl with
    | [] -> []
    | h::q ->
      let kdef = (translate_deff env h) in
      kdef :: (translate_funs env q)

  and translate_deff env f = failwith "TODO definition of function"

  and translate_expr env =
    function
    | S.Simple(_)  -> ()
    | S.Let(_,_,_) -> ()
    | S.IfThenElse(_,_,_) -> ()
    | S.BinOp(_,_,_) -> ()
    | S.BlockNew(_) -> ()
    | S.BlockGet(_,_) -> ()
    | S.BlockSet(_,_,_) -> ()
    | S.FunCall(_,_) -> ()
    | S.Print(_) -> ()
