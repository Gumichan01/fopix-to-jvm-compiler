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
  let ldef = retrieve_dval p in
  let fdef = retrieve_dfun p in
  (* Just to test *)
  print_string("hello "^ string_of_int(List.length ldef) ^" - "
               ^ string_of_int(List.length fdef)); print_endline(""); (([],T.TContCall(T.Print("exit"))), env)

  and retrieve_dval =
  (function
  | [] -> []
  | (S.DefVal(_) as dv)::q -> dv :: retrieve_dval q
  | _::q -> retrieve_dval q
  )

  and retrieve_dfun =
  (function
  | [] -> []
  | (S.DefFun(_) as df)::q -> df :: retrieve_dfun q
  | _::q -> retrieve_dfun q
  )

  (*match p with
  | [] -> ([], T.TContCall(T.Print("")))
  | d::q ->
    let cd = translate_def env d in
    ( cd :: , T.TContCall(T.Print("")))*)

  (*let rec translate_aux prog ev acc =
  match prog with
  | [] -> acc
  | d::q -> translate_aux q ev (acc @ translate_def env d)
  in
  translate_aux p env []*)

  (*and translate_def env = function
  | S.DefVal(_,_)   -> failwith "DefVal - TODO"
  | S.DefFun(_,_,_) -> failwith "DefFun - TODO"*)

(*(failwith "TODO translate" : T.t * environment)*)
