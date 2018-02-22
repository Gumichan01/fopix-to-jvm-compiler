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

  and retrieve_definitions p =
    let rec aux_retrieve p (d, f) =
      match p with
      | [] -> (d, f)
      | (S.DefVal(_) as dv)::q -> aux_retrieve q ((d @ [dv]), f)
      | (S.DefFun(_) as df)::q -> aux_retrieve q (d, (f @ [df]))
    in aux_retrieve p ([], [])
