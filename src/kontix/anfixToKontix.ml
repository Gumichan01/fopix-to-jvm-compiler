(** This module implements a compiler from Anfix to Kontix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Anfix
module S = Source.AST
module Target = Kontix
module T = Target.AST

type environment = unit (* TODO *)

let initial_environment () = () (* TODO *)

let rec translate (p : S.t) env =
  let rec translate_aux prog ev acc =
  match prog with
  | [] -> acc
  | d::q -> translate_aux q ev (translate_def env d)
  in
  translate_aux p env []

  and translate_def env = function
  | S.DefVal(_,_)   -> failwith "DefVal - TODO"
  | S.DefFun(_,_,_) -> failwith "DefFun - TODO"

(*(failwith "TODO translate" : T.t * environment)*)
