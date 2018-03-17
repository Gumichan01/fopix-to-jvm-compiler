(** Conversion from Fopix to Anfix *)

(** For the moment, we only handle Fopix code which is already
    in ANF form (i.e. all arguments of function call and binop
    are simple). This is used for parsing Anfix without defining
    a full parser (but reusing Fopix parser instead) *)

(** TODO: extend this code into a full Fopix to Anfix converter *)

module S=FopixAST
module T=AnfixAST

type environment = unit
let initial_environment () = ()

type defs = (T.identifier * T.expression) list

let fresh_variable =
  let r = ref 0 in
  fun str -> incr r; let s = str ^ (string_of_int !r) in (s, S.Var(s))

let rec program l = List.map definition l

and definition = function
  | S.DefVal (i,e) -> T.DefVal (i,expr e)
  | S.DefFun (f,a,e) -> T.DefFun (f,a,expr e)

and simplexpr : S.expression -> T.simplexpr = function
  | S.Num n -> T.Num n
  | S.FunName f -> T.FunName f
  | S.Var x -> T.Var x
  | e -> failwith ("This expression should be simple:" ^
                     FopixPrettyPrinter.(to_string expression e))

and is_simple : S.expression -> bool = function
  | S.Num _ | S.FunName _ | S.Var _ -> true
  | e -> false

and expr : S.expression -> T.expression = function
  | S.Num n -> T.Simple (T.Num n)
  | S.FunName f -> T.Simple (T.FunName f)
  | S.Var x -> T.Simple (T.Var x)
  | S.Let (x,e1,e2) -> T.Let (x, expr e1, expr e2)
  | S.IfThenElse (e1,e2,e3) -> expr_if (e1, e2, e3)
  | S.BinOp (b,e1,e2) -> expr_binop (b, e1, e2)
  | S.BlockNew e -> T.BlockNew (simplexpr e)
  | S.BlockGet (e1,e2) -> T.BlockGet (simplexpr e1, simplexpr e2)
  | S.BlockSet (e1,e2,e3) -> T.BlockSet (simplexpr e1, simplexpr e2, simplexpr e3)
  | S.FunCall (e,el) -> T.FunCall (simplexpr e, List.map simplexpr el)
  | S.Print s -> T.Print s

and expr_if (e1, e2, e3) =
  match is_simple e1 with
  | true  -> T.IfThenElse (simplexpr e1, expr e2, expr e3)
  | false ->
    let s, ifx = fresh_variable "anfix" in
    expr ( S.Let (s, e1, S.IfThenElse(ifx, e2, e3) ) )

and expr_binop (b, e1, e2) =
  match is_simple e1, is_simple e2 with
  | true, true -> T.BinOp (b, simplexpr e1, simplexpr e2)

  | false, true ->
    let s, t = fresh_variable "anfix" in
    expr ( S.Let(s, e1, S.BinOp(b, t, e2)) )

  | true, false ->
    let s, t = fresh_variable "anfix" in
    expr ( S.Let(s, e2, S.BinOp(b, e1, t)) )

  | _ ->
    let s1, t1 = fresh_variable "anfix" in
    let s2, t2 = fresh_variable "anfix" in
    expr ( S.Let (s1, e1, S.Let(s2, t2, S.BinOp(b, t1, t2))) )
