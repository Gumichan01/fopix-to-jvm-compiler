(** Conversion from Fopix to Anfix *)

(** For the moment, we only handle Fopix code which is already
    in ANF form (i.e. all arguments of function call and binop
    are simple). This is used for parsing Anfix without defining
    a full parser (but reusing Fopix parser instead) *)


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

and are_all_simple = function
  | [] -> true
  | e::q when (is_simple e) -> are_all_simple q
  | _ -> false

and expr : S.expression -> T.expression = function
  | S.Num n -> T.Simple (T.Num n)
  | S.FunName f -> T.Simple (T.FunName f)
  | S.Var x -> T.Simple (T.Var x)
  | S.Let (x,e1,e2) -> T.Let (x, expr e1, expr e2)
  | S.IfThenElse (e1,e2,e3) -> expr_if (e1, e2, e3)
  | S.BinOp (b,e1,e2) -> expr_binop (b, e1, e2)
  | S.BlockNew e -> expr_blocknew e
  | S.BlockGet (e1,e2) -> expr_blockget (e1, e2)
  | S.BlockSet (e1,e2,e3) -> expr_blockset (e1, e2, e3)
  | S.FunCall (e,el) -> expr_funcall (e, el)
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

and expr_blocknew e =
  match is_simple e with
  | true  -> T.BlockNew (simplexpr e)
  | false ->
    let s, bn = fresh_variable "anfix" in
    expr ( S.Let (s, e, S.BlockNew(bn) ) )

and expr_blockget (e1, e2) =
  match is_simple e1, is_simple e2 with
  | true, true -> T.BlockGet (simplexpr e1, simplexpr e2)

  | false, true ->
    let s, t = fresh_variable "anfix" in
    expr ( S.Let(s, e1, S.BlockGet(t, e2)) )

  | true, false ->
    let s, t = fresh_variable "anfix" in
    expr ( S.Let(s, e1, S.BlockGet(e1, t)) )

  | _ ->
    let s1, t1 = fresh_variable "anfix" in
    let s2, t2 = fresh_variable "anfix" in
    expr ( S.Let (s1, e1, S.Let(s2, t2, S.BlockGet(t1, t2))) )

and expr_blockset (e1, e2, e3) =
  let l = e1 :: e2 :: e3 :: [] in
  match l with
  | [] -> assert(false) (* S.BlockSet(e1,e2,e3) *)
  | _ as l ->
    match are_all_simple l with
    | true -> T.BlockSet (simplexpr e1, simplexpr e2, simplexpr e3)
    | false ->
      let s1, t1 = fresh_variable "anfix" in
      let s2, t2 = fresh_variable "anfix" in
      let s3, t3 = fresh_variable "anfix" in
      expr ( S.Let(s1, e1, S.Let( s2, t2, S.Let(s3, e3, S.BlockSet(t1, t2, t3)))) )

and expr_funcall (e, el) =
  match is_simple e, are_all_simple el with
  | true, true  -> T.FunCall (simplexpr e, List.map simplexpr el)
  | false, _ ->
    let s, t = fresh_variable "anfix_fun" in
    expr ( S.Let(s, e, S.FunCall(t, el)) )
  | true, _ -> expr ( expr_funcall_aux_bis e el )

and expr_funcall_aux_bis e l =
  let rec funcall_aux argv = function
  | [] -> S.FunCall(e, argv)
  | ex::q ->
    let s, svar = fresh_variable "anfix_argv" in
    S.Let(s, ex, (funcall_aux (argv @ [svar]) q) )
  in funcall_aux [] l
