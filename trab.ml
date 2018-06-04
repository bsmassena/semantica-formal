type variable = string

(* Outros operadores binário e unários podem ser adicionados a linguagem *) 

(* SEMÂNTICA BIG-STEP *)

type operator = Sum | Diff | Mult | Div | Eq | Leq 

type tipo  = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo


type expr = Num of int 
			| Bool of bool 
			| Bop of operator * expr * expr
			| If of expr * expr * expr 
			| Var of variable 
			| App of expr * expr 
			| Lam of variable * tipo * expr 
			| Let of variable * tipo * expr * expr
			| Lrec of variable * tipo * tipo * variable * tipo * expr * expr
			(* expressões de lista *)
			| Cons of expr * expr
			| Nil
			| Hd of expr
			| Tl of expr
			| IsEmpty of expr
			(* expressões tryWith-raise *)
        	| TryWith of expr * expr
			| Raise

type value = Vnum of int 
			| Vbool of bool 
			| Vclos of variable * expr * enviroment
			| Vrclos of variable * variable * expr * enviroment
			(* valores de lista *)
			| Vcons of value * value
			| Vnil
			| Raise
and
	enviroment = (variable * value) list



(* AVALIADOR SEMÂNTICA BIG-STEP *)

let rec evaluate (env : enviroment) (e : expr) = (
	match e with
		(* BS-NUM *)
		Num(n) -> Vnum(n)
		(* BS-BOOL *)
		| Bool(b) -> Vbool(b)

		(* === REGRAS A SEREM IMPLEMENTADAS === *)
		(* BS-ID *)
		(* ==================================== *)

		(* BS-IF *)
		| If(e1, e2, e3) -> (
			let e1' = evaluate env e1 in (
				match e1' with
				(Vbool true) -> evaluate env e2
				| (Vbool false) -> evaluate env e3
				| Raise -> Raise
			)
		)

		(* === REGRAS A SEREM IMPLEMENTADAS === *)
		(* BS-FN *)
		(* BS-APP *)
		(* BS-LET *)
		(* BS-LETREC *)
		(* ==================================== *)

		(* BS-CONS *)
		| Cons(e1, e2) -> (
			let e1' = evaluate env e1 in
			let e2' = evaluate env e2 in (
				match (e1',e2') with
				| _ -> Vcons(e1', e2')
				| (Raise,_) -> Raise
				| (_,Raise) -> Raise
			)
		)
		(* BS-NIL *)
		| Nil -> Vnil
		(* BS-HD *)
		| Hd(e) -> (
			let e' = evaluate env e in (
				match e' with
				| Vcons(v1,v2) -> v1
				| Vnil -> Raise
				| Raise -> Raise
			)
		)
		(* BS-TL *)
		| Tl(e) -> (
			let e' = evaluate env e in (
				match e' with
				| Vcons(v1,v2) -> v2
				| Vnil -> Raise
				| Raise -> Raise
			)
		)
		(* BS-ISEMPTY *)
		| IsEmpty(e) -> (
			let e' = evaluate env e in (
				match e' with
				| Vnil -> (Vbool true)
				| Vcons(v1, v2) -> (Vbool false)
				| Raise -> Raise
			)
		)
		(* BS-TRYWITH *)
		| TryWith(e1,e2) -> (
			let e1' = evaluate env e1 in (
				match e1' with
				| Raise -> evaluate env e2
				| _ -> e1'
			)
		)
		(* BS-RAISE *)
		| Raise -> Raise

)

(* Inicialmente avaliacao está sendo testada com um ambiente vazio *)
let eval e = evaluate [] e

(* Expressoes para teste *)
let e1 = If(IsEmpty(Cons(Bool(true),Nil)),Bool(true),Bool(false))
let e2 = Hd(Cons(Num(5), Cons(Num(7), Nil)))
let e3 = Tl(Cons(Num(5), Cons(Num(7), Nil)))

(* Expressoes acima podem seguinte forma:

	- No terminal linux, execute o comando:
		ocaml

	- Após abrir o interpretador, execute o comando: 
		# use "trab.ml" 
			*(com o hashtag, ou seja, existem dois hashtags na tela)

	- Expressoes podem ser testadas com:
		eval nome_da_expressao
			*Por exemplo:
				eval e1
 *)



(* EXEMPLO DO PROFESSOR *)

(* Segue um exemplo de como o programa L1 abaixo pode ser representado internamente *)

(* let rec fat: int -> int = (fn x: int => if (x == 0) then 1 else x * (fat (x - 1)))
   in fat (5)
   end
*)

(*
Lrec("fat", TyInt, TyInt, "x", TyInt,
If(Bop(Eq, Var("x"), Num(0)),
   Num(1),
   Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
App(Var("fat"), Num(5)))
*)
					 
