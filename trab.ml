#use "big_step_evaluator.ml"

(* Expressoes para teste *)
let e1 = If(IsEmpty(Cons(Bool(true),Nil)),Bool(true),Bool(false))
let e2 = Hd(Cons(Num(5), Cons(Num(7), Nil)))
let e3 = Tl(Cons(Num(5), Cons(Num(7), Nil)))
let sum = Bop(Sum, Num(1), Num(2))
let mult = Bop(Mult, Num(3), Num(4))
let eq = Bop(Eq, Num(4), Num(4))

let var = Var("x")
let let_test = Let("z", TyInt, Num(7), Var("z"))



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

(* let rec typeInfer (env : enviroment) (e : expr) = ( *)
