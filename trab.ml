#use "big_step_evaluator.ml"
#use "type_inference.ml"
#use "debugging.ml"


(* === EXPRESSOES PARA TESTES === *)

(* IF *)
let if_test = If(IsEmpty(Cons(Bool(true),Nil)),Bool(true),Bool(false))
(* LISTA - HEAD *)
let head_test = Hd(Cons(Num(5), Cons(Num(7), Nil)))
(* LISTA - TAIL *)
let tail_test = Tl(Cons(Num(5), Cons(Num(7), Nil)))
(* SOMA *)
let sum_test = Bop(Sum, Num(1), Num(2))
(* PRODUTO *)
let mult_test = Bop(Mult, Num(3), Num(4))
(* IGUAL *)
let equal_test = Bop(Eq, Num(4), Num(4))
(* VAR *)
let var_test = Var("x")
(* LET *)
let let_test = Let("z", TyInt, Num(7), Var("z"))
let let_test2 = Let("y", TyInt, Num(7), Bop(Mult,Var("y"), Num(10)))
(* LET REC *)
let letrec_test = (Lrec("fat", TyInt, TyInt, "x", TyInt,
					If(Bop(Eq, Var("x"), Num(0)),
						Num(1),
						Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
					App(Var("fat"), Num(5)))
				  )


(* === TESTES DE AVALIAÇAO === *)

(* Inicialmente avaliacao está sendo testada com um ambiente vazio *)
let eval e = evaluate [] e

(* Podem ser definidas variáveis previamente em um ambiente *)
let test_env = [("x", Vnum(2)); ("y", Vnum(3))]
(* Avaliação com variaveis definidas no ambiente *)
let eval_env e = evaluate test_env e



(* === TESTES DE INFERENCIA DE TIPOS === *)

 (* Lista com expressoes de teste *)
let expr_list = [letrec_test; mult_test; head_test; tail_test; if_test; tail_test]

(* Teste de inferencia de tipo com expressoes da lista *)
let partial_inference_test = print_sub_inferred_types expr_list
let inference_test = print_inferred_types expr_list




(* --- EXEMPLO DE TESTE DO PROFESSOR --- *)

(* Segue um exemplo de como o programa L1 abaixo pode ser representado internamente *)

(* let rec fat: int -> int = (fn x: int => if (x == 0) then 1 else x * (fat (x - 1)))
   in fat (5)
   end
*)
