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
