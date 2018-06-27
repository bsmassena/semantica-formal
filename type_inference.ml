
(* Arquivo com funcoes para inferencia de tipos *)

exception UndeclaredVariable of string

let rec get_constraints (tyEnv: type_enviroment) nextuvar (expression: expr) = match expression with
  | Num(t) -> (TyInt, nextuvar, [])
  | Bool(t) -> (TyBool, nextuvar, [])
  | Var(e1) ->
    (try (let term = (snd (List.find (fun (variable, _) -> String.compare variable e1 == 0) tyEnv)) in
    (match term with
     | _ -> (term, nextuvar, [])))
   with _ -> raise (UndeclaredVariable "Didn't found variable in context"))
  | If(t1,t2,t3) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t1 in
      let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar1 t2 in
      let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
      let newconstr = [(tyT1,TyBool); (tyT2,tyT3)] in
      (tyT3, nextuvar3,
      List.concat [newconstr; constr1; constr2; constr3])
  | Lam(v1,t1,e1) ->
      let newctx = [(v1,t1)] in
      let (tyT1,nextuvar1,constr1) = get_constraints (List.concat [newctx;tyEnv]) nextuvar e1 in
      (tyT1, nextuvar1,
      List.concat [constr1])
  | App(t1,t2) ->
    let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t1 in
    let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar1 t2 in
    let NextUVar(tyX,nextuvar') = nextuvar2() in
    let newconstr = [(tyT1,TyFn(tyT2,TyId(tyX)))] in
    ((TyId(tyX)), nextuvar',
    List.concat [newconstr; constr1; constr2])
  | Let(var1,t2,expr1,expr2) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar expr1 in
      let NextUVar(tyX1,nextuvar2) = nextuvar1() in
      let newctx = [(var1,TyId(tyX1))] in
      let (tyT2,nextuvar3,constr2) = get_constraints (List.concat [newctx;tyEnv]) nextuvar2 expr2 in
      let newconstr = [(t2, tyT1)] in
      (tyT2, nextuvar3,
      List.concat [newconstr; constr1; constr2])
  | Lrec(var1,t1,t2,var2,t3,expr1,expr2) ->
      let firstctx = [(var1, TyFn(t1,t2))] in
      let secondctx = [(var2, t3)] in
      let (tyT1,nextuvar1,constr1) = get_constraints (List.concat [secondctx;firstctx;tyEnv]) nextuvar expr1 in
      let (tyT2,nextuvar2,constr2) = get_constraints (List.concat [firstctx;tyEnv]) nextuvar1 expr2 in
      let newconstr = [(t3, tyT1)] in
      (tyT2, nextuvar2,
      List.concat [newconstr; constr1; constr2])
  | Cons(t1,t2) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t1 in
      let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar1 t2 in
      let newconstr = [(TyList tyT1,tyT2)] in
      (tyT2, nextuvar2,
      List.concat [newconstr; constr1; constr2])
  | Nil ->
    let NextUVar(tyX,nextuvar') = nextuvar() in
    (TyList(TyId(tyX)), nextuvar', [])
  | Hd(t1) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t1 in
      let NextUVar(tyX,nextuvar') = nextuvar1() in
      let newconstr = [(tyT1,TyList(TyId(tyX)))] in
      ((TyId(tyX)), nextuvar',
      List.concat [newconstr; constr1])
  | Tl(t1) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t1 in
      let NextUVar(tyX,nextuvar') = nextuvar1() in
      let newconstr = [(tyT1,TyList(TyId(tyX)))] in
      ((TyId(tyX)), nextuvar',
      List.concat [newconstr; constr1])
  | IsEmpty(t) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t in
      let NextUVar(tyX,nextuvar') = nextuvar1() in
      let newconstr = [(tyT1,TyList(TyId(tyX)))] in
        ((TyId(tyX)), nextuvar',
        List.concat [newconstr; constr1])
  | TryWith(t1,t2) ->
      let (tyT1,nextuvar1,constr1) = get_constraints tyEnv nextuvar t1 in
      let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar1 t2 in
      let newconstr = [(tyT1,tyT2)] in
      (tyT2, nextuvar2,
      List.concat [newconstr; constr1; constr2])
  | Raise ->
    let NextUVar(tyX,nextuvar') = nextuvar() in
    (TyId(tyX), nextuvar', [])
  | Bop(t1,t2,t3) -> (
      (match t1 with
        | Eq ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Leq ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Less ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Geq ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Greater ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Or ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyBool);(tyT3, TyBool)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | And ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyBool);(tyT3, TyBool)] in
            (TyBool, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Sum ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyInt, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Diff ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyInt, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Mult ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyInt, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        | Div ->
            let (tyT2,nextuvar2,constr2) = get_constraints tyEnv nextuvar t2 in
            let (tyT3,nextuvar3,constr3) = get_constraints tyEnv nextuvar2 t3 in
            let newconstr = [(tyT2, TyInt);(tyT3, TyInt)] in
            (TyInt, nextuvar3,
            List.concat [newconstr; constr2; constr3])
        )
)
let collectTyEqs type_enviroment expression = get_constraints type_enviroment uvargen expression

(***** UNIFY *****)
exception UnifyFailed of string

(* Funções auxiliares *)
(* Substitui X por T no tipo S*)
let substitutionInType tyX tyT tyS =
  let rec subs tyS = match tyS with
    | TyList(tyS1) -> TyList(subs tyS1)
    | TyFn(tyS1, tyS2) -> TyFn(subs tyS1, subs tyS2)
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyId(s) -> (if s=tyX then tyT else TyId(s)) (* se for X, troca por T*)
  in subs tyS

(* Chama a substituição de tyX por tyT para cada equação do conjunto*)
let sustitutionInTyEquation tyX tyT tyEquations =
  List.map (fun (tyS1,tyS2) -> (substitutionInType tyX tyT tyS1, substitutionInType tyX tyT tyS2)) tyEquations

(* Verifica se o tipo X ocorre em T*)
let occurCheck tyX tyT =
  let rec occur tyT = match tyT with
    | TyList(tyT1) -> occur tyT1
    | TyFn(tyT1,tyT2) -> occur tyT1 || occur tyT2
    | TyInt -> false
    | TyBool -> false
    | TyId(s) -> (s=tyX) (* define se X ocorre ou não em T*)
  in occur tyT

(* Função de Unify *)
(* Recebe as equações de tipo em tyEquations e retorna as substituições de tipo*)
(* let tySubstitutions = unify tyEquations *)
let unify tyEquations =
  let rec unify_rec tyEquations = match tyEquations with
    | [] -> []
    | (TyInt,TyInt) :: tail -> unify_rec tail (* Caso 1 *)
    | (TyBool,TyBool) :: tail -> unify_rec tail (* Caso 2 *)
    | (TyId(tyX),tyT) :: tail -> (* Caso 4 *)
        if tyT = TyId(tyX) then unify_rec tail (* Caso 3 *)
        else if occurCheck tyX tyT then (* Se X ocorre em T, não é uma equação válida*)
          raise (UnifyFailed "occurCheck didn't pass: circular type")
        else (* Se não, faz a substituição de X por T no resto das equações e chama o Unify novamente. Ainda, adiciona na lista de substituições (X,T)*)
          List.append (unify_rec (sustitutionInTyEquation tyX tyT tail)) [(TyId(tyX),tyT)]
    | (tyT,TyId(tyX)) :: tail -> (* Caso 5 *)
        if tyT = TyId(tyX) then unify_rec tail (* Caso 3 *)
        else if occurCheck tyX tyT then (* Se X ocorre em T, não é uma equação válida*)
          raise (UnifyFailed "occurCheck didn't pass: circular type")
        else (* Se não, faz a substituição de X por T no resto das equações e chama o Unify novamente. Ainda, adiciona na lista de substituições (X,T)*)
          List.append (unify_rec (sustitutionInTyEquation tyX tyT tail)) [(TyId(tyX),tyT)]
    | (TyFn(tyT1,tyT2),TyFn(tyT3,tyT4)) :: tail -> unify_rec ((tyT1,tyT3) :: (tyT2,tyT4) :: tail) (* Caso 6 *)
    | (TyList(tyT1),TyList(tyT2)) :: tail -> unify_rec ((tyT1,tyT2) :: tail) (* Caso 7 *)
    | (tyS,tyT)::tail -> raise (UnifyFailed "Not possible to solve type equations")
  in unify_rec tyEquations


(*** ApplySubs ***)
(* Aplica as substituições obtidas no Unify no tipo obtido pelo collectTyEqs *)
(* List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn *)
let applySubs tySubstitutions tyT =
  List.fold_left (fun tyS (TyId(tyX),tyC2) -> substitutionInType tyX tyC2 tyS) tyT (List.rev tySubstitutions)


(*** TypeInfer ***)
let typeInfer type_enviroment expression =
  let tyT, nextuvar, tyEquations = collectTyEqs type_enviroment expression in
    let  tySubstitutions = unify tyEquations in
      applySubs tySubstitutions tyT


