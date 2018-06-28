
(* Arquivo com funcoes para inferencia de tipos *)


exception TypeNotFound of string

(* === COLETA DE CONSTRAINTS === *)

let rec get_constraints_rec (type_env: type_enviroment) nextuvar (expression: expr) = match expression with
  | Num(t) -> (TyInt, nextuvar, [])
  | Bool(t) -> (TyBool, nextuvar, [])
  | Var(e1) ->
        (try (let term = (snd (List.find (fun (variable, _) -> String.compare variable e1 == 0) type_env)) in
                (match term with
                | _ -> (term, nextuvar, [])))
        with _ -> raise (TypeNotFound "Couldn't get variable constraint."))
  | If(t1,t2,t3) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t1 in
        let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar1 t2 in
        let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
        let newconstr = [(typeT1,TyBool); (typeT2,typeT3)] in
        (typeT3, nextuvar3,
            List.concat [newconstr; constr1; constr2; constr3])
  | Lam(v1,t1,e1) ->
        let newctypeX = [(v1,t1)] in
        let (typeT1,nextuvar1,constr1) = get_constraints_rec (List.concat [newctypeX;type_env]) nextuvar e1 in
        (typeT1, nextuvar1,
            List.concat [constr1])
  | App(t1,t2) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t1 in
        let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar1 t2 in
        let NextUVar(typeX,nextuvar') = nextuvar2() in
        let newconstr = [(typeT1,TyFn(typeT2,TyId(typeX)))] in
        ((TyId(typeX)), nextuvar',
            List.concat [newconstr; constr1; constr2])
  | Let(var1,t2,expr1,expr2) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar expr1 in
        let NextUVar(typeX1,nextuvar2) = nextuvar1() in
        let newctypeX = [(var1,TyId(typeX1))] in
        let (typeT2,nextuvar3,constr2) = get_constraints_rec (List.concat [newctypeX;type_env]) nextuvar2 expr2 in
        let newconstr = [(t2, typeT1)] in
        (typeT2, nextuvar3,
            List.concat [newconstr; constr1; constr2])
  | Lrec(var1,t1,t2,var2,t3,expr1,expr2) ->
        let firstctypeX = [(var1, TyFn(t1,t2))] in
        let secondctypeX = [(var2, t3)] in
        let (typeT1,nextuvar1,constr1) = get_constraints_rec (List.concat [secondctypeX;firstctypeX;type_env]) nextuvar expr1 in
        let (typeT2,nextuvar2,constr2) = get_constraints_rec (List.concat [firstctypeX;type_env]) nextuvar1 expr2 in
        let newconstr = [(t3, typeT1)] in
        (typeT2, nextuvar2,
            List.concat [newconstr; constr1; constr2])
  | Cons(t1,t2) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t1 in
        let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar1 t2 in
        let newconstr = [(TyList typeT1,typeT2)] in
        (typeT2, nextuvar2,
            List.concat [newconstr; constr1; constr2])
  | Nil ->
        let NextUVar(typeX,nextuvar') = nextuvar() in
        (TyList(TyId(typeX)), nextuvar', [])
  | Hd(t1) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t1 in
        let NextUVar(typeX,nextuvar') = nextuvar1() in
        let newconstr = [(typeT1,TyList(TyId(typeX)))] in
        ((TyId(typeX)), nextuvar',
            List.concat [newconstr; constr1])
  | Tl(t1) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t1 in
        let NextUVar(typeX,nextuvar') = nextuvar1() in
        let newconstr = [(typeT1,TyList(TyId(typeX)))] in
        ((TyId(typeX)), nextuvar',
            List.concat [newconstr; constr1])
  | IsEmpty(t) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t in
        let NextUVar(typeX,nextuvar') = nextuvar1() in
        let newconstr = [(typeT1,TyList(TyId(typeX)))] in
        ((TyId(typeX)), nextuvar',
            List.concat [newconstr; constr1])
  | TryWith(t1,t2) ->
        let (typeT1,nextuvar1,constr1) = get_constraints_rec type_env nextuvar t1 in
        let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar1 t2 in
        let newconstr = [(typeT1,typeT2)] in
        (typeT2, nextuvar2,
            List.concat [newconstr; constr1; constr2])
  | Raise ->
        let NextUVar(typeX,nextuvar') = nextuvar() in
        (TyId(typeX), nextuvar', [])
  | Bop(t1,t2,t3) -> (
        (match t1 with
            | Eq ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Leq ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Less ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Geq ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Greater ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Or ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyBool);(typeT3, TyBool)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | And ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyBool);(typeT3, TyBool)] in
                (TyBool, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Sum ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyInt, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Diff ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyInt, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Mult ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyInt, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            | Div ->
                let (typeT2,nextuvar2,constr2) = get_constraints_rec type_env nextuvar t2 in
                let (typeT3,nextuvar3,constr3) = get_constraints_rec type_env nextuvar2 t3 in
                let newconstr = [(typeT2, TyInt);(typeT3, TyInt)] in
                (TyInt, nextuvar3,
                    List.concat [newconstr; constr2; constr3])
            )
)

let get_constraints type_enviroment expression = get_constraints_rec type_enviroment uvargen expression


(* === UNIFY === *)

(** Exceções para Unify **)
exception NotUnifiable of string
exception CyclicSubstitution of string


(** Funções auxiliares para UNIFY **)

let check_if_occurs typeX typeT =
    let rec occur typeT = match typeT with
        | TyList(typeT1) -> occur typeT1
        | TyFn(typeT1,typeT2) -> occur typeT1 || occur typeT2
        | TyInt -> false
        | TyBool -> false
        | TyId(s) -> (s=typeX)
    in occur typeT

let apply_substitution_type typeX typeT typeS =
    let rec subs typeS = match typeS with
        | TyList(typeS1) -> TyList(subs typeS1)
        | TyFn(typeS1, typeS2) -> TyFn(subs typeS1, subs typeS2)
        | TyInt -> TyInt
        | TyBool -> TyBool
        | TyId(s) -> (if s=typeX then typeT else TyId(s)) 
    in subs typeS

let apply_substitution_constraint typeX typeT constraints =
    List.map (fun (typeS1,typeS2) -> (apply_substitution_type typeX typeT typeS1, apply_substitution_type typeX typeT typeS2)) constraints


(* Função principal de unify -> resolve as equações nas constraints *)
let unify constraints =
    let rec unify_rec constraints = match constraints with
        | [] -> []
        | (TyInt,TyInt) :: tail -> unify_rec tail 
        | (TyBool,TyBool) :: tail -> unify_rec tail 
        | (TyId(typeX),typeT) :: tail -> 
            if typeT = TyId(typeX) then unify_rec tail 
            else if check_if_occurs typeX typeT then 
                raise (CyclicSubstitution "Cyclic Substitution")
            else
                List.append (unify_rec (apply_substitution_constraint typeX typeT tail)) [(TyId(typeX),typeT)]
        | (typeT,TyId(typeX)) :: tail -> 
            if typeT = TyId(typeX) then unify_rec tail 
            else if check_if_occurs typeX typeT then 
                raise (CyclicSubstitution "Cyclic Substitution")
            else
                List.append (unify_rec (apply_substitution_constraint typeX typeT tail)) [(TyId(typeX),typeT)]
        | (TyFn(typeT1,typeT2),TyFn(typeT3,typeT4)) :: tail -> unify_rec ((typeT1,typeT3) :: (typeT2,typeT4) :: tail) (* Caso 6 *)
        | (TyList(typeT1),TyList(typeT2)) :: tail -> unify_rec ((typeT1,typeT2) :: tail) 
        | (typeS,typeT)::tail -> raise (NotUnifiable "Couldn't unify constraints")
    in unify_rec constraints



(* === INFERÊNCIA DE TIPO === *)

(** Função auxiliar **)
let apply_substitution typeS typeT =
    List.fold_left (fun typeS (TyId(typeX),tyC2) -> apply_substitution_type typeX tyC2 typeS) typeT (List.rev typeS)


let typeInfer type_enviroment expression =
    let typeT, nextuvar, constraints = get_constraints type_enviroment expression in
        let  typeS = unify constraints in 
                apply_substitution typeS typeT


