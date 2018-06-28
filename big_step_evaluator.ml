#use "type_definitions.ml"

(* FUNÇÕES AUXILIARES *)

(* Acha uma variavel no ambiente passado como argumento em "evaluate". Se ela nao existir, gera Raise *)
let rec find_from_enviroment (env : enviroment) (label : variable) = (
  match env with
    [] -> Raise
    | _ -> find_in_not_empty_list env label
) and find_in_not_empty_list (env : enviroment) (label : variable) = (
  match List.hd env with
    (l, value) when l = label -> value
    | _ ->  let new_env = List.tl env in find_from_enviroment new_env label
)

(* AVALIADOR SEMÂNTICA BIG-STEP *)

let rec evaluate (env : enviroment) (e : expr) = (
  match e with
    (* BS-NUM *)
    Num(n) -> Vnum(n)
    (* BS-BOOL *)
    | Bool(b) -> Vbool(b)
    (* BS-ID *)
    | Var(label) -> find_from_enviroment env label
    (* BS-IF *)
    | If(e1, e2, e3) -> (
      let e1' = evaluate env e1 in (
        match e1' with
        (Vbool true) -> evaluate env e2
        | (Vbool false) -> evaluate env e3
        | Raise -> Raise
      )
    )
    (* BS-FN *)
    | Lam(x, t, e) -> Vclos(x, e, env)
    (* BS-APP *)
    | App(e1, e2) -> (
        let e1' = evaluate env e1 in
        let e2' = evaluate env e2 in (
            match(e1', e2') with
            | (Vclos(x, e, env'), v) -> evaluate ((x,v)::env') e
            | (Vrclos(f, x, e, env'), v) -> evaluate ((x,v)::(f,Vrclos(f,x,e,env'))::env') e
            | (Raise,_) -> Raise
            | (_,Raise) -> Raise
        )
    )
    (* BS-LET *)
    | Let(x, t, e1, e2) -> let e1' = (evaluate env e1) in (
                               match e1' with
                               | Raise -> Raise
                               | _ -> evaluate ((x,e1')::env) e2
                            )
    (* BS-LETREC *)
    | Lrec(f,t1,t2,x,t1',e1,e2) -> let rclos = Vrclos(f,x,e1,env) in
                                       evaluate ((f,rclos)::env) e2
    (* BS-CONS *)
    | Cons(e1, e2) -> (
      let e1' = evaluate env e1 in
      let e2' = evaluate env e2 in (
        match (e1',e2') with
        | (Raise,_) -> Raise
        | (_,Raise) -> Raise
        | _ -> Vcons(e1', e2')
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
    (* BS-OP *)
    | Bop(op, e1, e2) -> (
        let e1' = evaluate env e1 in
        let e2' = evaluate env e2 in (
            match(op, e1', e2') with
                (Sum, Vnum n1, Vnum n2) -> Vnum(n1 + n2)
              | (Diff, Vnum n1, Vnum n2) -> Vnum(n1 - n2)
              | (Mult, Vnum n1, Vnum n2) -> Vnum(n1 * n2)
              | (Div, Vnum n1, Vnum n2) -> if n2 != 0 then Vnum(n1 / n2) else Raise
              | (Eq, Vnum n1, Vnum n2) -> Vbool(n1 == n2)
              | (Leq, Vnum n1, Vnum n2) -> Vbool(n1 <= n2)
              | (Less, Vnum n1, Vnum n2) -> Vbool(n1 < n2)
              | (Geq, Vnum n1, Vnum n2) -> Vbool(n1 >= n2)
              | (Greater, Vnum n1, Vnum n2) -> Vbool(n1 > n2)
              | (And, Vbool b1, Vbool b2) -> Vbool(b1 && b2)
              | (Or, Vbool b1, Vbool b2) -> Vbool(b1 || b2)
              | (_, Raise, _) -> Raise
              | (_, _, Raise) -> Raise
        )
    )
)
