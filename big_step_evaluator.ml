#use "type_definitions.ml"

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
