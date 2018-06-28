
(* Arquivo com funcoes usadas para os testes *)

let rec type_to_string (tp:tipo) : string =
  match tp with
  | TyInt  -> "INT"
  | TyBool -> "BOOL"
  | TyList a -> (type_to_string a) ^ "LIST"
  | TyFn(tp1,tp2) -> "[" ^ (type_to_string tp1) ^ " --> " ^ (type_to_string tp2) ^ "]"
  | TyId(tp) -> "TyId:" ^ tp
;;

let rec list_to_string (lista: (tipo * tipo) list) =  match lista with
    | (head::tail) ->
        (match head with
          | t1, t2 -> print_endline ("[" ^ (type_to_string t1) ^ "  |  " ^ (type_to_string t2) ^ "]"))
        ;
        list_to_string tail;
    | [] -> ();;


let rec p_sub_inferred_types exp counter = match exp with
  | (hd::tl) ->
    (match hd with
      | head -> let (teste, nextuvar, constr) = get_constraints [] hd in
        print_endline "*******************************************************";
        Printf.printf "--> CONSTRAINTS / expression %d  \n\n" counter;
        list_to_string constr;
        print_endline "\n--> CONSTRAINTS SOLVING (UNIFY) \n" ;
        (let tySubstitutions = unify constr in list_to_string tySubstitutions);
        print_endline "******************************************************* \n";
        p_sub_inferred_types tl (counter+1))
  | [] -> ();;

let print_sub_inferred_types exp = p_sub_inferred_types exp 1;;


let rec p_inferred_type exp counter = match exp with
  | (hd::tl) ->
    (match hd with
      | head -> let tyT = typeInfer [] hd in
        print_endline "----------------------------------------";
        Printf.printf "---> TYPE INFERENCE / expression %d \n" counter;
        print_endline (type_to_string tyT);
        print_endline "---------------------------------------- \n";
        p_inferred_type tl (counter+1))
  | [] -> ();;

let print_inferred_types exp = p_inferred_type exp 1;;