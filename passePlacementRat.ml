(*(* Module de la passe du placement mémoire *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* AstType.instruction -> int -> string -> AstPlacement.instruction * int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre depl : le déplacement courant *)
(* Paramètre reg : le registre courant *)
(* Renvoie l'instruction i avec les déplacements mémoire mis à jour *)
let rec analyse_placement_instruction i depl reg = 
  match i with
  | AstType.Declaration (info,e) -> 
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,_,_) -> modifier_adresse_variable depl reg info;
          [AstPlacement.Declaration(info,e), getTaille t]
        | _ -> raise (MauvaiseUtilisationIdentifiant id)
    end
  | AstType.Conditionnelle (c,t,e) -> 
    let (nt,_) = analyse_placement_bloc t depl reg in
    let (ne,_) = analyse_placement_bloc e depl reg in
    (AstPlacement.Conditionnelle(c,nt,ne), 0)
  | AstType.TantQue (c,b) -> failwith "todo"
  | AstType.Retour (e,ia) -> failwith "todo"
  | AstType.Affectation (ia,e) -> failwith "todo"
  | AstType.AffichageInt e -> failwith "todo"
  | AstType.AffichageRat e -> failwith "todo"
  | AstType.AffichageBool e -> failwith "todo"
  | AstType.Empty -> AstPlacement.Empty,depl

(* AstType.bloc -> int -> string -> AstPlacement.bloc * int *)
(* Paramètre li : le bloc à analyser *)
(* Paramètre depl : le déplacement courant *)
(* Paramètre reg : le registre courant *)
(* Renvoie le bloc li avec les déplacements mémoire mis à jour *)
and analyse_placement_bloc li depml reg = match li with
  | [] -> [],0
  | t::q -> let (ni,ti) = analyse_placement_instruction t depml reg in
            let (nq,tq) = analyse_placement_bloc q (depml + ti) reg in
            ni::nq,ti + tq


(*AstType.fonction -> AstPlacement.fonction*)
let analyse_placement_fonction (AstType.Fonction(info,lp,li)) = failwith "todo"

(*AstType.programme -> AstPlacement.programme*)
let analyser (AstType.Programme(fonctions,bloc)) = 
  let nlf = List.map analyse_placement_fonction fonctions in
  let (nb,_) = analyse_placement_bloc bloc 0 "SB" in
  AstPlacement.Programme(nlf,nb)*)