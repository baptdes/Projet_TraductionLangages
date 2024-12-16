(*(* Module de la passe du placement mémoire *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type

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
          (AstPlacement.Declaration(info,e), getTaille t)
        | _ -> failwith "La passe Tds est mal faite"
    end
  | AstType.Conditionnelle (c,t,e) -> 
    let nt = analyse_placement_bloc t depl reg in
    let ne = analyse_placement_bloc e depl reg in
    (AstPlacement.Conditionnelle(c,nt,ne), 0)
  | AstType.TantQue (c,b) -> 
    let nb = analyse_placement_bloc b depl reg in
    (AstPlacement.TantQue(c,nb), 0)
  | AstType.Retour (e,ia) -> 
    begin
      match info_ast_to_info ia with
        | InfoFun(_,t,lpt) -> 
            let tailleParametre = List.fold_left (fun acc t -> acc + getTaille t) 0 lpt in
            AstPlacement.Retour(e, getTaille t, tailleParametre), 0
        | _ -> failwith "La passe Tds est mal faite"
    end
  | AstType.Affectation (ia,e) -> AstPlacement.Affectation(ia,e), 0
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  | AstType.Empty -> (AstPlacement.Empty, 0)

(* AstType.bloc -> int -> string -> AstPlacement.bloc * int *)
(* Paramètre li : le bloc à analyser *)
(* Paramètre depl : le déplacement courant *)
(* Paramètre reg : le registre courant *)
(* Renvoie le bloc li avec les déplacements mémoire mis à jour *)
and analyse_placement_bloc li depml reg = match li with
  | [] -> ([], 0)
  | t::q -> let (ni,ti) = analyse_placement_instruction t depml reg in
            let (nq,tq) = analyse_placement_bloc q (depml + ti) reg in
            (ni::nq, ti + tq)


(*AstType.fonction -> AstPlacement.fonction*)
let analyse_placement_fonction (AstType.Fonction(info,lp,li)) = 
  let rec placer_parametres lp depl = match lp with
    |[] -> ()
    |tInfo::q -> 
      begin
        match info_ast_to_info tInfo with
          | InfoVar(_,t,_,_) -> 
            let new_depl = depl - getTaille t in
            modifier_adresse_variable new_depl "LB" tInfo;
            placer_parametres q new_depl
          | _ -> failwith "La passe Tds est mal faite"
      end
  in
  placer_parametres (List.rev lp) 0;
  let nb = analyse_placement_bloc li 3 "LB" in
  AstPlacement.Fonction(info,lp,nb)

(*AstType.programme -> AstPlacement.programme*)
let analyser (AstType.Programme(fonctions,bloc)) = 
  let nlf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc bloc 0 "SB" in
  AstPlacement.Programme(nlf,nb)*)