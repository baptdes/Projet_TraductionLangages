(* Module de la passe du placement mémoire *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

let analyse_placement_variable_statique (info_fun) depl reg = 
  
  match info_fun with 
  | InfoFun(_,_,_,lv,b) ->
      if b then 
        let taille = List.fold_left (fun acc infoVar -> 
                   
                    modifier_adresse_variable depl reg (info_to_info_ast infoVar);

                   
                    match infoVar with
                        | InfoVar(_,t,_,_) -> acc + getTaille t
                        | InfoConst _ -> failwith "erreur analyse placement"
                        | InfoFun _ -> failwith "erreur analyse placement" ) 0 lv in                                                                                                    
        
        print_endline "ici";
        modifier_bool_fun false (info_to_info_ast info_fun);
        match info_fun with
          | InfoFun(_,_,_,_,b) -> print_endline (string_of_bool b);
                        taille
      else  
        0 
      
  | _ -> failwith "erreur anaylse placement"

(* AstType.expression -> int -> string -> AstPlacement.expression * int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre depl : le déplacement courant *)
(* Paramètre reg : le registre courant *)
(* Renvoie l'instruction i avec les déplacements mémoire mis à jour *)
let analyse_placement_expression i depl reg = match i with 
    | AstType.AppelFonction(info,_) -> analyse_placement_variable_statique (info_ast_to_info info) depl reg
    | _ -> 0

(* AstType.instruction -> int -> string -> AstPlacement.instruction * int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre depl : le déplacement courant *)
(* Paramètre reg : le registre courant *)
(* Renvoie l'instruction i avec les déplacements mémoire mis à jour *)
let rec analyse_placement_instruction i depl reg = 
  match i with
  | AstType.Declaration (info,e) -> 
    let taille = analyse_placement_expression e depl reg in 
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,_,_) -> modifier_adresse_variable (depl+taille) reg info;
          (AstPlacement.Declaration(info,e), taille + getTaille t)
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
    let taille = analyse_placement_expression e depl reg in
    begin
      match info_ast_to_info ia with
        | InfoFun(_,t,lpt,_,_) -> 
            let tailleParametre = List.fold_left (fun acc t -> acc + getTaille t) 0 lpt in
            AstPlacement.Retour(e, taille + getTaille t, tailleParametre), 0
        | _ -> failwith "La passe Tds est mal faite"
    end
  | AstType.Affectation (ia,e) -> let taille = analyse_placement_expression e depl reg in (AstPlacement.Affectation(ia,e), taille)
  | AstType.AffichageInt e -> let taille = analyse_placement_expression e depl reg in (AstPlacement.AffichageInt e, taille)
  | AstType.AffichageRat e -> let taille = analyse_placement_expression e depl reg in (AstPlacement.AffichageRat e, taille)
  | AstType.AffichageBool e -> let taille = analyse_placement_expression e depl reg in (AstPlacement.AffichageBool e, taille)
  | AstType.Static(info, e, info_fun)  -> modifier_var_static (info_ast_to_info info) (info_fun);   (AstPlacement.Static(info, e, info_fun),0)                      
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

  
let analyse_placement_var  (AstType.Var (info, e)) depl =
  begin
    match info_ast_to_info info with
      | InfoVar(_,t,_,_) -> modifier_adresse_variable depl "SB" info;
        (AstPlacement.Var(info,e), getTaille t)
      | _ -> failwith "La passe Tds est mal faite"
  end


let rec analyse_placement_vars vars = 
  match vars with 
  | [] -> [],0
  | t::q ->let (nq,depl) = analyse_placement_vars q  in let (nv,t) = (analyse_placement_var t depl) in nv::(nq),depl+t    
  


(*AstType.programme -> AstPlacement.programme*)
let analyser (AstType.Programme(vars,fonctions,bloc)) = 
  let nv,depl = (analyse_placement_vars vars) in (* le déplacement pour ajouté les variables globales à l'avance *)
  let nlf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc bloc depl "SB" in
  AstPlacement.Programme((nv,depl),nlf,nb)