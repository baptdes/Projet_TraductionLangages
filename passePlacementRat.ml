(* Module de la passe du placement mémoire *)
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
        | InfoVar(_,t,_,_) -> modifier_adresse_variable (depl) reg info;
          (AstPlacement.Declaration(info,e),getTaille t)
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
  | AstType.Affectation (ia,e) -> (AstPlacement.Affectation(ia,e), 0)
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  | AstType.Static(_, _)  -> (AstPlacement.Empty,0)                      
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

(* AstType.instruction -> int -> int *)
(* Paramètre i : l'instruction provenant d'une fonction à analyser *)
(* Paramètre deplLB : le déplacement courant de LB*)
(* Paramère deplSB : le déplacement courant de SB*)
(* Renvoie l'instruction i avec les déplacements mémoire mis à jour (dans LB et SB)*)
let analyse_placement_instruction_fonction i deplLB deplSB = 
  match i with
    (* Cas des variables statiques *)
    | AstType.Static(info, e) -> 
        modifier_adresse_variable deplSB "SB" info;
        begin
          match info_ast_to_info info with 
            | InfoVar(_,t,_,_) -> 
              (AstPlacement.Static(info, e), 0), getTaille t
            | _ -> failwith "La passe Tds est mal faite"
        end
    | _ -> analyse_placement_instruction i deplLB "LB", 0

(*analyse_placement_bloc_fonction : AstType.instruction list -> int -> int -> (AstPlacement.instruction list * int) * (AstPlacement.instruction list * int)*)
(*Paramètre li : liste d'instructions à analyser*)
(*Paramètre deplLB : le déplacement courant de LB*)
(*Paramètre deplSB : le déplacement courant de SB*)
(*Renvoie la liste d'instructions li (sans les variables statiques) avec les déplacements mémoire mis à jour*)
(* ainsi que la liste des déclarations de variables statiques avec les déplacements mémoire mis à jour*)
let rec analyse_placement_bloc_fonction li deplLB deplSB =
  match li with
      | [] -> ([], 0), ([],0)
      | h::q ->
        (* Analyse de la 1ère instruction*)
        let ((i, tailleLB), tailleSB) = analyse_placement_instruction_fonction h deplLB deplSB in
        let ((li, tailleActuelleLB),(lstatic, tailleActuelleSB)) = analyse_placement_bloc_fonction q (deplLB + tailleLB) (deplSB + tailleSB) in

        if tailleSB <> 0 then
          ((li, tailleLB + tailleActuelleLB), (i::lstatic, tailleSB + tailleActuelleSB))
        else
          ((i::li, tailleLB + tailleActuelleLB), (lstatic, tailleSB + tailleActuelleSB))

(*AstType.fonction -> AstPlacement.fonction * int*)
(* Paramètre AstType.fonction : Fonction à analyser *)
(* Paramètre deplSB : le déplacement courant de SB*)
(* Renvoie la fonction f analysée ainsi que les variables statiques et le déplacement qu'elles induisent dans SB *)
let analyse_placement_fonction (AstType.Fonction(info,lp,li)) deplSB = 
  (* Placement des paramètres *)
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
  let (bloc,(lv,deplSB)) = analyse_placement_bloc_fonction li 3 deplSB in
  AstPlacement.Fonction(info,lp,bloc), deplSB, lv

(* AstType.var -> int -> AstPlacement.var * int*)
(* Paramètre AstType.var : Variable à analyser *)
(* Paramètre depl : le déplacement courant *)
(* Renvoie la variable e avec le déplacement mémoire mis à jour *)
let analyse_placement_var  (AstType.Var (info, e)) depl =
  match info_ast_to_info info with
    | InfoVar(_,t,_,_) -> modifier_adresse_variable depl "SB" info;
      (AstPlacement.Var(info,e), getTaille t)
    | _ -> failwith "La passe Tds est mal faite"


(* AstType.var list -> AstPlacement.var list * int*)
(* Paramètre vars : Variables globales à analyser *)
(* Renvoie la liste des variables globales avec le déplacement totale lié à ses variables *)
let rec analyse_placement_vars vars = 
  match vars with
  | [] -> [],0
  | t::q -> 
    let (nq,depl) = analyse_placement_vars q in
    let (nv,t) = (analyse_placement_var t depl) in
    nv::(nq), depl+t


(*AstType.programme -> AstPlacement.programme*)
(* Paramètre AstType.programme : Programme à analyser *)
(* Renvoie le programme p avec les déplacements mémoire mis à jour *)
let analyser (AstType.Programme(vars,fonctions,bloc)) = 
  (* Analyse des variables globales pour connaitre leurs déplacements *)
  let nv,deplv = (analyse_placement_vars vars) in
  (* Analyse des fonctions pour connaitre le déplacement lié leurs variables statiques*)
  let nlf,depTotal,lv = List.fold_left(fun acc i -> 
    (*Accumulation des fonctions analysées, du déplacement dans SB et des variables globales*)
    let f,accdep,nlv = acc in
    let nf,depl,lv = (analyse_placement_fonction i accdep) in 
    nf::f,(accdep+depl),nlv@lv) ([],deplv,[]) fonctions
  in
  (* Décalage du bloc en fonction de la place que prennent les variables globales et statiques*)
  let nb = analyse_placement_bloc bloc depTotal "SB" in
  AstPlacement.Programme((nv,deplv),nlf,nb,lv)