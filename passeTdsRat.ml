(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* analyse_tds_affectable : tds -> AstSyntax.affectable -> bool -> AstTds.affectable*)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre a : l'affectation à analyser *)
(* Paramère modif : Booleen pour savoir si l'affectable est à gauche (true) ou si l'affectable est à droite (false) *)
(* Transforme l'expression en une expression de type AstTds.expression
et vérifie TODO... *)
(* Erreur si TODO... *)
let rec analyse_tds_affectable tds a modif = match a with
  |AstSyntax.Ident n -> 
      begin
        match (chercherGlobalement tds n) with
          |None -> raise (IdentifiantNonDeclare n)
          |Some infoast -> 
            begin
              match (info_ast_to_info infoast) with
                |InfoVar _ -> AstTds.Ident infoast
                |InfoConst (_,_)-> 
                  (* Si l'affectable est à gauche, on ne peut pas avoir de constante*)
                  if modif then 
                    raise (MauvaiseUtilisationIdentifiant n)
                  (* Si l'affectable est à droite, tout est OK*)
                  else
                    AstTds.Ident(infoast)
                |InfoFun _ -> raise (MauvaiseUtilisationIdentifiant n)
            end
      end
  |AstSyntax.Deref a -> 
    let nv = analyse_tds_affectable tds a modif in
    AstTds.Deref(nv)

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e (tds_main, dico_defaut)= match e with
  | AstSyntax.AppelFonction (id,le) -> 
    begin
      match (chercherGlobalement tds id) with
        |None -> raise (IdentifiantNonDeclare id)
        |Some infoast -> 
          begin
          match (info_ast_to_info infoast) with
            |InfoFun _ -> 
              let analyse_args = List.map (fun e -> analyse_tds_expression tds e (tds_main, dico_defaut)) le in
              let l_defaut = Hashtbl.find dico_defaut id in
              let liste_args_completee = completer_arguments tds_main dico_defaut analyse_args l_defaut in
              AstTds.AppelFonction (infoast, liste_args_completee)
            |_ -> raise (MauvaiseUtilisationIdentifiant id)
          end
    end
  | AstSyntax.Binaire (op, e1, e2) ->
      let ne1 = analyse_tds_expression tds e1 (tds_main, dico_defaut) in
      let ne2 = analyse_tds_expression tds e2 (tds_main, dico_defaut) in
      AstTds.Binaire (op, ne1, ne2)
  | AstSyntax.Unaire (op, e) ->
      let ne = analyse_tds_expression tds e (tds_main, dico_defaut) in
      AstTds.Unaire (op, ne)
  | AstSyntax.Entier i -> AstTds.Entier i
  | AstSyntax.Booleen b -> AstTds.Booleen b
  | AstSyntax.Affectable a ->  AstTds.Affectable (analyse_tds_affectable tds a false)
  | Adresse id ->
    begin 
      match (chercherGlobalement tds id) with
        |None -> raise (IdentifiantNonDeclare id)
        |Some infoast -> 
          begin
          match (info_ast_to_info infoast) with
            |InfoVar _ -> AstTds.Adresse infoast
            |_ -> raise (MauvaiseUtilisationIdentifiant id)
          end
    end
  | AstSyntax.New t -> AstTds.New t
  | AstSyntax.Null -> AstTds.Null

and completer_arguments tds_main dico_defaut args l_defaut =
  match (l_defaut, args) with
    | [], [] -> []
    | (Some (AstSyntax.Defaut(e)))::q, [] ->
        (analyse_tds_expression tds_main e (tds_main, dico_defaut)) :: completer_arguments tds_main dico_defaut [] q
    | None :: q, [] -> completer_arguments tds_main dico_defaut [] q (* On continue de remplir même si le nombre d'arguments est faux*)
    | _:: q, arg :: args_q ->
        arg :: completer_arguments tds_main dico_defaut args_q q
    | [], t :: q -> t :: completer_arguments tds_main dico_defaut q []

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i (tds_main, dico_defaut) =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e (tds_main, dico_defaut) in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) -> 
      AstTds.Affectation (analyse_tds_affectable tds a true, analyse_tds_expression tds e (tds_main, dico_defaut))
  | AstSyntax.Constante (n,v) ->
      begin
        begin
        match chercherGlobalement tds n with 
        | Some _ -> raise(DoubleDeclaration n)
        | None ->       
          match chercherLocalement tds n with
          | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
              il n'a donc pas été déclaré dans le bloc courant *)
            (* Ajout dans la tds de la constante *)
            ajouter tds n (info_to_info_ast (InfoConst (n,v)));
            (* Suppression du noeud de déclaration des constantes devenu inutile *)
            AstTds.Empty
          | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
        end
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e (tds_main, dico_defaut) in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c (tds_main, dico_defaut) in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia t (tds_main, dico_defaut) in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e (tds_main, dico_defaut) in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c (tds_main, dico_defaut) in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b (tds_main, dico_defaut) in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *) 
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e (tds_main, dico_defaut) in
        AstTds.Retour (ne,ia)
      end
  | AstSyntax.Static(t,n,e) -> 
    begin
      (* On récupère l'information associée à la fonction à laquelle la variable statique est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *) 
      | None -> raise StaticHorsFonction
        (* Il y a une information -> la variable statique est dans une fonction *)
      | Some _ ->
        begin
          match chercherLocalement tds n with
          | None ->
              (* On autorise seulement les literaux et les variables globales dans les variables static locale comme dans le langage C*)
              (* Ainsi, on analyse l'expression seulement dans la tds_main où il y a les variables globales*)
              let ne = analyse_tds_expression tds_main e (tds_main, dico_defaut) in
              let info = InfoVar (n,Undefined, 0, "") in
              let iap = info_to_info_ast info in
              ajouter tds n iap;
              AstTds.Static (t, iap, ne)
          | Some _ ->
              raise (DoubleDeclaration n)
        end
    end


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li (tds_main, dico_defaut) =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (fun i -> analyse_tds_instruction tdsbloc oia i (tds_main, dico_defaut)) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli

(* get_liste_param_defaut : (typ * string * AstSyntax.defaut option) list -> bool -> AstSyntax.defaut option list *)
(* Paramètre params : la liste des paramètres à analyser *)
(* Paramètre defaut_present : booléen indiquant si un paramètre par défaut a déjà été rencontré *)
(* Vérifie la bonne utilisation des paramètres par défaut et renvoie la liste des paramètres par défaut *)
(* Erreur si mauvaise utilisation des paramètres par défaut *)
let rec get_liste_param_defaut params defaut_present =
  match params with
    | [] -> []
    | (_,_,Some d )::q -> (Some d)::(get_liste_param_defaut q true)
    | (_,p,None)::q -> 
      if defaut_present then 
        (* Si un paramètre par défaut a déjà été rencontré, il ne peut y avoir de paramètre normal*)
        raise (Exceptions.ParametreDefautNonDernier p)
      else 
        None::(get_liste_param_defaut q false)

(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds dico_defaut (AstSyntax.Fonction(t,n,lp,li)) =
  match chercherGlobalement maintds n with
  | Some _ -> raise (DoubleDeclaration n)
  | None ->
    (*Récupération des paramètres par default*)
    let l_defaut = get_liste_param_defaut lp false in
    Hashtbl.add dico_defaut n l_defaut;

    (*Gestion de la déclaration de la fonction*)
    let info = InfoFun (n, t, List.map (fun (t, _, _) -> t) lp) in
    let info_ast_fonction = info_to_info_ast info in
    ajouter maintds n info_ast_fonction;

    (*Gestion des arguments*)
    let tds = creerTDSFille maintds in
    let nlp = List.map (fun (typ, nom, _) ->
      begin
        match chercherLocalement tds nom with
        | Some _ -> raise (DoubleDeclaration nom)
        | None ->
          let info = InfoVar (nom, typ, 0, "") in
          let info_ast_arg = info_to_info_ast info in
          ajouter tds nom info_ast_arg;
          (typ, info_ast_arg)
      end
    ) lp in
    let nli = analyse_tds_bloc tds (Some info_ast_fonction) li (maintds, dico_defaut) in
    AstTds.Fonction(t, info_ast_fonction, nlp, nli)

(* analyser : AstSyntax.var -> AstTds.var*)
(* Paramètre : la variable globale  à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let annalyse_tds_var tds dico_defaut (AstSyntax.Var (t, n, e)) =
  begin
    match chercherLocalement tds n with
    | None ->
        (* L'identifiant n'est pas trouvé dans la tds locale,
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Vérification de la bonne utilisation des identifiants dans l'expression *)
        (* et obtention de l'expression transformée *)
        let ne = analyse_tds_expression tds e (tds, dico_defaut) in
        (* Création de l'information associée à l'identfiant *)
        let info = InfoVar (n,Undefined, 0, "") in
        (* Création du pointeur sur l'information *)
        let ia = info_to_info_ast info in
        (* Ajout de l'information (pointeur) dans la tds *)
        ajouter tds n ia;
        (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
        et l'expression remplacée par l'expression issue de l'analyse *)
        AstTds.Var (t, ia, ne)
        
    | Some _ ->
        (* L'identifiant est trouvé dans la tds locale,
        il a donc déjà été déclaré dans le bloc courant *)
        raise (DoubleDeclaration n)
    end


(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (varStatic,fonctions,prog)) =
  let tds = creerTDSMere () in
  let dico_defaut = Hashtbl.create 100 in
  let nv = List.map (annalyse_tds_var tds dico_defaut) varStatic in 
  let nf = List.map (analyse_tds_fonction tds dico_defaut) fonctions in
  let nb = analyse_tds_bloc tds None prog (tds,dico_defaut) in
  AstTds.Programme (nv,nf,nb)
