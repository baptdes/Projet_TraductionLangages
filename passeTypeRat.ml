(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstType.programme

let rec analyse_tds_expression tds e = match e with
  | AstTds.AppelFonction (info, le) -> 
    begin
      match info_ast_to_info info with
        | InfoFun (_, tr, tp) -> 
          let l = List.map analyse_type_expression le in
          let np = List.map fst l in
          let ntp = List.map snd l in
          if List.for_all2 (fun t te -> est_compatible t te) tp ntp then
            [AstType.AppelFonction (info, np),tr]
          else
            raise (TypesParametresInattendus (tp, ntp))
        | _ -> raise (MauvaiseUtilisationIdentifiant id)
    end
  | AstTds.Ident info ->
  | AstTds.Booleen b ->
  | AstTds.Entier i ->
  | AstTds.Unaire (op, e) ->
  | AstTds.Binaire (op, e1, e2) ->
    let (me1,te1) = analyse_type_expression e1 in
    let (me2,te2) = analyse_type_expression e2 in
    match te1,b,te2 with
    | Int, Plus, Int -> [AstType.Binaire (op, me1, me2), Int]
    | Int, Moins, Int -> [AstType.Binaire (op, me1, me2), Int]

let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, info, e) -> 
      let (me,te) = analyse_type_expression e in
      if est_compatible t te then
        modifier_type_variable t info;
        AstType.Declaration (info, me)
  | AstTds.AstAffectation (info, e) ->
      begin
        match info_ast_to_info with
        | InfoVar (_, t, _, _) ->
          let (me,te) = analyse_type_expression e in
          if est_compatible t te then
            AstType.Affectation (info, me)
          else
            raise (TypeInattendu (t, te))
        | _ -> raise (MauvaiseUtilisationIdentifiant id)
      end
  | AstTds.Affichage(e) -> 
      let (me,te) = analyse_type_expression e in
      match te with
      | Int -> AstType.AffichageInt me
      | Bool -> AstType.AffichageBool me
      | Rat -> AstType.AffichageRat me
      | _ -> raise (TypeInattendu (Int, te)) (*TODO a vérifier*)
  | AstTds.Conditionnelle (c, t, e) -> 
      let (me,te) = analyse_type_expression c in
      if te = Bool then
        let nt = analyse_type_bloc t in
        let ne = analyse_type_bloc e in
        AstType.Conditionnelle (me, nt, ne)
      else
        raise (TypeInattendu (Bool, te)) (*TODO à vérifier le type des 2 blocs*)
  | AstTds.TantQue (c, b) -> 
      let (me,te) = analyse_type_expression c in
      if te = Bool then
        AstType.TantQue (me, analyse_type_bloc b)
      else
        raise (TypeInattendu (Bool, te))
  |AstTds.Retour e -> AstType.Retour e
  |AstTds.Empty -> AstType.Empty

and analyse_type_bloc li = List.map analyse_type_instruction li


let analyse_type_fonctions lf = failwith "TODO"

(* analyser : AstType.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstType.Programme (fonctions,prog)) =
  AstType.Programme(analyse_type_fonctions fonctions, analyse_type_bloc prog)
