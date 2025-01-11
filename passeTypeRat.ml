(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

let rec analyser_type_affectable a = 
  match a with
  | AstTds.Ident(info) -> 
    begin
        match info_ast_to_info info with
            | InfoVar (_,t,_,_) -> (AstType.Ident info, t)
            | InfoConst _ -> (AstType.Ident info, Int)
            | _ -> failwith "Erreur"
    end
  | AstTds.Deref(aff) -> 
    begin
        match analyser_type_affectable aff with
            | (naff, Pointeur(t)) -> (AstType.Deref (naff, t), t)
            | (_, t) -> raise (TypeInattendu(t, Pointeur(Undefined)))
end

let rec analyse_type_expression e = match e with
  | AstTds.AppelFonction (info, le) -> 
    begin
      match info_ast_to_info info with
        | InfoFun (_, tr, tp) -> 
          let l = List.map analyse_type_expression le in
          let np = List.map fst l in
          let ntp = List.map snd l in
          if List.length tp = List.length ntp && List.for_all2 (fun t te -> est_compatible t te) tp ntp then
            AstType.AppelFonction (info, np),tr
          else
            raise (TypesParametresInattendus (tp, ntp))
        | _ -> failwith "La passe Tds est mal faite"
    end
  | AstTds.Booleen b -> AstType.Booleen b, Bool
  | AstTds.Entier i -> AstType.Entier i, Int
  | AstTds.Unaire (op, e) -> 
    begin
      let (me,te) = analyse_type_expression e in
      match te,op with
      | Rat, AstSyntax.Numerateur -> AstType.Unaire (AstType.Numerateur, me), Int
      | Rat, AstSyntax.Denominateur -> AstType.Unaire (AstType.Denominateur, me), Int
      | _ -> raise (TypeInattendu (te, Rat))
    end
  | AstTds.Binaire (op, e1, e2) ->
    begin
      let (me1,te1) = analyse_type_expression e1 in
      let (me2,te2) = analyse_type_expression e2 in
      match te1,op,te2 with
      | Int, Plus, Int -> AstType.Binaire (PlusInt, me1, me2), Int
      | Int, Fraction, Int -> AstType.Binaire (Fraction, me1, me2), Rat
      | Int, Mult, Int -> AstType.Binaire (MultInt, me1, me2), Int
      | Int, Equ, Int -> AstType.Binaire (EquInt, me1, me2), Bool
      | Int, Inf, Int -> AstType.Binaire (Inf, me1, me2), Bool
      
      | Rat, Plus, Rat -> AstType.Binaire (PlusRat, me1, me2), Rat
      | Rat, Mult, Rat -> AstType.Binaire (MultRat, me1, me2), Rat
      
      | Bool, Equ, Bool -> AstType.Binaire (EquBool, me1, me2), Bool

      | _ -> raise (TypeBinaireInattendu (op, te1, te2))
    end
  | AstTds.Affectable a -> let (na,t) = analyser_type_affectable a in (AstType.Affectable na, t)
  | AstTds.Adresse info -> 
    begin
      match info_ast_to_info info with
        | InfoVar (_,t,_,_) -> (AstType.Adresse info, Pointeur(t))
        | _ -> failwith "Erreur passe Tds"
    end
  | AstTds.New t -> (AstType.New t, Pointeur(t))
  | AstTds.Null -> (AstType.Null, Undefined)

let rec analyse_type_instruction i = 
  match i with
  | AstTds.Declaration (t, info, e) -> 
      let (me,te) = analyse_type_expression e in
      if est_compatible t te then
        ( modifier_type_variable t info;
        AstType.Declaration (info, me))
      else
        raise (TypeInattendu (te, t))
  | AstTds.Affectation (a, e) ->
      let (na,ta) = analyser_type_affectable a in
      let (ne,te) = analyse_type_expression e in
      if (est_compatible ta te) then 
        AstType.Affectation(na, ne)
      else 
        raise (TypeInattendu(te, ta))
  | AstTds.Affichage(e) -> 
      let (me,te) = analyse_type_expression e in
      begin
        match te with
        | Int -> AstType.AffichageInt me
        | Bool -> AstType.AffichageBool me
        | Rat -> AstType.AffichageRat me
        | _ -> raise (TypeInattendu (te, Int))
      end
  | AstTds.Conditionnelle(c, t, e) -> 
      let (me,te) = analyse_type_expression c in
      if te = Bool then
        let nt = analyse_type_bloc t in
        let ne = analyse_type_bloc e in
        AstType.Conditionnelle (me, nt, ne)
      else
        raise (TypeInattendu (te, Bool))
  | AstTds.TantQue (c, b) -> 
      let (me,te) = analyse_type_expression c in
      if te = Bool then
        AstType.TantQue (me, analyse_type_bloc b)
      else
        raise (TypeInattendu (te, Bool))
  |AstTds.Retour (e,info) -> 
    let (ne, te) = analyse_type_expression e in
      begin
      match info_ast_to_info info with
        | InfoFun(_, t, _) -> 
          if est_compatible t te then
            AstType.Retour(ne, info)
          else
            raise (TypeInattendu (te, t))
        | _ -> failwith("Cas impossible")
      end
  | AstTds.Static(t, info, e ) -> 
    let (ne, te) = analyse_type_expression e in
    if est_compatible t te then
      ( modifier_type_variable t info;
      AstType.Static(info , ne ))
    else
      raise (TypeInattendu (te, t))
  | AstTds.Empty -> AstType.Empty
and analyse_type_bloc li = List.map analyse_type_instruction li


let analyse_type_fonction (AstTds.Fonction(t,info,lp,bloc)) = 
  let ltype = List.map fst lp in
  let linfo = List.map (fun (t, infop) -> modifier_type_variable t infop; infop) lp in
  modifier_type_fonction t ltype info;
  let nbloc = analyse_type_bloc bloc in
  AstType.Fonction(info, linfo, nbloc)

let analyse_type_fonctions lf = List.map analyse_type_fonction lf


let analyse_type_var (AstTds.Var (t, info, e)) = 
  let (me,te) = analyse_type_expression e in 
  if est_compatible t te then
    (modifier_type_variable t info;
    AstType.Var (info, me))
  else
    raise (TypeInattendu (te, t))

let annalyse_type_vars lv = List.map analyse_type_var lv

(* analyser : AstType.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (var,fonctions,prog)) =
  let nv = annalyse_type_vars var in
  
  AstType.Programme(nv,analyse_type_fonctions fonctions, analyse_type_bloc prog)
