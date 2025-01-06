open Tds
open Ast
open Type
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

(* deref : true si on a déjà effectué un déréférencement, false sinon *)
let rec analyse_code_affectable a modif deref = match a with
  | AstType.Ident(info) -> 
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> 
          if (modif && not deref) then (Tam.store (getTaille t) dep reg)
          else (Tam.load (getTaille t) dep reg)
        | InfoConst(_,v) -> Tam.loadl_int v
        | _ -> failwith "Problème dans la passe Tds"
    end
  | AstType.Deref(aff,t) -> 
      let taille = getTaille t in
      let action = 
        if (modif && not deref) then
          (Tam.storei taille)
        else
          (Tam.loadi taille)
      in
      (analyse_code_affectable aff modif true) ^ action

(*AstPlacement.expression -> string *)
let rec analyse_code_expression e = match e with
  | AstType.Affectable(a) ->
    analyse_code_affectable a false false
  | AstType.AppelFonction (info,le) -> 
    (*Récupérer le nom de la fonction*)
    let id = match info_ast_to_info info with
      | InfoFun(id,_,_,_,_) -> id
      | _ -> failwith "Problème dans la passe Tds"
    in
    (List.fold_right (fun t acc -> (analyse_code_expression t) ^ acc) le "") ^ (Tam.call "SB" id)
  | AstType.Booleen b -> Tam.loadl_int (if b then 1 else 0)
  | AstType.Entier i -> Tam.loadl_int i
  | AstType.Unaire (op,e) -> 
    let ne = analyse_code_expression e in
    begin
      match op with
        | Numerateur -> ne ^ Tam.pop 0 1
        | Denominateur -> ne ^ Tam.pop 1 1
    end
  | AstType.Binaire (op,e1,e2) -> 
    analyse_code_expression e1 ^ analyse_code_expression e2 ^
    begin
      match op with
      | PlusInt -> Tam.subr "IAdd"
      | PlusRat -> Tam.call "ST" "RAdd"
      | MultInt -> Tam.subr "IMul"
      | MultRat -> Tam.call "ST" "RMul"
      | Fraction -> Tam.call "ST" "norm"
      | EquInt | EquBool -> Tam.subr "IEq"
      | Inf -> Tam.subr "ISub"
      end
  | AstType.New t ->
    let taille = getTaille t in
    Tam.loadl_int taille ^ Tam.subr "MAlloc"
  | AstType.Adresse ia ->
    begin
      match info_ast_to_info ia with
        | InfoVar(_,_,dep,reg) -> Tam.loada dep reg
        | _ -> failwith "Problème dans la passe Tds"
    end
  | AstType.Null ->
    Tam.loadl_int 0 (*Null = 0 en mémoire*)

(* AstPlacement.instruction -> String *)
let rec analyse_code_instruction i = match i with
  | AstPlacement.Declaration(info,e) -> 
    let ne = analyse_code_expression e in
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> 
          Tam.push (getTaille t) ^ ne ^ Tam.store (getTaille t) dep reg;
        | _ -> failwith "Problème dans la passe Tds"
    end
  | AstPlacement.Affectation(a,e) -> analyse_code_expression e ^ analyse_code_affectable a true false
  | AstPlacement.TantQue (c,b) -> 
    let nc = analyse_code_expression c in
    let nb = analyse_code_bloc b in
    let debut = getEtiquette() in
    let fin = getEtiquette() in
    Tam.label debut ^ nc ^ Tam.jumpif 0 fin ^ nb ^ Tam.jump debut ^ Tam.label fin
  | AstPlacement.Conditionnelle (c,t,e) -> 
      let nc = analyse_code_expression c in 
      let nt = analyse_code_bloc t in
      let ne = analyse_code_bloc e in
      let toElse = getEtiquette() in
      let fin = getEtiquette() in
      nc ^ Tam.jumpif 0 toElse ^ nt ^ Tam.jump fin ^ Tam.label toElse ^ ne ^ Tam.label fin
  | AstPlacement.Retour (e,tretour,tparams) -> (analyse_code_expression e) ^ (Tam.return tretour tparams) 
  | AstPlacement.AffichageInt e -> 
    let ne = analyse_code_expression e in
    ne ^ Tam.subr "IOut"
  | AstPlacement.AffichageRat e -> 
    let ne = analyse_code_expression e in
    ne ^ Tam.call "ST" "ROut"
  | AstPlacement.AffichageBool e -> 
    let ne = analyse_code_expression e in
    ne ^ Tam.subr "BOut"
  | AstPlacement.Empty -> ""

(* AstPlacement.bloc -> String *)
and analyse_code_bloc (li,taille) = 
  Tam.push taille ^
  List.fold_left (fun acc i -> acc ^ analyse_code_instruction i) "" li ^ (Tam.pop 0 taille)

(* AstPlacement.fonction -> String *)
let analyse_code_fonction (AstPlacement.Fonction(info,_,bloc)) = 
  let nom = match info_ast_to_info info with
    | InfoFun(id,_,_,_,_) -> id
    | _ -> failwith "Problème dans la passe Tds"
  in
  (Tam.label nom) ^ (analyse_code_bloc bloc) ^ Tam.halt


let annalyse_code_var (AstPlacement.Var(info,e)) = 
  let ne = analyse_code_expression e in
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> 
          Tam.push (getTaille t) ^ ne ^ Tam.store (getTaille t) dep reg;
        | _ -> failwith "Problème dans la passe Tds"
    end


(* AstPlacement.programme -> String *)
let analyser (AstPlacement.Programme((vars,depl),fonctions,prog)) = 
  let n = Code.getEntete() in
  let nv = List.fold_left (fun acc i -> acc ^ (annalyse_code_var i)) "" vars in 
  let nf = List.fold_left (fun acc i -> acc ^ (analyse_code_fonction i)) "" fonctions in
  let (np,nt) = prog in  
  n ^ nf ^ Tam.label "main" ^ nv ^ analyse_code_bloc (np, nt + depl ) ^ Tam.halt