open Tds
open Exceptions
open Ast
open Type
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

(*AstPlacement.expression -> string *)
let rec analyse_code_expression e = match e with
  | AstType.AppelFonction (id,le) -> failwith "todo"
  | AstType.Ident (info) -> 
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> Tam.load (getTaille t) dep reg
        | InfoConst(_,v) -> Tam.loadi v
        | _ -> failwith "Problème dans la passe Tds"
    end
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
      | Fraction -> ""
      | EquInt | EquBool -> Tam.subr "IEq"
      | Inf -> Tam.subr "ISub"
      end

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
  | AstPlacement.Affectation(info,e) -> 
    let ne = analyse_code_expression e in
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> 
          ne ^ Tam.store (getTaille t) dep reg;
        | _ -> failwith "Problème dans la passe Tds"
    end
  | AstPlacement.TantQue (c,b) -> 
    let nc = analyse_code_expression c in
    let nb = analyse_code_bloc b in
    let debut = getEtiquette() in
    let fin = getEtiquette() in
    Tam.label debut ^ nc ^ Tam.jumpif 0 fin ^ nb ^ Tam.jump debut ^ Tam.label fin
  | AstPlacement.Conditionnelle (c,t,e) -> failwith "todo"
  | AstPlacement.Retour (_,_,_) -> failwith "todo"
  | AstPlacement.AffichageInt e -> 
    let ne = analyse_code_expression e in
    ne ^ Tam.subr "IOut"
  | AstPlacement.AffichageRat e -> 
    let ne = analyse_code_expression e in
    ne ^ Tam.subr "ROut"
  | AstPlacement.AffichageBool e -> 
    let ne = analyse_code_expression e in
    ne ^ Tam.subr "BOut"
  | AstPlacement.Empty -> ""

(* AstPlacement.bloc -> String *)
and analyse_code_bloc (li,taille) = 
  Tam.push taille ^
  List.fold_left (fun acc i -> acc ^ analyse_code_instruction i) "" li

(* AstPlacement.fonction -> String *)
let analyse_code_fonction f = ""

(* AstPlacement.programme -> String *)
let analyser (AstPlacement.Programme(fonctions,prog)) = 
  List.fold_left (fun acc i -> acc ^ analyse_code_fonction i) "" fonctions ^
  analyse_code_bloc prog 