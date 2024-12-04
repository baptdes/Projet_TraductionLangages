open Tds
open Exceptions
open Ast

type t1 = Ast.AstTypePlacement.programme
type t2 = string

(*AstPlacement.expression -> string *)
let rec analyse_code_expression e = match e with
  | AppelFonction (id,le) -> failwith "todo"
  | Ident (info) -> 
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> Tam.load (getTaille t) dep reg
        | InfoConst(_,c) -> Tam.loadl_int c
        | _ -> failwith "Problème dans la passe Tds"
    end
  | Booleen b -> failwith "todo"
  | Entier i -> failwith "todo"
  | Unaire (op,e) -> 
    let ne = analyse_code_expression e1 in
    begin
      match op with
        | Numerateur -> ne ^ Tam.pop(0) 1
        | Denominateur -> ne ^ Tam.pop(1) 1
    end
  | Binaire (op,e1,e2) -> failwith "todo"

(* AstPlacement.instruction -> String *)
let rec analyse_code_instruction i = match i with
  | Declaration(info,e) -> 
    let ne = analyse_code_expression e in
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> 
          Tam.push (getTaille t) ^ ne ^ Tam.store (getTaille t) dep reg;
        | _ -> failwith "Problème dans la passe Tds"
    end
  | Affectation(info,e) -> 
    let ne = analyse_code_expression e in
    begin
      match info_ast_to_info info with
        | InfoVar(_,t,dep,reg) -> 
          ne ^ Tam.store (getTaille t) dep reg;
        | _ -> failwith "Problème dans la passe Tds"
    end
  | TantQue (c,b) -> 
    let nc = analyse_code_expression c in
    let nb = analyse_code_bloc b in
    let debut = getEtiquette() in
    let fin = getEtiquette() in
    Tam.label debut ^ nc ^ Tam.jumpif 0 fin ^ nb ^ Tam.jump debut ^ Tam.label fin
  | Conditionnelle (c,t,e) -> failwith "todo"
  | Retour e -> failwith "todo"
  | AffichageInt e -> failwith "todo"
  | AffichageRat e -> failwith "todo"
  | AffichageBool e -> failwith "todo"
  | Empty -> ""

(* AstPlacement.bloc -> String *)
and analyse_code_bloc b = failwith "todo"

(* AstPlacement.fonction -> String *)
let analyse_code_fonction f = failwith "todo"

(* AstPlacement.programme -> String *)
let analyser (Programme (fonction,prog)) = failwith "todo"