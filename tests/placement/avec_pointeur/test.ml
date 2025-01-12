open Rat
open Compilateur
open Passe

(* Return la liste des adresses des variables d'un programme RAT *)
let getListeDep ratfile =
  let input = open_in ratfile in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    let past = CompilateurRat.calculer_placement ast in
    let listeAdresses = VerifPlacement.analyser past in
    listeAdresses
  with
  | Lexer.Error _ as e ->
      report_error ratfile filebuf "lexical error (unexpected character).";
      raise e
  | Parser.Error as e->
      report_error ratfile filebuf "syntax error.";
      raise e

(* teste si dans le fichier fichier, dans la fonction fonction (main pour programme principal)
la occ occurence de la variable var a l'adresse dep[registre]
*)
let test fichier fonction (var,occ) (dep,registre) = 
  let l = getListeDep fichier in
  let lmain = List.assoc fonction l in
  let rec aux i lmain = 
    if i=1 
    then
      let (d,r) = List.assoc var lmain in
      (d=dep && r=registre)
    else 
      aux (i-1) (List.remove_assoc var lmain)
  in aux occ lmain

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/placement/avec_pointeur/"



let%test "testPointeur1_f_a" = 
  test (pathFichiersRat^"testPointeur1.rat")  "f" ("a",1)  (-1, "LB")

let%test "testPointeur2_f_a" = 
  test (pathFichiersRat^"testPointeur2.rat")  "f" ("a",1)  (-1, "LB")

let%test "testPointeur3_f_b" = 
  test (pathFichiersRat^"testPointeur3.rat")  "f" ("b",1)  (-4, "LB")
    
let%test "testPointeur3_f_r" = 
  test (pathFichiersRat^"testPointeur3.rat")  "f" ("r",1)  (-3, "LB")
    
let%test "testPointeur3_f_i" = 
  test (pathFichiersRat^"testPointeur3.rat")  "f" ("i",1)  (-1, "LB")

let%test "testPointeur4_x" = 
  test (pathFichiersRat^"testPointeur4.rat")  "main" ("x",1)  (0, "SB")

let%test "testPointeur4_y" = 
  test (pathFichiersRat^"testPointeur4.rat")  "main" ("y",1)  (2, "SB")

let%test "testPointeur4_z" = 
  test (pathFichiersRat^"testPointeur4.rat")  "main" ("z",1)  (3, "SB")

let%test "testGeneral_x" = 
  test (pathFichiersRat^"testGeneral.rat")  "main" ("x",1)  (0, "SB")

let%test "testGeneral_y" = 
  test (pathFichiersRat^"testGeneral.rat")  "main" ("y",1)  (1, "SB")

let%test "testGeneral_z" = 
  test (pathFichiersRat^"testGeneral.rat")  "main" ("z",1)  (2, "SB")