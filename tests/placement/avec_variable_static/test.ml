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

let pathFichiersRat = "../../../../../tests/placement/avec_variable_static/"

(**********)
(*  TESTS *)
(**********)

(*deplacement par rapport à la variable z qui est de 1*)
let%test "testSujet_z" = 
  test (pathFichiersRat^"testSujet.rat")  "main" ("z",1)  (1,"SB")

(*deplacement par rapport à la variable z qui est de 1*)
let%test "testSujet_x" = 
  test (pathFichiersRat^"testSujet.rat")  "main" ("x",1)  (2,"SB")

(*deplacement par rapport à la variable z qui est de 1*)
let%test "test1_z" = 
  test (pathFichiersRat^"testSujet.rat")  "main" ("z",1)  (1,"SB")

(*deplacement par rapport à la variable z qui est de 1*)
let%test "test1_y" = 
  test (pathFichiersRat^"test1.rat")  "f" ("y",1)  (3,"LB")

(*deplacement par rapport à la variable z qui est de 1*)
let%test "test1_p" = 
  test (pathFichiersRat^"test1.rat")  "main" ("p",1)  (2,"SB")

(*deplacement par rapport à la variable z qui est de 1*)
let%test "test1_b" = 
  test (pathFichiersRat^"test1.rat")  "f" ("b",1)  (4,"LB")


(*deplacement par rapport à la variable z qui est de 1*)
let%test "test1_x" = 
  test (pathFichiersRat^"test1.rat")  "main" ("x",1)  (3,"SB")