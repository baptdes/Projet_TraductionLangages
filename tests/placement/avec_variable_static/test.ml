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
let%test "test1_z" = 
  test (pathFichiersRat^"test1.rat")  "main" ("z",1)  (0,"SB")


(*deplacement par rapport à la variable i qui est de 0*)
let%test "test1_i" = 
  test (pathFichiersRat^"test1.rat")  "f" ("i",1)  (1,"SB")


(*deplacement par rapport à la variable y qui est de 1*)
let%test "test1_y" = 
  test (pathFichiersRat^"test1.rat")  "f" ("y",1)  (0,"LB")


(*deplacement par rapport à la variable z qui est de 1*)
let%test "testSujet_p" = 
  test (pathFichiersRat^"testSujet.rat")  "main" ("p",1)  (1,"SB")

(*deplacement par rapport à la variable z qui est de 1*)
let%test "testSujet_y" = 
  test (pathFichiersRat^"testSujet.rat")  "f" ("i",1)  (0,"SB")