open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_param_defaut/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testSujet" = 
  let _ = compiler (pathFichiersRat^"testSujet.rat") in ()


let%test_unit "testDefautMilieu" = 
  try 
    let _ = compiler (pathFichiersRat^"testDefautMilieu.rat")
    in raise ErreurNonDetectee
  with
  | ParametreDefautNonDernier _ -> ()

let%test_unit "testDoubleDeclarationParamDefaut" = 
  try 
    let _ = compiler (pathFichiersRat^"testDoubleDeclarationParamDefaut.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration "b" -> ()