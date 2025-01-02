open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/type/avec_variable_static/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testSujet" = 
  let _ = compiler (pathFichiersRat^"testSujet.rat") in ()


let%test_unit "testMauvaisType" = 
  try 
    let _ = compiler (pathFichiersRat^"testMauvaisType.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu (Bool, Int) -> ()  

let%test_unit "testMauvaisType2" = 
  try 
    let _ = compiler (pathFichiersRat^"testMauvaisType2.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu (Bool, Int) -> ()  