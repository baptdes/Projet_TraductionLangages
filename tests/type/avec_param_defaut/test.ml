open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/type/avec_param_defaut/"

(**********)
(*  TESTS *)
(**********)


let%test_unit "testMauvaisType" = 
  try 
    let _ = compiler (pathFichiersRat^"testMauvaisType.rat")
    in raise ErreurNonDetectee
  with
  | TypesParametresInattendus _  -> ()  

let%test_unit "testMauvaisNombre" = 
  try 
    let _ = compiler (pathFichiersRat^"testMauvaisNombre.rat")
    in raise ErreurNonDetectee
  with
  | TypesParametresInattendus _  -> ()

let%test_unit "testMauvaisNombre2" = 
  try 
    let _ = compiler (pathFichiersRat^"testMauvaisNombre2.rat")
    in raise ErreurNonDetectee
  with
  | TypesParametresInattendus _  -> ()

let%test_unit "testSimple" = 
  let _ = compiler (pathFichiersRat^"testSimple.rat") in ()