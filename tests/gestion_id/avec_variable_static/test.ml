open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_variable_static/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testSujet" = 
  let _ = compiler (pathFichiersRat^"testSujet.rat") in ()


let%test_unit "testDoubleDeclaration" = 
  try 
    let _ = compiler (pathFichiersRat^"testDoubleDeclaration.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("i") -> ()  

let%test_unit "testStaticHorsFonction" = 
  try 
    let _ = compiler (pathFichiersRat^"testStaticHorsFonction.rat")
    in raise ErreurNonDetectee
  with
  | StaticHorsFonction -> ()  


let%test_unit "testSansErreur" = 
  let _ = compiler (pathFichiersRat^"testSansErreur.rat") in ()

let%test_unit "testXGlobale" = 
  let _ = compiler (pathFichiersRat^"testXGlobale.rat") in ()

let%test_unit "testNonLiteral" =
  try 
    let _ = compiler (pathFichiersRat^"testNonLiteral.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare "a" -> ()