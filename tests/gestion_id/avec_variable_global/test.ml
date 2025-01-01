open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_variable_global/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testStatic2" = 
  let _ = compiler (pathFichiersRat^"testStatic2.rat") in ()

let%test_unit "testStaticDoubleDeclaration1" = 
  try 
    let _ = compiler (pathFichiersRat^"testStaticDoubleDeclaration1.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()  

let%test_unit "testStaticDoubleDeclaration2" = 
  try 
    let _ = compiler (pathFichiersRat^"testStaticDoubleDeclaration2.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()  


let%test_unit "testSujet" = 
  let _ = compiler (pathFichiersRat^"testSujet.rat") in ()

let%test_unit "testDoubleDeclarationBloc" = 
  try 
    let _ = compiler (pathFichiersRat^"testDoubleDeclarationBloc.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()  


let%test_unit "testBloc" = 
  let _ = compiler (pathFichiersRat^"testBloc.rat") in ()


let%test_unit "testIDnonDeclarer" = 
  try 
    let _ = compiler (pathFichiersRat^"testIDnonDeclarer.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> () 
  
  
let%test_unit "testType1" = 
  let _ = compiler (pathFichiersRat^"testType1.rat") in ()