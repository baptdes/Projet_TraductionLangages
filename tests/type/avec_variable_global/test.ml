open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/type/avec_variable_global/"

(**********)
(*  TESTS *)
(**********)
  
let%test_unit "testType1" = 
  let _ = compiler (pathFichiersRat^"testType1.rat") in ()


let%test_unit "testTypeInatendu" = 
  try 
    let _ = compiler (pathFichiersRat^"testTypeInatendu.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu(Bool,Int) -> () 



let%test_unit "testPrintIntFonction" = 
  let _ = compiler (pathFichiersRat^"testPrintIntFonction.rat") in ()