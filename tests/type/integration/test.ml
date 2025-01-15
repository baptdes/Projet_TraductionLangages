open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/type/integration/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testPointeurXVariableGlobale" = 
  let _ = compiler (pathFichiersRat^"testPointeurXVariableGlobale.rat") in ()


let%test_unit "testPointeurXVariableGlobale2" = 
  try 
    let _ = compiler (pathFichiersRat^"testPointeurXVariableGlobale2.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu _ -> ()

let%test_unit "testPointeurXVariableStatique" = 
  let _ = compiler (pathFichiersRat^"testPointeurXVariableStatique.rat") in ()


let%test_unit "testPointeurXVariableStatique2" = 
  try 
    let _ = compiler (pathFichiersRat^"testPointeurXVariableStatique2.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu _ -> ()

let%test_unit "testPointeurXParam" = 
  try 
    let _ = compiler (pathFichiersRat^"testPointeurXParam.rat")
    in raise ErreurNonDetectee
  with
  | TypesParametresInattendus _ -> ()

