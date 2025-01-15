open Rat
open Compilateur

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/integration/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testPointeurXDefaut" = 
  let _ = compiler (pathFichiersRat^"testPointeurXDefaut.rat") in ()

let%test_unit "testPointeurXStaticXGlobale" = 
  let _ = compiler (pathFichiersRat^"testPointeurXStaticXGlobale.rat") in ()

let%test_unit "testStaticXGlobale" = 
  let _ = compiler (pathFichiersRat^"testStaticXGlobale.rat") in ()