open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_pointeur/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testUtilisationPointeur1"=
    let _ = compiler (pathFichiersRat^"testUtilisationPointeur1.rat") in ()

let%test_unit "testUtilisationPointeur2"=
  try
    let _ = compiler (pathFichiersRat^"testUtilisationPointeur2.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()


let%test_unit "testDoubleDeclaration1"=
  try
    let _ = compiler (pathFichiersRat^"testDoubleDeclaration1.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testDoubleDeclaration2"=
  try
    let _ = compiler (pathFichiersRat^"testDoubleDeclaration2.rat")
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testGeneral"=
    let _ = compiler (pathFichiersRat^"testGeneral.rat") in ()

