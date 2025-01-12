open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/type/avec_pointeur/"

(**********)
(*  TESTS *)
(**********)


let%test_unit "testPointeur1" =
let _ = compiler   (pathFichiersRat^"testPointeur1.rat") in ()

let%test_unit "testPointeur2" =
try
  let _ = compiler   (pathFichiersRat^"testPointeur2.rat") in raise ErreurNonDetectee
with
  | TypesParametresInattendus _ -> ()

let%test_unit "testPointeur3" =
let _ = compiler   (pathFichiersRat^"testPointeur3.rat") in ()

let%test_unit "testPointeur4" =
try
  let _ = compiler   (pathFichiersRat^"testPointeur4.rat") in raise ErreurNonDetectee
with
  | TypeInattendu(Pointeur(Int),Int) -> ()

let%test_unit "testPointeur5" =
let _ = compiler   (pathFichiersRat^"testPointeur5.rat") in ()

let%test_unit "testGeneral" =
let _ = compiler   (pathFichiersRat^"testGeneral.rat") in ()

(* Fichiers de tests de la génération de code -> doivent passer la TDS *)
open Unix
open Filename

let rec test d p_tam = 
  try 
    let file = readdir d in
    if (check_suffix file ".rat") 
    then
    (
     try
       let _ = compiler  (p_tam^file) in (); 
     with e -> print_string (p_tam^file); print_newline(); raise e;
    )
    else ();
    test d p_tam
  with End_of_file -> ()

let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_fonction/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam