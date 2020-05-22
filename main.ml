(*****************************************************
** @Author: Mamadou Saliou DIALLO (E18C764T)
**          Ibrahima DIALLO ()
** @Group: 685L
** Description: Implémentation des automates à pile
*****************************************************)

open Printf;;

(*  type_etat désigne l'ensemble des etats possibles pour une etat d'un automate*)
type type_etat = Initial | Final | Intermediaire | InitialFinal | Mixte;;

(* Le type transition caractérise la transition entre deux etats d'un automates *)
type transition = {destination: etat; valeur: char}

(* etat carctérise un etat d'un automate *)
and etat = {nom:string;  nature: type_etat; mutable transitions: transition list};;

(* Désigne un automate à pile *)
type automate = {etat_init: etat; alphabet: char list};;

(*****************************************
* Ajoute une transition entre deux etats
* @param etatSrc etat source
* @param etatDest etat destination
* @param Valeur sur la transition
******************************************)
let ajouter_transition etatSrc etatDest valeur =
  etatSrc.transitions  <- {destination = etatDest; valeur = valeur}::etatSrc.transitions
;;

(*******************************************************
* Genère les 26 lettres de l'alphabet français en 
* majuscule et minicule (un total de 52 caractères)
********************************************************)
let generer_alphabet_fr () = 
  let rec aux_function start = 
    match start with
    | 91 -> []
    | _         -> (char_of_int(start))::(char_of_int(start+32))::(aux_function (start + 1))

    in aux_function 65
  ;;

  let a = generer_alphabet_fr();;

(*******************************************
* Convertie un etat en chaine de caractère.
* @param n type_etat valeur à convertir. 
********************************************)
let type_etat_to_string n = 
  match n with 
  | Initial       -> "Initial"
  | Final         -> "Final"
  | Intermediaire -> "Intermediaire"
  | InitialFinal  -> "InitialFinal"
  | _              -> "Mixte"
;;

(*********************************************************************
* Affiche un les caractéristiques d'un etat de façon très simple en 
* @param etat Etat à afficher
**********************************************************************)
let affichage_simple_etat e =
  Printf.printf "Nom = %s\t Type = %s \t Transitions = [" e.nom (type_etat_to_string e.nature);
  let rec aux transitions = 
    match transitions with
    | [] -> ()
    | h::q -> Printf.printf "(%s, %c)" h.destination.nom h.valeur;
              aux q
    in aux e.transitions;
    Printf.printf "]\n"
;;

(*****************************************************
*     E x e m p l e s
******************************************************)
(* a^n b^m  with n <= m; n, m >=0 *)
let e1 = {nom = "e1"; nature = Initial; transitions = []};;
let e2 = {nom = "e2"; nature = Final; transitions = []};;
ajouter_transition e1 e1 'a';;
ajouter_transition e1 e2 'b';;
ajouter_transition e1 e2 ' ';;
ajouter_transition e2 e2 'b';;

affichage_simple_etat e1;;
affichage_simple_etat e2;;