(*****************************************************
** @Author: Mamadou Saliou DIALLO (E18C764T)
**          Ibrahima DIALLO (E15E632Q)
** @Group: 685L
** Description: Implémentation des automates à pile
*****************************************************)

open Printf;;

(*  type_etat désigne l'ensemble des etats possibles pour une etat d'un automate*)
type type_etat = Initial | Final | Intermediaire | InitialFinal | Mixte;;

(*************************************************************
* Désigne une règle donnée sur une transition 
* empiler: bool, indique si la valeur doit être empiler ou pas
* valeur: char, la valeur à empiler ou dépiler
***************************************************************)
type regle = {empiler:bool; valeur: char};;
(* Le type transition caractérise la transition entre deux etats d'un automates *)
type transition = {destination: etat; valeur: char; regle: regle}

(* etat carctérise un etat d'un automate *)
and etat = {nom:string;  nature: type_etat; mutable transitions: transition list};;

(* Désigne un automate à pile *)
type automate = {etat_init: etat; alphabet_automate: char list; alphabet_pile: char list};;

(*****************************************
* Ajoute une transition entre deux etats
* @param etatSrc etat source
* @param etatDest etat destination
* @param Valeur sur la transition
******************************************)
let ajouter_transition etatSrc etatDest valeur regle =
  etatSrc.transitions  <- {destination = etatDest; valeur = valeur; regle = regle}::etatSrc.transitions
;;

(**************************************************************  
* Créer un nouveau automate 
* @param etat_i etat designe l'etat initial de l'automate
* @param alphabet_automate char list désigne l'alphabet de l'automate
* @param alphabet_pile char list désigne l'alphabet de la pile.
****************************************************************)
let creer_automate etat_i alphabet_automate alphabet_pile = 
  {etat_init = etat_i; alphabet_automate = alphabet_automate; alphabet_pile = alphabet_pile}
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
* @return ()
**********************************************************************)
let affichage_simple_etat e =
  Printf.printf "Nom = %s\t Type = %s \t Transitions = [" e.nom (type_etat_to_string e.nature);
  let rec aux transitions = 
    match transitions with
    | [] -> ()
    | h::q -> Printf.printf "(%s, %c, %s %c)" h.destination.nom h.valeur 
              (if h.regle.empiler then "Empiler" else "Dépiler")  h.regle.valeur;
              aux q
    in aux e.transitions;
    Printf.printf "]\n"
;;

(*********************************************************
* Convertie une chaine en caractère en liste de caractères.
* @param mot string mot à transformer en liste de caractères.
* @return char list
 **********************************************************)
let string_to_list mot =
  let rec aux i l =
   if i < 0 then l else aux (i - 1) (mot.[i] :: l) in
  aux (String.length mot - 1) []
;;

let ieme_transition transitions index =
  let rec aux trans index_cible index_courant =
    match trans with
    | [] -> failwith "Index invalide"
    | t::q when index_cible = index_courant-> t
    | t::q -> aux q index_cible (index_courant+1)
  in aux transitions index 0
;;

let depiler_pile pile supp_bas = 
  match pile with 
  | [] -> []
  | '*'::[] when supp_bas-> []
  | '*'::[] -> pile
  | t::q  -> q
;;

let est_final etat = 
  (etat.nature = Final) || (etat.nature = InitialFinal)
;;
let mot_est_trouve etat pile =
  (est_final etat) && (List.hd pile)='*'
;;
let decouper_mot mot index = 
  let rec aux m i j = 
    match j with
    | x when j >= (String.length mot) -> ""
    | x when j >= i -> (String.make 1  mot.[j])^(aux m i (j+1))
    | _ -> aux m i (j+1)
  in aux mot index 0
;;

let afficher_pile pile = 
  Printf.printf "[";
  let rec aux p = 
    match p with
    | [] -> ()
    | x::q -> Printf.printf " %c" x; if (List.length q) > 0 then Printf.printf " ,";  aux q
  in aux pile;
  Printf.printf " ]"
;;
let afficher_etat_automate etat_courant transition mot pile = 
  Printf.printf "\027[34;1m Etat courant: %s \t\027[32;1m Mot: %s \t\027[36;1m Transition vers %s(Lu: %c) \t\027[31;1m Pile après transition: " 
            etat_courant.nom mot transition.destination.nom transition.valeur;
  afficher_pile pile;
  Printf.printf "\n\027[0m"
;;

let rec iter_etat etat_courant mot index_car index_trans pile = 
  let trans_courant = ieme_transition etat_courant.transitions index_trans in
  match trans_courant with
  | x when trans_courant.valeur = mot.[index_car] ->
          (match trans_courant.regle.empiler with
          | true when (index_car + 1) = (String.length mot)-> 
                  afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) pile;
                  afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) (depiler_pile pile false);
                  mot_est_trouve trans_courant.destination pile

          | false  when (index_car + 1) = (String.length mot)  ->  
                  afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) (depiler_pile pile false);
                  afficher_etat_automate etat_courant trans_courant  " " (depiler_pile (depiler_pile pile false) true);
                  mot_est_trouve trans_courant.destination (depiler_pile pile false)

          | true -> afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car)  (mot.[index_car]::pile); 
                    iter_etat trans_courant.destination mot (index_car + 1) 0 (mot.[index_car]::pile)

          | false -> afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car)  (depiler_pile pile false);
                    iter_etat trans_courant.destination mot  (index_car + 1) 0 (depiler_pile pile false))

  | _ when index_trans < ((List.length etat_courant.transitions)-1) -> 
            iter_etat etat_courant mot index_car (index_trans +1 ) pile
  | _  -> false
;;

(***********************************************************
* Verifie si un mot est réconnue par un automate
* @param automate automate automate à exécuter
* @param mot string mot à tester
************************************************************)
let valider_mot automate mot = 
  Printf.printf "\t\t\t\027[37;1m===============================================\n";
  Printf.printf "\t\t\t=== V é r i f i c a t i o n    d u    m o t ===\n";
  Printf.printf "\t\t\t===============================================\027[0m\n\n";
  if mot= "" then true  else  iter_etat  automate.etat_init mot 0 0 ['*']
;;

let afficher_resultat mot result = 
  Printf.printf "\n\t\t\t";
  if result then Printf.printf "\027[32;1m Le mot \"%s\" est reconnu par l'automate\027[0m\n\n" mot
  else Printf.printf "\027[31;1mLe mot \"%s\" n'est pas reconnu par l'automate\027[0m\n\n" mot
;;
(*****************************************************
*     E x e m p l e s
******************************************************)
(* a^n b^m  with n <= m; n, m >=0 *)
let e1 = {nom = "e1"; nature = Initial; transitions = []};;
let e2 = {nom = "e2"; nature = Final; transitions = []};;
ajouter_transition e1 e1 'a' {empiler = true; valeur = 'a'};;
ajouter_transition e1 e2 'b' {empiler = false; valeur = 'a'};;
ajouter_transition e1 e2 ' ' {empiler = false; valeur = '*'};;
ajouter_transition e2 e2 'b' {empiler = false; valeur = 'a'};;

(* affichage_simple_etat e1;;
affichage_simple_etat e2;; *)

let alphabet = generer_alphabet_fr();;
let automate1 = creer_automate e1 alphabet alphabet;;

let mot = "aaa";;
let resultat = valider_mot automate1 mot;;
afficher_resultat mot resultat;;
