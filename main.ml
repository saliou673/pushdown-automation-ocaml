(*****************************************************
** @Author: Mamadou Saliou DIALLO (E18C764T)
**          Ibrahima DIALLO (E15E632Q)
** @Group: 685L
** Description: Implémentation des automates à pile
*****************************************************)

open Printf;;

(*  type_etat désigne l'ensemble des etats possibles pour une etat d'un automate*)
type type_etat = Initial | Final | Intermediaire | InitialFinal;;

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
* @return unit
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


(*******************************************
* Convertie un etat en chaine de caractère.
* @param n type_etat valeur à convertir. 
* @return string
********************************************)
let type_etat_to_string n = 
  match n with 
  | Initial       -> "Initial"
  | Final         -> "Final"
  | Intermediaire -> "Intermediaire"
  | _  -> "InitialFinal"
;;

(*********************************************************************
* Affiche un les caractéristiques d'un etat de façon très simple en 
* @param etat Etat à afficher
* @return unit
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

(*******************************************************************
* Retourne la i-ème transition contenu dans une liste de transition
* @param transition list Liste de transition
* @param index int index de la transition à retourner
* @return transition
 ********************************************************************)
let ieme_transition transitions index =
  let rec aux trans index_cible index_courant =
    match trans with
    | [] -> failwith "Index invalide"
    | t::q when index_cible = index_courant-> t
    | t::q -> aux q index_cible (index_courant+1)
  in aux transitions index 0
;;

(***********************************************************
* Retourne la pile en supprimant l'élément au dessus.
* Le caractère initial '*' de la pile ne sera supprimé
* que si le paramètre 'supp_bas' est à true.
* @param pile char list liste de caractères.
* @param supp_bas bool true si le bas de la pile est à supprimer.
* @return char list
***********************************************************)
let depiler_pile pile supp_bas = 
  match pile with 
  | [] -> []
  | '*'::[] when supp_bas-> []
  | '*'::[] -> pile
  | t::q  -> q
;;

(************************************************************
* Test si un etat un final
* @param etat etat Etat de l'automate à tester
* @return bool
************************************************************)
let est_final etat = 
  (etat.nature = Final) || (etat.nature = InitialFinal)
;;

(************************************************************
* Vérifie si le mot recherché est retrouvé à partir de 
* l'etat courant de l'automate et de la pile.
* @param etat etat Etat courant de l'automate.
* @param pile char list Pile de l'automate.
* @return bool
************************************************************)
let mot_est_trouve etat pile =
  (est_final etat) && (List.hd pile)='*'
;;

(********************************************************************
* @param mot string mot à  découper
* @param index int index à partir du quel on commence le découpage
* @return string
*********************************************************************)
let decouper_mot mot index = 
  let rec aux m i j = 
    match j with
    | x when j >= (String.length mot) -> ""
    | x when j >= i -> (String.make 1  mot.[j])^(aux m i (j+1))
    | _ -> aux m i (j+1)
  in aux mot index 0
;;

(**********************************************************************
* Affiche la pile.
* @param pile char list Liste à afficher.
* @return ()
**********************************************************************)
let afficher_pile pile = 
  Printf.printf "[";
  let rec aux p = 
    match p with
    | [] -> ()
    | x::q -> Printf.printf " %c" x; if (List.length q) > 0 then Printf.printf " ,";  aux q
  in aux pile;
  Printf.printf " ]"
;;

(**********************************************************************
* Affiche l'etat d'un automate
* @param etat_courant etat Etat courant de l'automate
* @param transition transition Transition courante de l'automate.
* @param pile char list Pile de l'automate.
* @return unit
**********************************************************************)
let afficher_etat_automate etat_courant transition mot pile = 
  Printf.printf "\027[34;1m Etat courant: %s \t\027[32;1m Mot: %s \t\027[36;1m Transition vers %s(Lu: %c) \t\027[31;1m Pile après transition: " 
            etat_courant.nom mot transition.destination.nom transition.valeur;
  afficher_pile pile;
  Printf.printf "\n\027[0m"
;;

(***********************************************************************************
* Effectue une itération en partant d'un etat pour vérifier l'appartenance  d'un mot.
* Retourne true si le mot est reconnu et false dans le cas contraire.
* @param etat_courant etat Etat de l'automate.
* @param mot string Mot à vérifier.
* @param index_car int Index du caractère encours de vérification.
* @param index_trans int Index de la transition courante.
* @param pile char list Pile de l'automate
* @return bool
*************************************************************************************)
let rec tester_mot etat_courant mot index_car index_trans pile = 
  let trans_courant = ieme_transition etat_courant.transitions index_trans in
  match trans_courant with
  | x when trans_courant.valeur = ' ' -> 
          afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) pile;
          (tester_mot trans_courant.destination mot index_car 0 pile) || (tester_mot etat_courant mot index_car (index_trans + 1) pile)
  | x when trans_courant.valeur = mot.[index_car] ->
          (match trans_courant.regle.empiler with
          | true when (index_car + 1) = (String.length mot)-> 
                  afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) pile;
                  afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) (depiler_pile pile false);
                  mot_est_trouve trans_courant.destination pile

          | false  when (index_car + 1) = (String.length mot)  ->  
                let b = trans_courant.regle.valeur = (List.hd pile) in
                (match b with 
                | true -> 
                      afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) (depiler_pile pile false);
                      afficher_etat_automate etat_courant trans_courant  " " (depiler_pile (depiler_pile pile false) true);
                      mot_est_trouve trans_courant.destination (depiler_pile pile false)
                | _   -> afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) (depiler_pile pile false);
                        false
                )

          | true -> afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car)  (mot.[index_car]::pile); 
                    tester_mot trans_courant.destination mot (index_car + 1) 0 (mot.[index_car]::pile)

          | false -> 
                    
                    if trans_courant.regle.valeur = '*' then begin
                      afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car) pile;
                      tester_mot trans_courant.destination mot  (index_car + 1) 0 pile
                    end else begin
                      let b = trans_courant.regle.valeur = (List.hd pile) in
                      match b with 
                      | true -> afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car)  (depiler_pile pile false);
                                tester_mot trans_courant.destination mot  (index_car + 1) 0 (depiler_pile pile false)
                      | _    ->  if (index_trans + 1 )< (List.length etat_courant.transitions) then begin
                                    afficher_etat_automate etat_courant trans_courant (decouper_mot mot index_car)  pile;
                                    tester_mot trans_courant.destination mot  (index_car+1) (index_trans+1) (depiler_pile pile false)
                                 end else false
                    end
          )
  | _ when index_trans < ((List.length etat_courant.transitions)-1) -> 
            tester_mot etat_courant mot index_car (index_trans +1 ) pile
  | _  -> false
;;

(***********************************************************
* Verifie si un mot est réconnue par un automate
* @param automate automate automate à exécuter
* @param mot string mot à tester
************************************************************)
let valider_mot automate mot = 
  Printf.printf "\t\t\t\027[37;1m===============================================================\n";
  Printf.printf "\t\t\t V é r i f i c a t i o n    d u    m o t  \"%s\" \n" mot;
  Printf.printf "\t\t\t===============================================================\027[0m\n\n";
  if mot= "" then true  else  tester_mot  automate.etat_init mot 0 0 ['*']
;;

(*************************************************************
* Affiche un message du resultat de vérification
* @param mot string Mot recherché.
* @param result bool Resultat booléen de la recherche du mot.
* @return unit
***************************************************************)
let afficher_resultat mot result = 
  Printf.printf "\n\t\t\t";
  if result then Printf.printf "\027[32;1m Le mot \"%s\" est reconnu par l'automate\027[0m\n\n" mot
  else Printf.printf "\027[31;1mLe mot \"%s\" n'est pas reconnu par l'automate\027[0m\n\n" mot
;;


(********************************************************************************************************
**********************************  l  e  s       e  x  e  m  p  l  e  s ********************************
**********************************************************************************************************)


Printf.printf "\027[32;1m\n\n=======================================================================================================\n";;
Printf.printf "\t\t\t  E x e m p l e  1 :   \ta^n b^n  avec ; n >=0 \n";;
Printf.printf "=======================================================================================================\n\027[0m";;

let e1 = {nom = "e1"; nature = Initial; transitions = []};;
let e2 = {nom = "e2"; nature = Final; transitions = []};;
ajouter_transition e1 e1 'a' {empiler = true; valeur = 'a'};;
ajouter_transition e1 e2 'b' {empiler = false; valeur = 'a'};;
ajouter_transition e1 e2 ' ' {empiler = false; valeur = '*'};;
ajouter_transition e2 e2 'b' {empiler = false; valeur = 'a'};;

let alphabet = 'a'::'b'::[];;
let automate1 = creer_automate e1 alphabet alphabet;;

(* Test 1 a^3 b^3 *)
let mot = "aaabbb";;
let resultat = valider_mot automate1 mot;;
afficher_resultat mot resultat;;

(* Test 2 a^2 b^5 *)
let mot = "aabbbbb";;
let resultat = valider_mot automate1 mot;;
afficher_resultat mot resultat;;

(* Test 3 a^5 b^3 *)
let mot = "aaaaabbb";;
let resultat = valider_mot automate1 mot;;
afficher_resultat mot resultat;;



Printf.printf "\027[32;1m\n\n=======================================================================================================\n";;
Printf.printf "\t\t\t  E x e m p l e  2 :   \ta^n b^m  avec n <= m; n, m >=0 \n";;
Printf.printf "=======================================================================================================\n\027[0m";;

let e1 = {nom = "e1"; nature = Initial; transitions = []};;
let e2 = {nom = "e2"; nature = Final; transitions = []};;
ajouter_transition e1 e1 'a' {empiler = true; valeur = 'a'};;
ajouter_transition e1 e2 'b' {empiler = false; valeur = 'a'};;
ajouter_transition e1 e2 ' ' {empiler = false; valeur = '*'};;
ajouter_transition e2 e2 'b' {empiler = false; valeur = '*'};;
ajouter_transition e2 e2 'b' {empiler = false; valeur = 'a'};;

let alphabet = 'a'::'b'::[];;
let automate2 = creer_automate e1 alphabet alphabet;;

(* Test 1 a^3 b^5 *)
let mot = "aaabbbbb";;
let resultat = valider_mot automate2 mot;;
afficher_resultat mot resultat;;

(* Test 2 a^5 b^2 *)
let mot = "aaaaabb";;
let resultat = valider_mot automate2 mot;;
afficher_resultat mot resultat;;

(* Test 3 a^3 b^3 *)
let mot = "aaabbb";;
let resultat = valider_mot automate2 mot;;
afficher_resultat mot resultat;;


Printf.printf "\027[32;1m\n\n=======================================================================================================\n";;
Printf.printf "\t\t\t  E x e m p l e  3 :   \ta^i b^j c^k  avec i=j ou i=k \n";;
Printf.printf "=======================================================================================================\n\027[0m";;

let b = {nom = "b"; nature = Initial; transitions = []};;
let c = {nom = "c"; nature = Intermediaire; transitions = []};;
let d = {nom = "d"; nature = Final; transitions = []};;
let e = {nom = "e"; nature = Intermediaire; transitions = []};;
let f = {nom = "f"; nature = Final; transitions = []};;

ajouter_transition b b 'a' {empiler = true; valeur = 'a'};;
ajouter_transition b c ' ' {empiler = false; valeur = '*'};;
ajouter_transition b e ' ' {empiler = false; valeur = '*'};;
ajouter_transition c c 'b' {empiler = false; valeur = 'a'};;
ajouter_transition c d ' ' {empiler = false; valeur = '*'};;
ajouter_transition d d 'c' {empiler = false; valeur = '*'};;
ajouter_transition e e 'b' {empiler = false; valeur = '*'};;
ajouter_transition e f ' ' {empiler = false; valeur = '*'};;
ajouter_transition f f 'c' {empiler = false; valeur = 'a'};;

let alphabet = 'a'::'b'::'c'::[];;
let automate3 = creer_automate b alphabet alphabet;;
(* Test 1 a^2 b^2 c^1 *)
let mot = "aabbc";;
let resultat = valider_mot automate3 mot;;
afficher_resultat mot resultat;;

(* Test 2 a^2 b^2 c^2 *)
let mot = "aabbcc";;
let resultat = valider_mot automate3 mot;;
afficher_resultat mot resultat;;

(* Test 3 a^3 b^1 c^3 *)
let mot = "aaabccc";;
let resultat = valider_mot automate3 mot;;
afficher_resultat mot resultat;;

(* Test 3 a^3 b^1 c^2 *)
let mot = "aaabcc";;
let resultat = valider_mot automate3 mot;;
afficher_resultat mot resultat;;