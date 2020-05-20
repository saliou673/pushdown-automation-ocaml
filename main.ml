type type_etat = Initial | Final | Intermediaire | InitialFinal | Mixte;;
type transition = {destination: etat; valeur: string}
and etat = {nom:string; mutable nature: type_etat; transitions: transition list};;

let e1 = {nom = "e1"; nature = Initial; transitions = []};;
let e2 = {nom = "e2"; nature = Final; transitions = []};;

e1.transitions = {destination = e1; valeur = "a"}::{destination = e2; valeur = "b"}::e1.transitions;;
e2.transitions = [ {destination = e2; valeur = "b"}];;