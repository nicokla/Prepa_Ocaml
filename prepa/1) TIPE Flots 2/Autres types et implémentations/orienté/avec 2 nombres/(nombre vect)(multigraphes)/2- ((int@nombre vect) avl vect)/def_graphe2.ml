(*définitions de graphes plus pratiques pour certaines choses,
 ou optimisant les calculs*)

include "biblio2/TIPE/programmes/types/def_avl";;
type graphe2 == avl vect;;
(*type graphe_nomb2 == ((nombre vect) avl) vect;;*)

type dessin_nomb2 = {mutable tai: int; mutable arc : avl vect;
mutable pos : (int*int) vect;mutable nom : string vect;mutable col : color vect;};;

let ajouter2 G i (j,v) = G.(i)<- insere_avl G.(i) (j,v);;

exception plus_de_ij;;

let enleve_et_montre G i j = let l = ref [] in
    let rec aux () = 
     let a, b = enleve_avl G.(i) (j, [||]) in 
      if fst b<> (-1) then
       begin G.(i) <- a; l := b :: !l end
      else raise plus_de_ij in
    try while true do aux () done; !l with
     plus_de_ij -> !l;;

let ajouter_chemin G l1 =
  let rec aux l =
    match l with [] -> ();
      | [a] -> ();
      | a :: (b :: c) -> ajouter2 G (fst a) b; aux (tl l) in
aux l1;;
