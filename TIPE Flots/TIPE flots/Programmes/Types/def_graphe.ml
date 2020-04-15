(*def_graphe*)

type 'a graphe ==((int* 'a) list) vect;;
include "biblio2/TIPE/Programmes/Types/def_nombre";;

(*type graphe_nomb ==(int * (nombre vect)) list vect;;*)
#open "graphics";;

type dessin_nomb = {mutable tai: int; mutable arc : ((int * (nombre vect)) list) vect;
mutable pos : (int*int) vect;mutable nom : string vect;mutable col : color vect;};; 

let gra_of_des d = d.arc;;

(* type etiquette = {mutable cap : nombre ; mutable dis : nombre };;
type graphe_nomb_avecetiq = etiquette graphe;; *)
