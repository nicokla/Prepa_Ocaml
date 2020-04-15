(*def_ensemble*)
(*si un élément est plusieurs fois dans un ensemble, pas grave*)
(*file:///C:/Program%20Files/WinCaml3_1/
WinCaml/man-caml/node14.13.html*)
(*file:///C:/Program%20Files/WinCaml3_1/
WinCaml/man-caml/node15.16.html*)

____________________________________________
;;

#open "set";;
#open "graphics";;
include "biblio2/TIPE/programmes/types/def_nombre";;

type ensemble == (int*(nombre vect)) t;;
type graphe4 == ensemble vect;;
type dessin_nomb4 = {mutable tai: int;
 mutable arc : ((int*nombre vect) t) vect;
mutable pos : (int*int) vect;mutable nom : string vect;mutable col : color vect};;





