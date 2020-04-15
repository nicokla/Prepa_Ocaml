
(*fonctions de base*)

#open "set";;
type 'a dessin = {mutable tai: int; mutable arc : 'a;
mutable pos : (int*int) vect;mutable nom: string vect};;
(*mutable col:color vect*)
include "biblio2/TIPE/Programmes/types/def_nombre";;

(*------------------------*)
(*------------------------*)
(*------------------------*)
(*------------------------*)
(*orienté*)

(*------------------------*)
(*------------------------*)
(*------------------------*)
(*2 nombres*)

(*------------------------*)
(*------------------------*)
(*multigraphe*)

(*------------------------*)
(*type graphe_2_multi_vv == ((nombre*nombre) list) vect vect ;;*)

let coupe_vect k n v = 
concat_vect (sub_vect v 0 k) (sub_vect v (k+1) (n-k-1));;
include "biblio2/TIPE/Programmes/autres types et implémentations 2/listes";;

(*------------------------*)
(*type graphe_2_multi_avl == (int*((nombre*nombre) list)) avl vect;;*)
include "biblio2/TIPE/Programmes/autres types et implémentations 2/2_multi_avl1";;
include "biblio2/TIPE/Programmes/autres types et implémentations 2/2_multi_avl2";;

(*------------------------*)
(*type graphe_2_multi_list ==  (int*((nombre*nombre)list)) list vect;;*)
(*include "biblio2/TIPE/Programmes/autres types et implémentations 2/listes";;*)

(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
(*type graphe_2_normal_vv == (nombre*nombre) vect vect;;*)

(*------------------------*)
(*type graphe_2_normal_avl == (int*(nombre*nombre)) avl vect;;*)
include "biblio2/TIPE/programmes/autres types et implémentations 2/2_normal_avl";;

(*------------------------*)
(*type graphe_2_normal_list == (int*(nombre*nombre)) list vect ;;*)
include "biblio2/TIPE/programmes/autres types et implémentations 2/2_normal_list";;


(*------------------------*)
(*------------------------*)
(*------------------------*)
(*1 nombre*)

(*------------------------*)
(*------------------------*)
(*multigraphe*)

(*------------------------*)
(*type graphe_1_multi_vv == (nombre list) vect vect;;*)
include "biblio2/TIPE/programmes/autres types et implémentations 2/1_multi_vv";;

(*------------------------*)
(*type graphe_1_multi_avl == (int*(nombre list)) avl vect;;*)
include "biblio2/TIPE/programmes/autres types et implémentations 2/1_multi_avl";;

(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)

(*------------------------*)
(*type graphe_1_multi_list == (int*(nombre list)) list vect;;*)


(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
(*type graphe_1_normal_vv == nombre vect vect;;*)

(*------------------------*)
(*type graphe_1_normal_avl == (int*nombre) avl vect;;*)

(*------------------------*)
(*type graphe_1_normal_list == (int*nombre) list vect;;*)


(*------------------------*)
(*------------------------*)
(*------------------------*)
(*0 nombre*)

(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
(*type graphe_0_normal_vv== bool vect vect;;*)

(*------------------------*)
(*type graphe_0_normal_avl == int avl vect ;;*)

(*------------------------*)
(*type graph_0_normal_list == int list vect;;*)

(*------------------------*)
(*type graphe_0_normal_t == int t vect;;*)

(*---------------------------------------------------------------------*)
(*------------------------*)
(*-----------------------*)
(*-----------------------*)
(*-----------------------*)
(*non orienté*)

(*------------------------*)
(*------------------------*)
(*------------------------*)
(*2 nombres*)

(*------------------------*)
(*------------------------*)
(*multigraphe*)

(*------------------------*)
(*type graphe_2_multi_vv == ((nombre*nombre) list) vect vect ;;*)

(*------------------------*)
(*type graphe_2_multi_avl == (int*((nombre*nombre) list)) avl vect;;*)

(*------------------------*)
(*type graphe_2_multi_list ==  (int*((nombre*nombre)list)) list vect;;*)


(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
(*type graphe_2_normal_vv == (nombre*nombre) vect vect;;*)

(*------------------------*)
(*type graphe_2_normal_avl == (int*(nombre*nombre)) avl vect;;*)

(*------------------------*)
(*type graphe_2_normal_list == (int*(nombre*nombre)) list vect ;;*)


(*------------------------*)
(*------------------------*)
(*------------------------*)
(*1 nombre*)

(*------------------------*)
(*------------------------*)
(*multigraphe*)

(*------------------------*)
(*type graphe_1_multi_vv == (nombre list) vect vect;;*)

(*------------------------*)
(*type graphe_1_multi_avl == (int*(nombre list)) avl vect;;*)

(*------------------------*)
(*type graphe_1_multi_list == (int*(nombre list)) list vect;;*)


(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
(*type graphe_1_normal_vv == nombre vect vect;;*)

(*------------------------*)
(*type graphe_1_normal_avl == (int*nombre) avl vect;;*)

(*------------------------*)
(*type graphe_1_normal_list == (int*nombre) list vect;;*)


(*------------------------*)
(*------------------------*)
(*------------------------*)
(*0 nombre*)

(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
(*type graphe_0_normal_vv== bool vect vect;;*)

(*------------------------*)
(*type graphe_0_normal_avl == int avl vect ;;*)

(*------------------------*)
(*type graph_0_normal_list == int list vect;;*)

(*------------------------*)
(*type graphe_0_normal_t == int t vect;;*)



