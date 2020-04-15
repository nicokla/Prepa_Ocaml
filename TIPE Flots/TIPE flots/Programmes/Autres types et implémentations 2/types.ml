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

include 
"biblio2/TIPE/Programmes/autres types et implémentations 2/fonctions de base";;
(*------------------------*)
type graphe_2_multi_vv == ((nombre*nombre) list) vect vect ;;

let ajoute_arc_2_multi_vv_ori_1 g i j (cap, cout)=
g.(i).(j)<- ajout_1 g.(i).(j) (cap,cout);;

let enlève_et_donne_arc_plus_court_2_multi_vv_ori_1 g i j =
let r,s = plus_court_1 g.(i).(j) in g.(i).(j)<-r;s;;
(*<-renvoie (ent -1,omega) si absent*)

let enlève_et_donne_arc_plus_long_2_multi_vv_ori_1 g i j =
let r,s = plus_long_1 g.(i).(j) in g.(i).(j)<-r;s;;

let ajouter_sommet_2_multi_vv_ori_1 des =
  let n = des.tai in des.tai <- n + 1;
    for i = 0 to n - 1 do des.arc.(i) <- concat_vect des.arc.(i) [|[]|] done;
    des.arc <- concat_vect des.arc [|(make_vect (n + 1) [])|];
    des.nom <- concat_vect des.nom [|(string_of_int n)|];;

let enlève_sommet_2_multi_vv_ori_1 des k =
  let n = des.tai in
    des.arc <- coupe_vect k n (des.arc);
    for i = 0 to n - 2 do
      des.arc.(i) <- coupe_vect k n (des.arc.(i)) done;
    des.tai <- n - 1;;

(*------------------------*)
type graphe_2_multi_avl == (int*((nombre*nombre) list)) avl vect;;

let ajoute_arc_2_multi_avl_ori_1 g i j (cap,cout)=
g.(i)<-insere_avl g.(i) j (cap,cout);;

let enlève_et_donne_arc_plus_court_2_multi_avl_ori_1 g i j =
let arb,(p,q) = enleve_aux_avl_plus_petit (g.(i)) j in
g.(i)<-arb; (p,q);;

let enlève_et_donne_arc_plus_long_2_multi_avl_ori_1 g i j =
let arb,(p,q) = enleve_aux_avl_plus_long (g.(i)) j in
g.(i)<-arb; (p,q);;

let ajouter_sommet_2_multi_avl_ori_1 des =
  let n = des.tai in des.tai <- n + 1;
    des.arc <- concat_vect des.arc [|V|];
    des.nom <- concat_vect des.nom [|(string_of_int n)|];;

let enlève_sommet_2_multi_avl_ori_1 des k =
let n = des.tai in
    des.arc <- coupe_vect k n (des.arc);
    for i = 0 to n - 2 do
      des.arc.(i) <-enleve_tout1 (des.arc.(i)) k done;
    des.tai <- n - 1;;

(*------------------------*)
type graphe_2_multi_list ==  (int*((nombre*nombre)list)) list vect;;

let ajoute_arc_2_multi_list_ori_1 g i j (cap,cout)=
g.(i)<-insere_list g.(i) j (cap,cout);;

let enlève_et_donne_arc_plus_court_2_multi_list_ori_1 g i j =
let l,(p,q) = enlève_aux_l_plus_petit (g.(i)) j in
g.(i)<-l; (p,q);;

let enlève_et_donne_arc_plus_long_2_multi_list_ori_1 g i j =
let l,(p,q) = enlève_aux_l_plus_long (g.(i)) j in
g.(i)<-l; (p,q);;

let ajouter_sommet_2_multi_list_ori_1 des =
  let n = des.tai in des.tai <- n + 1;
    des.arc <- concat_vect des.arc [|[]|];
    des.nom <- concat_vect des.nom [|(string_of_int n)|];;

let enlève_sommet_2_multi_list_ori_1 des k =
let n = des.tai in
    des.arc <- coupe_vect k n (des.arc);
    for i = 0 to n - 2 do
      des.arc.(i) <-enleve_tout_list (des.arc.(i)) k done;
    des.tai <- n - 1;;

(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
type graphe_2_normal_vv == (nombre*nombre) vect vect;;

let ajoute_arc_2_normal_vv_ori_1 g i j (cap, cout) =
  match g.(i).(j) with (ent (-1) , indefini) ->
        g.(i).(j) <- cap, cout;
    | (cap2, cout2) when cout2 = cout ->
        if cap2 +& cap >& ent 0 then g.(i).(j) <- (cap2 +& cap, cout)
        else g.(i).(j) <- (ent (-1)), indefini
    | (cap2, cout2) -> g.(i).(j) <- (cap, cout);;

let enlève_et_donne_arc_2_normal_vv_ori_1 g i j = let k = g.(i).(j) in
g.(i).(j)<- (ent (-1),indefini); k;;

let ajouter_sommet_2_normal_vv_ori_1 des =
  let n = des.tai in des.tai <- n + 1;
    for i = 0 to n - 1 do des.arc.(i) <- concat_vect des.arc.(i) [|(ent (-1),indefini)|] done;
    des.arc <- concat_vect des.arc [|( make_vect (n + 1) (ent (-1),indefini) )|];
    des.nom <- concat_vect des.nom [|(string_of_int n)|];;

let enlève_sommet_2_normal_vv_ori_1 des k =
  enlève_sommet_2_multi_vv_ori_1 des k ;;

(*------------------------*)
type graphe_2_normal_avl == (int*(nombre*nombre)) avl vect;;

let ajoute_arc_2_normal_avl_ori_1 g i j (cap,cout)=
g.(i)<-insere_avl4 g.(i) j (cap,cout);;

let enlève_et_donne_arc_2_normal_avl_ori_1 g i j =
let arb,(p,q) = enleve_tout3 (g.(i)) j in
g.(i)<-arb; (p,q);;

let ajouter_sommet_2_normal_avl_ori_1 des =
   ajouter_sommet_2_multi_avl_ori_1 des;;

let enlève_sommet_2_normal_avl_ori_1 des k =
let n = des.tai in
    des.arc <- coupe_vect k n (des.arc);
    for i = 0 to n - 2 do
      des.arc.(i) <-enleve_tout6 (des.arc.(i)) k done;
    des.tai <- n - 1;;

(*------------------------*)
type graphe_2_normal_list == (int*(nombre*nombre)) list vect ;;

let ajoute_arc_2_normal_list_ori_1 g i j (cap,cout)=
g.(i)<-insere_list_normal g.(i) j (cap,cout);;

let enlève_et_donne_arc_2_normal_list_ori_1 g i j =
let l,(p,q) = enleve_normal (g.(i)) j in
g.(i)<-l; (p,q);;

let ajouter_sommet_2_normal_list_ori_1 des =
 ajouter_sommet_2_multi_list_ori_1 des ;;

let enlève_sommet_2_normal_list_ori_1 des k =
enlève_sommet_2_multi_list_ori_1 des k ;;

(*------------------------*)
(*------------------------*)
(*------------------------*)
(*1 nombre*)

(*------------------------*)
(*------------------------*)
(*multigraphe*)

(*------------------------*)
type graphe_1_multi_vv == (nombre list) vect vect;;

let ajoute_arc_1_multi_vv_ori_1 g i j nb=
g.(i).(j)<- nb::g.(i).(j);;

let enlève_et_donne_arc_plus_court_1_multi_vv_ori_1 g i j =
let r,s = plus_court_3 g.(i).(j) in g.(i).(j)<-r;s;;
(*<-renvoie (omega) si absent*)

let enlève_et_donne_arc_plus_long_1_multi_vv_ori_1 g i j =
let r,s = plus_long_3 g.(i).(j) in g.(i).(j)<-r;s;;

let ajouter_sommet_1_multi_vv_ori_1 des =
 ajouter_sommet_2_multi_vv_ori_1 des;;

let enlève_sommet_1_multi_vv_ori_1 des k =
 enlève_sommet_2_multi_vv_ori_1 des k ;;

(*------------------------*)
type graphe_1_multi_avl == (int*(nombre list)) avl vect;;

let ajoute_arc_1_multi_avl_ori_1 g i j nb=
g.(i)<-insere_1_multi_avl g.(i) j nb;;

let enlève_et_donne_arc_plus_court_1_multi_avl_ori_1 g i j =
let arb,(p,q) = enleve_aux_avl_plus_petit_2 (g.(i)) j in
g.(i)<-arb; (p,q);;

let enlève_et_donne_arc_plus_long_1_multi_avl_ori_1 g i j =
let arb,(p,q) = enleve_aux_avl_plus_long (g.(i)) j in
g.(i)<-arb; (p,q);;

let ajouter_sommet_1_multi_avl_ori_1 des =
   ajouter_sommet_2_multi_avl_ori_1 des;;

let enlève_sommet_1_multi_avl_ori_1 des k =
let n = des.tai in
    des.arc <- coupe_vect k n (des.arc);
    for i = 0 to n - 2 do
      des.arc.(i) <-enleve_tout1 (des.arc.(i)) k done;
    des.tai <- n - 1;;

(*------------------------*)
type graphe_1_multi_list == (int*(nombre list)) list vect;;


(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
type graphe_1_normal_vv == nombre vect vect;;

(*------------------------*)
type graphe_1_normal_avl == (int*nombre) avl vect;;

(*------------------------*)
type graphe_1_normal_list == (int*nombre) list vect;;


(*------------------------*)
(*------------------------*)
(*------------------------*)
(*0 nombre*)

(*------------------------*)
(*------------------------*)
(*graphe*)

(*------------------------*)
type graphe_0_normal_vv== bool vect vect;;

(*------------------------*)
type graphe_0_normal_avl == int avl vect ;;

(*------------------------*)
type graph_0_normal_list == int list vect;;

(*------------------------*)
type graphe_0_normal_t == int t vect;;



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

