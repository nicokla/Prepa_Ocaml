(*Problème de couplages dans des graphes bipartis*)
(*1) Mariage : couplages minimum (u,c) = (1,1)*)
(*11) Problème du travail, min doit être maximisé 
(traité à la fin du fichier)*)
(*2) Assignment, 
couplages de poids minimum (u,c) = (1,c)*)
(*21) cas particulier des skis (traité à la fin du fichier)*)
(*3) Transport : Capacités quelconques
31) version 1 : routes aux capacités infinies
32) version 2 : routes avec capacités *)

(*seuls les coûts >= 0 sont valides*)

include "biblio2/TIPE/programmes/partie graphique/dessins";;

(*-----------------------------------*)

let preparer k n=
let v = make_vect (k+n+2) [] in
for i = 1   to k   do v.(0)<-(i,[|ent 1; ent 0|])::v.(0) done;
for j = k+1 to k+n do v.(j)<-[(k+n+1,[|ent 1; ent 0|])] done;v;;

let graphe_couplage_of_mat m =
let k = vect_length m and n = vect_length m.(0) in
let v = preparer k n in
for i = 1 to k do
for j = 1 to n do
if m.(i-1).(j-1) then v.(i)<- (k+j, [|ent 1; ent 0|]) ::v.(i)
done done;v;;

let graphe_maxflow_mincut_assignment m=
let k = vect_length m and n = vect_length m.(0) in
let v = preparer k n in
for i = 1 to k do
for j = 1 to n do
if m.(i-1).(j-1) >= 0 (*si <0, signifie non relié*)
 then v.(i)<- (k+j, [|ent 1; ent m.(i-1).(j-1)|]) ::v.(i)
done done;v;;

let preparer_transport k n v1 v2 =
let v = make_vect (k+n+2) [] in
for i = 1   to k   do v.(0)<-(i,[|ent v1.(i-1); ent 0|])::v.(0) done;
for j = k+1 to k+n do v.(j)<-[(k+n+1,[|ent v2.(j-k-1); ent 0|])] done;v;;

let graphe_maxflow_min_cut_transport v1 v2 m=
let k = vect_length m and n = vect_length m.(0) in
let v = preparer_transport k n v1 v2 in
for i = 1 to k do
for j = 1 to n do
if m.(i-1).(j-1) >= 0 then 
v.(i)<- ( k+j, [|omega; ent m.(i-1).(j-1) |] ) ::v.(i)
done done;v;;

let graphe_maxflow_min_cut_transport_avec_capa v1 v2 m=
let k = vect_length m and n = vect_length m.(0) in
let v = preparer_transport k n v1 v2 in
for i = 1 to k do
for j = 1 to n do
if snd m.(i-1).(j-1) >= 0 then 
v.(i)<- ( k+j, [|ent (fst m.(i-1).(j-1)); 
ent (snd m.(i-1).(j-1) ) |] ) ::v.(i)
done done;v;;

(*---------------------------------*)

let faire_pos k n = let r = 600 / k and s = 600 / n and
  v = make_vect (k + n + 2) (100, 300) in
    v.(k + n + 1) <- (700, 300);
    for i = 1 to k do v.(i) <- (200, 600- (r * i) + (r/2)) done;
    for j = 1 to n do v.(k + j) <- (600, 600 -(s * j)+ (s/2)) done; v;;

let faire_biparti g k n= let v = faire_pos k n in
let dessin = des_of_gra2 g v in
menu_principal dessin;;

(*---------------------------------*)

let couplage_of_flot g k n =
  (*détruit g sous sa forme originelle
 mais on vient de le sauvegarder donc ce n'est pas grave*)
  let m = make_matrix k n false in
    for i = 1 to k do
      while g.(i) <> [] do
        let j, v = hd g.(i) in g.(i) <- tl g.(i);
          m.(i - 1).(j - k - 1) <- true done; done; m;;

let transport_of_flot g k n=
let m = make_matrix k n (ent 0) in
    for i = 1 to k do
      while g.(i) <> [] do
        let j, v = hd g.(i) in g.(i) <- tl g.(i);
          m.(i - 1).(j - k-1) <- v.(0) done; done; m;;

(*---------------------------------*)

let ouvrir_couplage m = let k = vect_length m and n = vect_length m.(0) in
    let g = graphe_couplage_of_mat m in
      let coût,(coupe,valeur_flot,g_flot,g_écart) = faire_biparti g k n in
valeur_flot, couplage_of_flot g_flot k n;;

let ouvrir_assignment m = let k = vect_length m and n = vect_length m.(0) in
let g = graphe_maxflow_mincut_assignment m in
let coût,(coupe,valeur_flot,g_flot,g_écart) = faire_biparti g k n in
coût, valeur_flot, couplage_of_flot g_flot k n;;

let ouvrir_transport v1 v2 m= 
let  k = vect_length m and n = vect_length m.(0) in
let g = graphe_maxflow_min_cut_transport v1 v2 m in
let coût,(coupe,valeur_flot,g_flot,g_écart) = faire_biparti g k n in
coût, valeur_flot, transport_of_flot g_flot k n;;

let ouvrir_transport_avec_capa v1 v2 m =
let  k = vect_length m and n = vect_length m.(0) in
let g = graphe_maxflow_min_cut_transport_avec_capa v1 v2 m in
let coût,(coupe,valeur_flot,g_flot,g_écart) = faire_biparti g k n in
coût, valeur_flot, transport_of_flot g_flot k n;;

(*----------------------------------*)

ouvrir_couplage [|
[|false;true;true;false;true|];
[|true;true;false;false;false|];
[|false;false;true;true;false|];
[|false;true;false;true;true|]
|];;

ouvrir_assignment [|
[|-1;2;7|];
[|2;1;4|];
[|3;6;1|]
|];;

ouvrir_transport
[|2;3;4|]
[|1;4;2;2|]
[|
[|1;2;-1;1|];
[|1;3;1;2|];
[|2;3;1;2|]
|];;

ouvrir_transport_avec_capa
[|2;3;4|]
[|1;4;2;2|]
[|
[|2,1;1,2;0,-1;3,1|];
[|2,1;2,3;2,1;1,2|];
[|1,2;3,3;1,1;2,2|]
|];;



(*------------------------------------*)
(*solutions différentes pour des cas spéciaux*)

(*
(*1: algorithme rapide pour les couplages*)
let trouver_couplage_max m = ...
;;

(*2: algorithme pour minimiser le max*)


(*3: algorithme dynamique pour les skis*)
*)
