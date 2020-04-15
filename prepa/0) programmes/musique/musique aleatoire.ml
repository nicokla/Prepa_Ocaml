include "biblio/utile/aléatoire.ml";;
include "biblio/utile/+Piano.ml";;
include "biblio/utile/+Fichiers.ml";;

let gamme_jazz = [|0;3;5;6;7;10|];;
let gamme_maj =[|0;2;4;5;7;9;11|];;
let gamme_min = [|0;2;3;5;7;8;11|];;
let gamme_peur = [|0;3;6;9|];;
let gamme_chin = [|0;3;5;7;10|];;
let gamme_chro = [|0;1;2;3;4;5;6;7;8;9;10;11|];;

type morceau == (int * int) vect;;
let duree n = 200*n;;(*en 5ème de seconde*)

let transf v t(*pour le mettre sous la bonne forme*)=
map_vect (fun x->(dton x,t)) v;;
let transf2 v =
map_vect (fun (x,_)->x) v;;

let truc (a,b) = (freq a), b;;

let jouer3 (*code hauteur + durée*) v =
let n=vect_length v in
for i=0 to (n-1) do let a,b =v.(i) in
sound a b  done;;

let jouer4 v=
let v2 = map_vect truc v in
jouer3 v2;;

let alea gamme m r=
  let n = vect_length gamme in
    let k = ref 10 in
      for i = 1 to m do
        sound (freq2 gamme n (!k)) 200;
        k := (!k + (random__int (2*r+1)) - r) done;; 

(*alea gamme_jazz 200 2;;*)


let aleatoire_total1 u v m=
  for i = 1 to m do
    let k = piocher u v in
      (*sound k 200*)
note 200 k done;;


let alea_mem gamme m r =
  let n = vect_length gamme
  and v = make_vect m 0
  and k = ref 10 in
    for i = 0 to m - 1 do
      let u = freq2 gamme n (!k) in
        v.(i) <- u;
        if u > 4000 then k := !k + piocher (- r) (- 1) (*0*)
        else if u < 200 then k := !k + piocher 1 (*0*) r
        else k := (!k + (*hazard r*) (piocher (- r) r)) done;
    jouer2 v 50;
    v;; 

let k = alea_mem gamme_jazz 400 2;;
(*sauvegarder k (dossier_sauv^"jazz2-50.mus");;*)

let jouer_fich ou =
let p = avoir ou in
jouer2 p 100;;

jouer_fich (dossier_sauv^"chin1.mus");;

