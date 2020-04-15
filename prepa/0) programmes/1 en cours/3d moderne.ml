include "bibliothèques\Graphique.ml";;
include "bibliothèques\-Vecteurs.ml";;

let transf_coord d [|xc;yc;zc|] ph th [|xp;yp;zp|] =;;

let rota phi th v (*en rad*)= let a1 = cos th and a2 = sin th 
and b1 = cos phi and b2 = sin phi in
([|v|]$*[|[|1.;0.;0.|];[|0.;a1;a2|];[|0.;-.a2;a1|]|]
$*[|[|b1;0.;-.b2|];[|0.;1.;0.|];[|b2;0.;b1|]|]).(0);;

