(*définitions types automate*)
include "bibliothèques\_Ensembles.ml";;
type etat == int;;
type etats == etat list;;
type etiquette == int;;
type transition == etat * etiquette * etat;;
type relation == transition list ;;
type paire == relation * relation ;;
type automate == etats * etats * etats * relation ;;
type mot == etiquette list;;
let epsilon = -1;;

(*exemples*)

let E = [0;1;2;3;4] and
I=[0] and
F =[4]
and T =[(0,-1,1);(0,0,3);(1,-1,2);(2,0,1);
(2,1,4);(3,-1,4)] in let p = (E,I,F,T) in
let a = decompose T
in compose (snd a) (fermeture E (fst a)) ;;

appartient 6 [8;9;6;2];;
cardinal [1;2;3;4;5;9];;
appartient 1 [6;9;2;1;3];;
egalite [5;9;2] [5;7;2];;
union [5;9;2] [6;9;2;8];;
intersection [1;2;3;4;5;9] [7;8;9;10;9;11;2];;

(*--------les automates-------*)

let prem = function (a,b,c) -> a;;
let mil = fun (a,b,c)->b;;
let der = function (a,b,c) -> c;;

let rec sui a gamma acc =
  match gamma with [] -> acc
    | x :: y -> if prem x = a then sui a y 
(union [der x] acc)
        else sui a y acc;;


let suivants e1 gamma =
  let rec suivantsr e1 gamma acc =
    match e1 with [] -> acc
      | a :: b -> suivantsr b gamma (union (sui a gamma []) acc)
  in suivantsr e1 gamma [];;

let accessibles e gamma =
  let rec accessiblesr b gamma acc=
    if egalite b acc then b
    else accessiblesr (union b (suivants b gamma)) gamma b
  in accessiblesr (union e (suivants e gamma)) gamma e;;

let rec prefixe o e ds =
  match ds with [] -> []
    | x :: y -> (o, e, x) :: (prefixe o e y);;

let decompose gamma (*gamma epsilon, l'autre*)=
  let rec decomposer gamma paire =
    match gamma with [] -> paire
      | a :: b -> let (_, y, _) = a in if y = epsilon then decomposer b (a :: fst paire, snd paire)
            else decomposer b (fst paire, a :: snd paire) in
    decomposer gamma ([], []);;

let rec fermeture e1 gamma =
match e1 with []->[]
|o::y-> union(prefixe o epsilon (accessibles [o] gamma)) (fermeture y gamma);;

let rec compose (g1:relation) (g2:relation) 
(*transitions normales, tr instantannées*)=
match g1 with []-> []
|x::y-> let (a,b,c) = x in 
let k = prefixe a b (accessibles [c] g2) in
union (compose y g2) k;;

let semi a1 =
let (E,I,F,T) = a1 in
let (m,n) = decompose T in let p = (fermeture E m)  in
(E,(union I (accessibles I m)),F,(compose n p));;



