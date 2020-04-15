

(*--------------------------------*)
(*TIPE 2009-2010, lycée Fermat *)
(*Jeux impartiaux normaux : jouer,connaitre la valeur
 d'un jeu et combiner des jeux*)
(*Auteurs : Pierre-Loup Sardo et Nicolas Klarsfeld*)
(*--------------------------------*)


(*définitions des types*)

type regle = | ni (*nim*)
 | wh of int(*whytoff;nb colonnes simultanément,si -1 autant sim*)
 | sp of int list(*spécial;coups autorisés*)
 |ch (*chocolat*);;

type position =
| int*int list
|int list;;

type 'a jeu_simple = {reg:regle; tou:int; mutable pos:'a list ; mutable nim:int};;

let l =
{reg=ni; tou=1; pos=
[
{reg=ch;tou=1;pos=[1;2;3];nim=3};{reg=ch;tou=1;pos=[(0,1);(1,2);(3,4)];nim=3}];
nim=3};;

add1 [|{pos=[|6;3;4|];regle=nim 3}|] {pos=[|4|];regle=nim 3};;

let add2 (jeu: jeu_simple vect) =
let n= vect_length jeu and j= ref [|jeu.(0)|] in
for i = 1 to n-1 do j:=add1 !j jeu.(i) done;
j;; 

(*---------------------------------*)
(*présentation*)


let string_of_vect v =
  let n = vect_length v and chaine = ref "[|" in
    for i = 0 to (n - 2) do chaine := (!chaine) ^ string_of_int (v.(i)) ^ ";"; done;
    (!chaine) ^ string_of_int (v.(n - 1)) ^ "|]";;

let présentation1(joueur,jeu)=
print_newline();print_newline();
print_endline (joueur);
print_endline (string_of_vect (jeu.pos));
écrire regle;;

let tour_n a joueur =


(*---------------------------------*)
(*jeu de nim*)


(*---------------------------------*)
(*whytoff*)




(*--------------------------------------*)
(*spécial*)


(*-------------------------*)
(*jouer*)

let tour_simple regle joueur=
match regle with
|nim(a)->tour_n a joueur
|w(a,b)->tour_w a b joueur
|s(v,c)->tour_s v c joueur;;


let jouer_simple (jeu:jeu_simple) (joueur: string vect)=
let fini = ref false and
qui=ref 0 and
j=ref jeu in

while fini = false do
tour_simple (!j) joueur.(!qui);
fini=fin(!j);
if !fini = false then qui=!qui+1
done;

print_endline (joueur.(!qui)^", tu as perdu :(");;

(*---------------------------*)
(*calculer position*)

let atteint regle position -> position vect
let mex : int vect -> int
let rec nimber regle pos= 
match atteint(pos) with
|[]->0
|v->mex (nimber regle a) (atteint regle position) (nimber b);;

