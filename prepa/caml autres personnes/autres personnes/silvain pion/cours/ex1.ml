
Exos de programmation No 1. (le 27 octobre 1994)




Exercice 1 (Filtrer une liste)

let l = [3;1;4;1;5;9;2;6;5;3;5];;

let inf5 n = n<5;;
let sup5 n = n>=5;;

let rec filtre p = fun (a::l) -> if p a then a::(filtre p l)
                                        else filtre p l
                      |  []   -> [];;

filtre inf5 l;;
filtre sup5 l;;





Exercice 2 (Parcourir un arbre)

type 'a arbre = Sommet of 'a arbre * 'a arbre
              | Feuille of 'a
              | Vide;;

let rec concat = fun (c::l,l2) -> (c::concat(l,l2))
                   | ( [] ,l2) -> l2;;


let rec liste_feuilles = fun 
            (Sommet  s) -> concat(liste_feuilles(fst s),liste_feuilles(snd s))
          | (Feuille f) -> [f]
          |  Vide       -> [];;

let pi = Sommet (Sommet (Sommet (Feuille 3,Feuille 1),Sommet (Feuille 4,Feuille 1)) ,Sommet (Sommet (Sommet (Feuille 5,Sommet (Feuille 9,Feuille 2)),Feuille 6) ,Sommet (Sommet (Feuille 5,Feuille 3),Feuille 5)));;

liste_feuilles pi;;
