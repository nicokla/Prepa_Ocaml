
type gror == int list vect ;;(*ou matrice*)
type grdist == int*float list vect ;;
type 'a arbre = R of ('a*'a foret) and 'a foret=='a arbre list;;
type sqarb = R of sqforet and sqforet == sqarb list;;
type 'a bintree = Nil | Noeud of 'a * 'a bintree * 'a bintree;;

type nombre = infp | infm | nom of float;;
let prefix 
