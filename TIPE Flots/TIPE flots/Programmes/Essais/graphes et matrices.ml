(*f&f_anciens ou un peu spéciaux ou particuliers*)
(*ici on code les graphes avec des matrices pour voir les parents d'un noeud facilement.
On se restreint donc à des petits graphes, mais c'est plus pratique si ils sont denses.
 *)
(*reference : livre "graphs and networks" de bernard carré*)

let algo_avec_matrices =(*on défini les fausses opérations, etc.*)
type nombre2 = ...
type nombre3 = ...
let prefix +| +~ +^ +$ +% +. +! +? +; +: +@
*| *~ *^ *$ *% *. *! *? *; *: *@ ...
 (**)

let atteint_et_modifie_ancien mat a b =

let ford_et_fulkerson_ancien mat a b =
(*on cherche comme avant, sans le graphe des écarts*)