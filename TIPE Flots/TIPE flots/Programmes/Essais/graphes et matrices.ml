(*f&f_anciens ou un peu sp�ciaux ou particuliers*)
(*ici on code les graphes avec des matrices pour voir les parents d'un noeud facilement.
On se restreint donc � des petits graphes, mais c'est plus pratique si ils sont denses.
 *)
(*reference : livre "graphs and networks" de bernard carr�*)

let algo_avec_matrices =(*on d�fini les fausses op�rations, etc.*)
type nombre2 = ...
type nombre3 = ...
let prefix +| +~ +^ +$ +% +. +! +? +; +: +@
*| *~ *^ *$ *% *. *! *? *; *: *@ ...
 (**)

let atteint_et_modifie_ancien mat a b =

let ford_et_fulkerson_ancien mat a b =
(*on cherche comme avant, sans le graphe des �carts*)