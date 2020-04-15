include "biblio2/TIPE/programmes/types/def_nombre";;
#open "graphics";;

type graphe3 == ((nombre vect)list) vect vect;;

type dessin_nomb3 = {mutable tai: int;
 mutable arc :(nombre vect) vect vect ;
mutable pos : (int*int) vect;mutable nom : string vect;mutable col : color vect};;

let rec ajouter_l l v =
  match l with [] -> [v]
    | a :: b when a.(1) = v.(1) -> let c = a.(0) +& v.(0) in
          if c >& ent 0 then ([|c; a.(1)|] :: b) else b
    | a :: b -> a :: (ajouter_l b v);;

let ajouter G i j v = G.(i).(j)<-ajouter_l G.(i).(j) v;;

let ajouter_chemin G l1 = let rec aux l = match l with
      [] -> ()
      | [a] -> ()
      | a :: (b :: c) -> ajouter G (fst a) (fst b) (snd b); aux (tl l) in
    aux l1;;

let enleve_et_montre G i j = let k = G.(i).(j) in
    G.(i).(j) <- []; k;;
