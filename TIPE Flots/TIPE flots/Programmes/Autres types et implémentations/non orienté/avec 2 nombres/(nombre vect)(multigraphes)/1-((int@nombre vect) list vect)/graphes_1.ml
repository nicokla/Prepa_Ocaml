
include "biblio2/TIPE/programmes/types/def_nombre";;
#open "graphics";;
type graphe1 == ((int* (nombre vect)) list) vect;;
type dessin_nomb1 = {mutable tai: int;
mutable arc :((int* (nombre vect)) list) vect ;
mutable pos : (int*int) vect;mutable nom : string vect;mutable col : color vect};;

(*arcs :*)
(*ajouter*)

let rec ajouter_l l (j, v) =(*option multigraphes*)
  match l with [] -> if v.(1)>& ent 0 then [j,v] else []
    | a :: b when ((snd a).(1) = v.(1) && fst a = j)->
    let c = (snd a).(0) +& v.(0) in
          if c >& ent 0 then ((j,[|c; (snd a).(1)|]) :: b) else b
    | a :: b -> a :: (ajouter_l b (j, v));;

let ajouter G i j v = G.(i)<-ajouter_l G.(i) (j,v);;

let ajouter_chemin G l1 = let rec aux l = match l with
      [] -> ()
      | [a] -> ()
      | a :: (b :: c) -> ajouter G (fst a) (fst b) (snd b); aux (tl l) in
    aux l1;;

(*enlever et montrer*)

let enleve_et_montre_tout G i j = let rec aux l l1 =
    match l with [] -> l1
      | a :: b when (fst a) = j -> aux b (a :: l1)
      | a :: b -> begin G.(i) <- a :: G.(i); aux b l1 end
  in let l = G.(i) in G.(i) <- []; aux l [];;

(*let enleve_et_montre_tout G i j = let l = ref G.(i) and l2 = ref [] in
    G.(i) <- []; while !l <> [] do
      if fst (hd !l) = j then l2 := (hd !l) :: (!l2)
      else G.(i) <- (hd !l) :: G.(i);
    done; !l2;;*)

let enleve_et_montre_cout G i j cout = let rec aux l =
    match l with [] -> (- 1, [||])
      | a :: b when (fst a) = j && (snd a).(1) = cout -> G.(i) <- b @ G.(i); a
      | a :: b -> begin G.(i) <- a :: G.(i); aux b end
  in let l = G.(i) in G.(i) <- []; aux l;;

let enleve_et_montre_petit G i j = let rec aux l c =
    match l with [] -> c
      | a :: b when (fst a) = j && (snd a).(1) <& (snd c).(1) ->
          G.(i) <- c :: G.(i); aux b a
      | a :: b -> begin G.(i) <- a :: G.(i); aux b c end
  in let l = G.(i) in G.(i) <- []; aux l (- 1, [|omega; omega|]);;

let enleve_et_montre_grand G i j = let rec aux l c =
    match l with [] -> c
      | a :: b when (fst a) = j && (snd a).(1) >& (snd c).(1) ->
          G.(i) <- c :: G.(i); aux b a
      | a :: b -> begin G.(i) <- a :: G.(i); aux b c end
  in let l = G.(i) in G.(i) <- []; aux l (- 1, [|omega_m; omega_m|]);;

(*montrer*)
let montre_tout G i j = let l = ref G.(i) and l2 = ref [] in
    while !l <> [] do
      if fst (hd !l) = j then l2 := (hd !l) :: (!l2); l := tl !l
    done; !l2;;

let montre_petit G i j =
  let k = ref (- 1, [|omega; omega|]) and l = ref G.(i) in
    while !l <> [] do
      let m = hd !l in if fst m = j && (snd m).(1) <& (snd !k).(1) then k := m;
        l := tl !l;
    done; !k;;

let montre_grand G i j =
  let k = ref (- 1, [|omega_m; omega_m|]) and l = ref G.(i) in
    while !l <> [] do
      let m = hd !l in if fst m = j && (snd m).(1) >& (snd !k).(1) then k := m;
        l := tl !l;
    done; !k;;



