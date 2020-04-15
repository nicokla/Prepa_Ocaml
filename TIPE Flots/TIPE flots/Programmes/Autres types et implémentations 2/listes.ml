

let ajout_1 l1 (p,q) =
let rec aux l l2 =
match l with [] -> (p,q)::l2
|a :: b when snd a = q -> if fst a +& p <=& ent 0 then (b@l2)
else ((p +& fst a, q)::(b@l2))
|a :: b -> aux b (a :: l2)
in aux l1 [];;

let plus_court_1 l =
let rec aux l1 (p, q) l2 =
match l1 with [] -> l2, (p, q)
|(a, b)::c when b <& q -> aux c (a, b) ((p, q) :: l2)
|(a, b)::c -> aux c (p, q) ((a, b) :: l2) in
aux l (ent (-1), omega) [];;

let plus_long_1 l =
let rec aux l1 (p, q) l2 =
match l1 with [] -> l2, (p, q)
|(a, b)::c when b >& q -> aux c (a, b) ((p, q) :: l2)
|(a, b)::c -> aux c (p, q) ((a, b) :: l2) in
aux l (ent (-1), omega_m) [];;

let rec insere_list l j (cap, cout) =
  match l with
    [] -> if cout >& ent 0 then [j, [cap, cout]]
        else []
    | (j2, l1) :: b when j2 = j ->
        let l2 = ajout_1 l1 (cap, cout) in if l2 = [] then b
          else ((j, l2) :: b)
    | (j2, l1) :: b -> (j2, l1) :: (insere_list b j (cap, cout));;

let rec enlève_aux_l_plus_petit l j =
  match l with [] -> [], (ent (- 1), omega_m)
    | (j2, l1) :: b when j2 = j ->
        let l2, (p, q) = plus_court_1 l1 in
          if l2 = [] then (b, (p, q))
          else ((j2, l2) :: b), (p, q)
    | (j2, l1) :: b ->
        let p = enlève_aux_l_plus_petit (tl l) j in
          ((j2, l1) :: fst p), snd p;;

let rec enlève_aux_l_plus_long l j =
  match l with [] -> [], (ent (- 1), omega_m)
    | (j2, l1) :: b when j2 = j ->
        let l2, (p, q) = plus_long_1 l1 in
          if l2 = [] then (b, (p, q))
          else ((j2, l2) :: b), (p, q)
    | (j2, l1) :: b ->
        let p = enlève_aux_l_plus_long (tl l) j in
          ((j2, l1) :: fst p), snd p;;

let rec enleve_tout_list l k =
  match l with [] -> []
    | (j2, l2) :: b -> if j2 = k then enleve_tout_list b k
        else if j2 > k then ((j2 - 1), l2) :: (enleve_tout_list b k)
        else (j2, l2) :: (enleve_tout_list b k);;






