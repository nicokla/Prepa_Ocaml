(*2_normal_list*)

let rec insere_list_normal l j (cap, cout) =
  match l with [] -> [j, (cap, cout)]
    | (j2, (cap2, cout2)) :: b when j2 = j ->
        if cout2 = cout then
          if cap2 +& cap >& ent 0 then (j2, (cap2 +& cap, cout2)) :: b
          else b
        else if cap >& ent 0 then (j, (cap, cout)) :: b else l
    | a :: b -> a :: (insere_list_normal b j (cap, cout));;

let enleve_normal l1 j =
  let rec aux l l2 =
    match l with [] -> l2, (ent (- 1), indefini)
      | (j2, (cap, cout)) :: b -> if j2 = j then (l2 @ (tl l)), (cap, cout)
          else aux (tl l) ((hd l) :: l2) in
aux l1 [];;

