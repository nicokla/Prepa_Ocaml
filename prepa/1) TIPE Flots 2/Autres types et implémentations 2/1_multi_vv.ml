let plus_court_3 l1 =
  let rec aux l l2 nb = match l with [] -> l2, nb
      | a :: b -> if a <& nb then aux b (nb :: l2) a
          else aux b (a :: l2) nb in
    aux l1 [] omega;;

let plus_long_3 l1 =
  let rec aux l l2 nb = match l with [] -> l2, nb
      | a :: b -> if a >& nb then aux b (nb :: l2) a
          else aux b (a :: l2) nb in
    aux l1 [] omega;;

