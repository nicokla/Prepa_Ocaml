(*Fonctions basiques utiles pour les programmes maxflow et mincost*)

let nb_vect_default =
(-1,[|ent 0;ent 0|]);;

let enlever_arc G i j =
  let rec enlever l l2 k =
    match l with |[] -> l2, k
      | a :: b -> if fst a = j then enlever b l2 a else
            enlever b (a :: l2) k in let t = enlever G.(i) [] nb_vect_default
    in G.(i) <- fst t; snd t;;

let augmenter G i j p =
  let a, v = enlever_arc G i j in
    if a = - 1 then G.(i) <- (j, [|p|]) :: G.(i)
    else (*if a = j*) G.(i) <- (j, [|v.(0) +& p|]) :: G.(i);;

let diminuer G i j p =
  let a, v = enlever_arc G i j in
  (*on suppose l'absence d'erreur, ie on suppose a = j*)
    let r = v.(0) -& p in if r >& ent 0 then G.(i) <- (j, [|r|]) :: G.(i);;

let rec tout_changer G l p = match l with
    | [] -> ()
    | [a] -> ()
    | a :: b :: c -> diminuer G a b p; augmenter G b a p; tout_changer G (tl l) p;;

let valeur_flot G a =
  let k = ref (ent 0) in
    let l = ref G.(a) in
      while !l <> [] do
        k := !k +& (snd (hd !l)).(0); l := tl !l
      done; !k;;

let flot_of_ecart n g_ec2 g_cap2 =
  let g_cap = copy_vect g_cap2
  and g_ec = copy_vect g_ec2 in
    for i = 0 to (n - 1) do
      let l = ref [] in
        while g_cap.(i) <> [] do
          let (j, capa) = hd g_cap.(i) in
            g_cap.(i) <- tl g_cap.(i);
            let (p, q) = enlever_arc g_ec i j in
              if p = (- 1) (*route pleine*) then l := (j, [|capa.(0); capa.(1)|]) :: !l
              else let r = capa.(0) -& q.(0) in
                  if r >& ent 0 && r <> indefini && r <> omega && r <> omega_m
                  then l := (j, [|r; capa.(1)|]) :: !l
                  else if capa.(0) = omega then let (p, q) = enlever_arc g_ec j i in
                      if p <> (- 1) then l := (j, [|q.(0); capa.(1)|]) :: !l
        done;
        g_cap.(i) <- !l done; g_cap;;

let simplifier G g n a b (*renvoie la partition de la coupe min avec juste ceux accessibles 
depuis b dans le graphe d'écart final, et puis renvoie la valeur totale du flot*)=
      let v = make_vect n false in
        let rec explorer x =
          if not v.(x) then begin v.(x) <- true;
              let l1 = ref G.(x) in while !l1 <> [] do
                  let u = fst (hd !l1) in l1 := tl !l1; explorer u done;
            end in explorer a; let r = flot_of_ecart n G g in
(v, valeur_flot r a, r, G );;
(*coupe, valeur_flot, graphe_flot, graphe_écarts*)

let string_of_list l=
let rec string_of_list2 l=
match l with
|[]->""
|[a]-> string_of_int a
|a::b-> string_of_int a ^ ";" ^ string_of_list2 b
in "["^string_of_list2 l^"]";;

(*------------------------------------------------*)
(*Pour mincost :*)

let rec tout_changer3 G l p =
  match l with
    | [] -> ()
    | [a] -> ()
    | a :: b :: c ->
        let r, r2 = enlever_arc G a b and s, s2 = enlever_arc G b a in
            G.(b) <- (a, [|s2.(0) +& p; moins_nombre r2.(1)|]) :: G.(b);
            if r2.(0) >& p then G.(a) <- (b, [|r2.(0) -& p; r2.(1)|]) :: G.(a);
            tout_changer3 G (tl l) p;;


