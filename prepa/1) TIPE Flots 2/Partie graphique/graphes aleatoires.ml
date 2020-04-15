
(*---------------------------*)
#open "random";;
include "biblio2/TIPE/main/programmes/Types/def_nombre";;

(*-------------*)
let sqrt_int a = (*arrondie au supérieur, 
sqrt_int 9 = 3 mais sqrt_int 10 = 4*)
  let i = ref 1 and k = ref 0 in
    while !k < a do
      k := !k + !i;
      i := !i + 2 done; !i / 2;;

let dist_int (a, b) (c, d) =
  let k = a - c and t = b - d in sqrt_int (k * k + t * t);;

let dist_float (a, b) (c, d) =
  let k = floatof (a - c) and t = floatof (b - d) in sqrt (k ** 2. +. t ** 2.);;
(*----------*)

let random_list n k (*avec capa et distances par défaut*) =
  (*k est entre 0 et n-1*)
  let l = ref [] in
    for i = 0 to n - 1 do
      if i <> k then begin
          let z = random__int 2 in
            if z = 1 then l := (i, [|ent 1; ent 1|]) :: !l end
    done; !l;;

let graphe_aleatoire n (*on prend les capacités et distances égales à ent 1 par défaut
Ensuite on calculera des distances en plaçant les points aléatoirement
On aurait pu choisir les distances en même temps avec le random mais pb : l'inégalité triangulaire*) =
  let v = make_vect n [] in
    for i = 0 to n - 1 do
      v.(i) <- random_list n i done; v;;

let new_pos n = let v = make_vect n (- 2, - 2) in for i = 0 to n - 1 do
      let x = random__int 1000 and y = random__int 600 in v.(i) <- (x, y) done; v;;

let graphe_aleatoire2 e n (*pour les distances, en entier si e, sinon float
par contre pas vraiment aléatoire car points pris dans (1000,600)
De plus pas réaliste pour routes car non planaire à priori.*) =
  let pos = new_pos n in
    let g = graphe_aleatoire n in
      for i = 0 to n - 1 do let l = ref g.(i) and l2 = ref [] in
          while !l <> [] do let (p, h) = hd !l in l := tl !l;
              if e then h.(1) <- ent (dist_int pos.(i) pos.(p))
              else h.(1) <- flo (dist_float pos.(i) pos.(p));
              l2 := (p, h) :: !l2;
          done;
          g.(i) <- !l2;
      done; g;;

