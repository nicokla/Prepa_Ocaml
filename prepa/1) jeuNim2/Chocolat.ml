
let inferieur a b=
fst a<fst b && snd a>snd b;;

let inv a=
let b,c= a in
c,b;;

let rec seqok l= match l with
|[]-> true
|[a]->true
|a::(b::c)-> ( inferieur a b ) && seqok (tl l);;

let inverser l =
  let rec inverser2 l acc =
    match l with
      | [] -> acc
      | a :: b -> inverser2 b ((inv a) :: acc)
  in inverser2 l [];;

let inverser2 l =
  let rec inverser3 l acc =
    match l with
      | [] -> acc
      | a :: b -> inverser3 b (a :: acc)
  in inverser3 l [];;


let rec balai a l acc =
    match l with
      | [] -> acc
      | q :: p -> if fst a <= fst q && snd a <= snd q && a <> q then balai a p acc
          else balai a p (q :: acc);;

let coupchoco a l = (* choco correct, fini *)
let entre b c d =
inferieur b c && inferieur c d in
  let rec caser a t acc joué =
      match t with
        | [] -> if joué= false then a::acc
								else acc
        | p :: q -> if fst p > fst a then caser a q (p :: acc) false
            else if fst p = fst a then (inverser2 t) @ acc
            else if (*fst p < fst a &&*) joué = false then begin
                if acc = [] then inverser2 (a :: t)
                else if entre p a (hd acc) then caser a q ([p; a] @ acc) true
                else caser a q (p :: acc) false end
            else caser a q (p :: acc) true
    in caser a (balai a l []) [] false;;


let taille choco =
  let rec taille2 choco i =
    match choco with
      | [] -> -1, -1
			|[0,0]-> 0,0
      | [a] -> if snd a = 0 then (fst a, i) else (- 1, i)
      | a :: b -> if fst a = 0 then taille2 b (snd a)
          else taille2 b i
  in taille2 choco (- 1);;

let normalise choco (*si correct et fini*)=
let k=taille choco in
if (fst k < snd k) then inverser choco
else choco;;

normalise [0,3;2,0];;

let verifier choco =
if seqok choco = false then failwith "chocolat incorrect"
else begin let x, y = (taille choco) in
        if x = (- 1) || y = (- 1) then failwith "chocolat infini" end;;

(*------------------------------------*)

#open "graphics";;

let bloc l x2 y2=
fill_rect (l*x2) ((l*y2)-1) (l-1) (l-1);;

let présenter choco m =
		verifier choco;
  let t = taille choco in let si = string_of_int in
      let f = si (m * fst t) and g = si (m * snd t) in
        open_graph (f ^ "*" ^ g);
        clear_graph();
        set_color black;(*rgb 128 64 0;*)
          let rec présenter2 choco =
            match choco with
              | [a] -> ()
              | a :: (b :: c) -> for i = (fst a) to (fst b) - 1 do
                    for j = 0 to (snd a) - 1 do
                      bloc m i j
                    done; done;
                  présenter2 (b :: c)
          in présenter2 choco;;

présenter [0,0] 20 ;;
présenter [0,10;3,9;4,8;5,7;7,2;9,0] 20;;

let coup k taille m =
set_color white;
  for i = fst k to (fst taille) - 1
  do for j = snd k to (snd taille) - 1
    do bloc m i j
    done; done;;

coup (4,5) (9,10) 20;;

(*-----------------------------------*)
(*evalue nimber de toutes les pos< [a;b]*)

let rec gagnant l  (*gagnant ssi true*)=
match l with
|[]-> false
|p::q-> (not p) || gagnant q;;


let atteint choco =
	verifier choco;
	let rec atteint2 choco1 choco2 acc =
		match choco2 with
			| [] -> acc
			| [a] -> if a = (0,0) then [[0,0]] else acc
			| a :: (b :: c) -> let d = ref acc in
							for i = fst a to (fst b) - 1 do
								for j = 0 to snd a - 1 do
									d := (coupchoco (i, j) choco1) :: (!d) done; done;
							(atteint2 choco1 (tl choco2) !d)
	in atteint2 choco choco [];;

(*version reccursive peu efficace :*)
let rec evaluer_rec pos=
match pos with
|[0,0]-> true
|[0,1;1,0]-> false
|[_;_]->true
|k when let h=taille k in fst h =snd h->
if hd (tl k) <> (1,1) then true
else false
|[_;1,1;_]-> true
|_->let k= atteint pos in
gagnant (map evaluer_rec (atteint pos));;

evaluer_rec [0,3;1,2;2,0];;

(*recherche pour 3 lignes*)



(*tour*)
let tour m=







