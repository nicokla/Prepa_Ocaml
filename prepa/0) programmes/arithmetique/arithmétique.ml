
(*nul car liste nécessaire*
let test_prem l n=
let l2 = ref l and k=racine_int n in
	try
		while (hd !l2) <= k do
			if n mod hd !l2 = 0 then raise composé ;
			l2:= tl !l2
		done;true;
	with |composé-> false
|_-> true;;

let add a b = a+b;;

let preliste n (*sens +*)= 
let l=ref [] in
for i = n downto 1 do
l:=(map (add (30*i)) [1;7;11;13;17;19;23;29])@(!l)
done;
[2;3;5;7;11;13;17;19;23;29]@(!l);;

let premiers n (*sens - ! mauvais car big preliste*)=
  let l = preliste n and l1 = ref [] in
    let test = test_prem l and
    l2 = ref l in
      while !l2 <> [] do let a = hd !l2 in
          if test a then l1 := a :: (!l1);
          l2 := tl (!l2) done;
      !l1;;

test_prem [2;3;5;7;11;13] 5;;
let ecrire_prem n = ecrire (str_list2 (premiers n)) "C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/premier.txt";;
let effacer ()= ecrire2 "" "C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/premier.txt";;
ecrire_prem 1000;;
effacer();;
*-------------------*)
(*
somme diviseurs
prod diviseurs
;;*)


(*----------*)


#open "sys";;
include "biblio/utile/fonctions discrètes.ml";;

let ecrire2 quoi ou (*efface ancien contenu*)=
let m=open_out ou in
output_string m quoi;
close_out m;;
let fichier1 ="C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/premier.txt";;

exception composé ;;
exception trop;;
let flo = float_of_int;;

let rec converti base n (*a l'envers*)= match n with
|n when n<base -> [n]
|_-> (n mod base)::(converti base (n/base));;

let rec converto base l = match l with
|[]->0
|x::q-> x+ (base*(converto base q));;

converti 16 1036;;

 
let racine_int n =
  let i = ref 1 and j = ref 1 in
    while !i <= n do
      j := (!j) + 2;
      i := (!i) + (!j)
    done;
    !j / 2;;

let test2 (*pas de vecteurs*) n (*renvoie true s'il est prem*) =
  if n = 2 || n=3 then true else
  if n mod 2 = 0 then false else
    let k = racine_int n in
      let rec test3 i n =
        match n mod i with
          | 0 -> false
          | _ -> if i <= k then test3 (i + 2) n
              else true
      in test3 3 n;;

let premiers2 n1 (*avec listes*) =
  let rec premiers3 n l (* tous les premiers <= n*) =
    match n with
      | 1 -> 1 :: l
      | 2 -> 2 :: l
      | 3 -> 2 :: 3 :: l
      | _ -> if test2 n then premiers3 (n - 2) (n :: l)
          else premiers3 (n - 2) l
  in if n1 = 2 then [2] else if (n1 mod 2) = 0 then premiers3 (n1 - 1) []
    else premiers3 n1 [];;

let ecrire_prem n ou (*40 sec pour 1 million*) =
  let m = open_out ou and i = ref 3 in
    let t = output_string m in
      while !i < n do
        if test2 !i then t ((string_of_int !i) ^ "\n");
        i := (!i) + 2
      done;
      close_out m;;

ecrire_prem 100000 fichier1;;


let ecrire_prem2 n =
  let i = ref 5 and p = ref 2 in
    let v = make_vect n 1 in
      v.(0) <- 2;
      v.(1) <- 3;
      while !p < n do
        let k = racine_int !i and j = ref 0 and c = ref true in
          while v.(!j) <= k && !c do
            if (!i mod v.(!j) = 0) then c := false
            else j := !j + 1 done; 
					if !c then
            begin v.(!p) <- (!i); p := !p + 1 end;
          i := !i + 2;
      done; v;;

ecrire_prem2 20;;

let effacer ()= ecrire2 "" fichier1 ;;


let rec pgcd a b = match a, b with
    | 0, _ -> b
    | _, 0 -> a
    | _ -> pgcd b (a mod b);;

let bezout x0 y0 =
  let x = ref x0
  and y = ref y0
  and a = ref 1
  and b = ref 0
  and c = ref 0
  and d = ref 1 and
  q = ref 0 in
    while !y > 0 do
      let t = !x in
        x := !y; q := t / (!y);
        y := t mod !y;
        let h = !a and i = !b in
          a := !c;
          b := !d;
          c := h - (!q * !c);
          d := i - (!q * !d);
    done;
    !a, !b;;

bezout 5 4;;

let ppcm a b = a*b/(pgcd a b);;

let decompose n (*tous diviseurs premiers*) =
  let l1 = premiers2 (racine_int (n)) in
    let rec dede l n = match l, n with
        | _, 1 -> []
				| _,2->[2]
        | _, 3 -> [3]
        | [], _ -> [n]
        | _ -> let a = hd l in if n mod a = 0 then a :: (dede l (n / a))
              else dede (tl l) n
    in dede l1 n;;

let cardi l =
  let rec cardz l n k (*rangé*) =
    match l with [] -> n
      | [a] -> if a = k then begin if n = [] then [k,1]
              else ((k,(snd (hd n) + 1)) :: (tl n)) end else (a,1) :: n
      | a :: b -> if a = k then cardz b ((k,(snd (hd n) + 1)) :: (tl n)) (a)
          else cardz b ((a,1) :: n) (a);
  in cardz l [(hd l,0)] (hd l);;

cardi [5;8;88;18;9;9;9;0];;

let add a b = a+b;;

let rec mult l =
match l with []-> 1
|_-> hd l * mult (tl l);;

let rec som l =
match l with []-> 0
|_-> hd l + mult (tl l);;

let nb_div n = mult (map (add 1) (map snd (cardi (decompose n))));;

let decompose2 n = cardi (decompose n);;

let euler x = let k = ref 1 in for i = 2 to x/2
do if pgcd x i =1 then k:= !k+1 done; 2* (!k);;

let ordre_add a b = let k =ref 1 and o = ref a in
while !o<>0 do o:= (!o + a) mod b; k:= !k+1 done; !k;;

let ordre_mult a b (*modulo b*)=
if pgcd a b =1 then 
let k =ref 1 and o = ref a in
while !o<>1 do o:= (!o * a) mod b; k:= !k+1 done;!k;
else 0;;

ordre_mult 3 10;;

let inverse a b = if pgcd a b = 1 then begin
let t = (fst (bezout a b)) in if t >= 0 then (t mod b)
else b + (t mod b) end else failwith "no inverse";;


draw_discret euler 3 1000 500 2;;
draw_discret (fun x-> x) 1 100 400 1;;
(*----------------------------------*)
type fraction == int*int;;

let add_frac (a,b) (c,d) =(a*d+b*c,b*d);;
let soustr_frac (a,b) (c,d) =(a*d-b*c,b*d);;
let mult_frac (a,b) (c,d)=(a*c,b*d);;
let inv_frac (a,b) = (b,a);;
let div_frac (a,b) (c,d) = a*d,b*c;;
let float_of_frac (a,b) =flo a /. flo b;;
let simplifie_frac (p,q) = let k =pgcd p q in
(p/k, q/k);;

let frac_egypt (p0, q0) =
  let p = ref (p0 mod q0) and q = ref q0 and
  p2 = ref 1 and q2 = ref 0 in
    let l = ref [(p0 / q0, 1)] in while !p <> 0 do
        if !q mod !p = 0 then q2 := !q / !p else q2 := (!q / !p) + 1;
        let k = soustr_frac (!p, !q) (!p2, !q2) in
          p := fst k;
          q := snd k;
          l := (1, !q2) :: (!l) done; !l;;


let frac_cont (p0,q0) = let l = ref [] in
let p1 = ref p0 and q1=ref q0 and
q = ref 1 and r = ref 1 in
while !p1 mod !q1 <>0 do
q:=!p1/(!q1);
l:= !q::(!l);
r:=!p1 mod !q1;
p1:= !q1;
q1:= !r done; (!p1/(!q1)) ::(!l);;

(*periode frac base;;*)
