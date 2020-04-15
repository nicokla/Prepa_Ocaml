

(*!--------------------------------*)
(*fonctions pour test2 avec nimbers*)

let flo=float_of_int;;

let rec converti base n = match n with
|n when n<base -> [n]
|_-> (n mod base)::(converti base (n/base));;

let rec converto base l = match l with
|[]->0
|x::q-> x+ (base*(converto base q));;

let XOR b1 b2= match (b1,b2) with
|(b1,b2) when b1=b2->false
|_->true;;

let rec insere e l =match l with
|[] -> [e]
|a::q -> if e < a then e::l else (a::insere e q );;

let rec tri l=match l with
|[]->[]
|a::q-> insere a (tri q);;

let somme v =
  let k = ref 0 in
    for i = 0 to (vect_length v) - 1 do
      k := !k + v.(i)
    done;
    !k;;


let nimaddn base k l =
    let rec ninim a2 b2 = match (a2, b2) with
        | ([], _) -> b2
        | (_, []) -> a2
        | (a :: m, b :: n) -> ((a + b) mod base) :: ninim m n in
     (ninim k l);;

let nimaddv base v =
  let n = vect_length v in
    let x = ref (converti 2 (v.(0))) in
      for i = 1 to (n - 1) do
        x := nimaddn base !x (converti 2 v.(i)); done;
      !x;;

let maxi v = 
if v=[||] then 0
else let m = ref v.(0) and n=vect_length v in
for i=0 to n-1 do if !m<v.(i) then m:=v.(i) done; !m;;


(*!--------------------------------------------*)
(*test 1*)

let test v max =
  let pos = make_vect max 0 in
    let t = vect_length v in
      for i = 0 to (max - 1) do
        if pos.(i) = 0
        then
          begin
            for j = 0 to (t - 1) do
              if (i + v.(j) < max )
              then pos.(i + v.(j)) <- 1;
            done;
          end;
      done;
      pos;;


(*------------------------------------------*)
(*test2*)

let atte v t i pos = 
(*v rangé < !
renvoie les positions atteignables depuis i avec la règle v
de taille t dans le vecteur pos*)
  let e = ref 0 and k = ref [] in
    while ( (!e < t) && ( (i - v.(!e)) >= 0 ) ) do
      k := pos.(i - v.(!e)) :: (!k); e := (!e) + 1 done;
    !k;;

let mex l =
(*renvoie mex d'une liste d'entiers*)
  let l2 = tri l in
    if l2 = [] || hd l2 > 0 then 0
    else let rec mex2 l a =
        match (l, a) with
          | ([], a) -> a
          | (t :: q, a) -> if t > (a + 1) then a
              else mex2 q t
      in (mex2 l2 0) + 1;;

let test2 v max =
(*renvoie de 0 à max-1 les nimbers des positions*)
  let pos = make_vect max 0 in
    let t = (vect_length v) in
      for i = 0 to (max - 1) do pos.(i) <- mex (atte v t i pos)
      done;
      pos;;

atte [|1;5|] 2 1 [|5;1;0;5;2;7;8;0|];;
test2 [|3|] 15;;

(*-----------------------------------------*)
(*présentation, ecriture dans fichier générale*)


let fichier1= "C:/Documents and Settings/All Users/Bureau/test.txt";;
let fichier2= "C:/test.txt";;
let fichier3= "C:/Documents and Settings/All Users/Bureau/test2.txt";;

let chemin1="C:/Documents and Settings/All Users/Bureau/";;
let chemin2="C:/";;

let string_of_vect v =
  let n = vect_length v 
and st = string_of_int in
match n with
|0-> "[||]"
|1->"[|"^st v.(0)^"|]"
|_ ->let chaine = ref "[|" in
    for i = 0 to (n - 2) do chaine := (!chaine) ^ st (v.(i)) ^ ";"; done;
    (!chaine) ^ st (v.(n - 1)) ^ "|]";;

let string_of_list l=
let rec string_of_list2 l=
match l with
|[]->""
|[a]-> string_of_int a
|a::b-> string_of_int a ^ ";" ^ string_of_list2 b
in "["^string_of_list2 l^"]";;


#open "sys";;
let ecrire quoi ou (*sans effacer ancien contenu*)=
  let m = open ou [O_CREAT; O_TEXT; O_RDWR] s_iwall in
    let f = open_descriptor_out m in
      let bb = out_channel_length f in
        seek_out f (bb);
        (*let g=open_out_gen [O_CREAT ;O_TEXT; O_RDWR] s_iwall "C:/baaaaa.txt";;*)
        output_string f quoi;
        close_out f;;

(*ecrire "fdf" "C:/essai.txt";;*)

let ecrire2 quoi ou (*efface ancien contenu*)=
let m=open_out ou in
output_string m quoi;
close_out m;;

(*-----------------------------------------*)
(*recherche périodes depuis début*)

let cherche v n a l =
  (*cherche si il y a une période commençant à a,
de longueur l dans v, vecteur de longueur n*)
  let k = ref l and
  pasper = ref false in
    while !k + a < n && !pasper = false do
      pasper := v.(!k + a) <> v.(((!k) mod l) + a); (*preuve de l'absence de per.*)
      k := (!k) + 1
    done;
    not (!pasper);;


let periode v n a =
  (*Cherche une période commençant à a dans v vecteur de longueur n.
 Il faut qu'elle aie 2 occurences au moins dans v, 
cad qu'il ne s'interesse qu'aux périodes de longeurs <= (n-a)/2
 Renvoie 0 si pas de pér. trouvée*)
  let trouvé = ref false
  and l = ref 1 in
    while (!trouvé) = false && (!l) <= (n - a)/2 do
      trouvé := cherche v n a (!l);
      l := (!l) + 1
    done;
    if !trouvé = false then 0
    else (!l) - 1;;


periode [|1;1;0;1;1;0;1;1;0;
0;1;1;0;1;1;0;1;1;0;1;1;0;1;1;
1;0;1;1;0;1;1;0;1;1;0;1;1;0;0;1;1;0;1;1;0;1;1;0;1;1;0;1;1;0;0;
1;1;0;1;1;0;1;1;0;1;1;0;1;1;1;0;1;1;0|] 74 0;;


let cherche_n k v n =
  if n = 0 then []
  else begin
    (*cherche où il y a le nombre k dans le vecteur v de taille n*)
      let l = ref [] in
        for i = (n - 1) downto 0 do
          if v.(i) = k then l := i :: (!l)
        done;
        !l;
    end;;


let periode_gen v n =
  (*renvoie la longueur de la transition et la longueur de la période*)
  let a = ref 0 and per = ref 0 in
    while !per = 0 && !a < n
      (*on teste jusqu'au bout, dans le cas où la période commence tard*) do
      per := (periode v n !a);
      if !per = 0 then a := (!a) + 1
    done;
    if !per = 0 then 0, 0
    else !a, (!per) (*(n-a)/per *);;


(*---------------------------------------*)
(*programme qui fait tout ce qu'on veut = big_test*)
(*-------------------------------------*)

let strategie5 v=
let n =vect_length v in
let taille = 4*maxi v*n in
let k=test v taille in
(periode_gen k taille),k;;


let rec ajout e lst_ens=match lst_ens with
|[]->[]
|ens::q->(e::ens) :: (ajout e q);;

let rec parties ens = match ens with
|[]->[[]]
|e::sEns-> let partiesQ=parties sEns in
(partiesQ)@(ajout e partiesQ);;

let rec combinaisons p ens= match p,ens with
|0,_-> [[]]
|_,[]->[]
|p,e::sEns-> (combinaisons p sEns) @ (ajout e (combinaisons (p-1) sEns));;



let big_test l n fichier pres (*test les parties à n éléments*)=
  let si = string_of_int and vl = vect_of_list
  and sl = string_of_list and ll = list_length
  and fich = chemin1 ^ fichier ^ ".txt" in
  (*let part=parties l*) let part = combinaisons n l in
      
      let rec testl l =
        match l with
          | [] -> ()
          | a :: b -> let k = vl a in let strat = strategie5 k in let p = fst strat
                  in ecrire ((sl a) ^ "   " ^ si (snd p)) fich;
                    if fst p <> 0 then ecrire ("   (t=" ^ si (fst p) ^ ")    " ^
                        string_of_vect (sub_vect (snd strat) 0 (fst p))) fich;
                    ecrire ("     " ^ string_of_vect (sub_vect (snd strat) (fst p) (snd p)) ^ "\n") fich;
                    testl b;
      
      in if pres=true then ecrire ("_____________________" ^ "\n\n" ^ sl l ^ "     " ^ si n ^ "\n\n") fich;
        testl part;;


ecrire2 "" (chemin1^"test5.txt");;
(*big_test [1;4;7;11;12;13;17] 2 "test5" true;;*)

let test12 a b l m =
let fich = chemin1 ^ "test5.txt" in
ecrire ("_____________________" ^ "\n\n") fich;
  for i = l to m do
    let v = [a; b; i]
    in big_test v 3 "test5" false
  done;;

test12 2 9 1 50;;


(*let p = periode_gen (test v (500)) 500 in
let v1=test2 v (fst p + snd p ) in
ecrire (si (fst p)) (chemin1^"test5.txt");
ecrire ((string_of_vect (sub_vect (v1) (fst p) (snd p)))^"\n\n") (chemin1^"test5.txt");*)
