#open "graphics";;


let string_of_vect v =
  let n = vect_length v and chaine = ref "[|" in
    for i = 0 to (n - 2) do chaine := (!chaine) ^ string_of_int (v.(i)) ^ ";"; done;
    (!chaine) ^ string_of_int (v.(n - 1)) ^ "|]";;


let rec converti base n = match n with
|n when n<base -> [n]
|_-> (n mod base)::(converti base (n/base));;

let rec converto base l = match l with
|[]->0
|x::q-> x+ (base*(converto base q));;

let XOR b1 b2= match (b1,b2) with
|(b1,b2) when b1=b2->false
|_->true;;

let normalise v=
let n=vect_length v in
for i = 0 to n-1 do v.(i)<- abs(v.(i));done;v;;

let nimaddn base k l =
    let rec ninim a2 b2 = match (a2, b2) with
        | ([], _) -> b2
        | (_, []) -> a2
        | (a :: m, b :: n) -> ((a + b) mod base) :: ninim m n in
     (ninim k l);;

let nimaddv base v=
let n=vect_length v in
let x=ref (converti 2 (v.(0))) in
for i=1 to (n-1) do
x:=nimaddn base !x (converti 2 v.(i)); done;
!x;;

let fini_ou_pas v=
let n=vect_length v and i=ref 0 and trouve=ref false in
while(!i<n && !trouve=false) do
trouve:= v.(!i)<>0; i:=!i+1; done;
not(!trouve);;

let saute_ligne n=
for i=1 to n do print_newline() done;;


let dessiner_allumettes w h esp x1 x2 n =
set_line_width w;
let a=ref x1 and b = ref x2 in
for i=1 to n do
moveto !a !b; 
lineto !a (!b+h);
a:=!a+esp done;;


(*espace lignes = hauteur/3, width=esp/5*)
let maxi v = let m = ref v.(0) and n=vect_length v in
for i=0 to n-1 do if !m<v.(i) then m:=v.(i) done; !m;;

let représenter v = 
let max = (maxi v) in 
if (max) <> 0 then begin
      let n = (vect_length v) in
        let n2 = float_of_int n in
          let h = int_of_float (floor (400. /. n2 *. (1. -. (n2 +. 1.) /. (3. *. n2)))) in
              let esp = 550 / (max) in
                let k = h / 3 in
                  for i = 0 to (n - 1) do dessiner_allumettes (esp / 5) h esp 10 (h / 3 + i * 400 / n) v.(i)
                  done;
    end;;


(*----------------------------------------*)

let zero k v =
converto k (nimaddv k v)=0;;

let jouer_au_hasard v =
  let taille = vect_length v in
    let j = ref 0 in
      let k = ref 0 in
        while !k = 0 do
          j := random__int taille;
          k := v.(!j);
        done;
        v.(!j)<- random__int (!k);;


let jouer_bete v =
  let taille = vect_length v in
    let i = ref 0 in
      while !i < taille && v.(!i) = 0 do
        i := (!i) + 1
      done;
      v.(!i) <- random__int v.(!i);;


let tour_ia_bourrin v =
  if (zero 2 v)
  then jouer_bete v
  else
    begin
      let taille = vect_length v and
      vect = copy_vect v and j = ref 0 and i = ref 0 in
        while (not (zero 2 vect)&& !i<taille) do
          while ((!j) <= v.(!i)&& not (zero 2 vect)) do
            vect.(!i) <- (!j);
            j := (!j) + 1 done;
          if (zero 2 vect) then v.(!i) <- vect.(!i);
          j := 0; i := (!i) + 1
        done;
    end;;

let v=[|3;2;9|] in jouer_au_hasard v;v;;


(*-----------------------------------*)


let tour_joueur v k =
let n = vect_length v
and erreur = ref true
and a = ref 0 and b = ref 0 in
  for i = 1 to k do
    représenter v;
    while (!erreur) do
      print_string "Paquet à modifier? : ";
      a := read_int(); erreur := (n <= (!a)) || ((!a) < 0 || (v.(!a) = 0)); done;
    erreur := true; print_int !a; print_newline();
    while (!erreur) do
      print_string "Nombre d'allumettes en moins? : ";
      b := read_int(); erreur := (!b) <= 0 || (!b) > v.(!a); done;
    erreur := true; print_int !b;
    v.(!a) <- v.(!a) - !b;
    clear_graph();
  done;;

(*--------------------------------------*)


let final joueur =
  saute_ligne 2;
  print_string (joueur ^ ", tu es un looser :( \n");
  close_graph();;


(*--------------------------------------*)


let nim_classique u k ia =
  open_graph "";
  let v = normalise u (*enleve les négatifs*)in
    let fin = ref (fini_ou_pas v) and
    joueur = ref "joueur 1" in
      représenter v;
      while (!fin = false) do
        saute_ligne 2;
        print_string (!joueur); print_newline();
        print_string (string_of_vect (v)); print_newline();
        if ia = false || !joueur = "joueur 1" then
          tour_joueur v k
        else tour_ia_bourrin v;
        fin := fini_ou_pas v;
        if (!joueur = "joueur 1") then joueur := "joueur 2"
        else joueur := "joueur 1";
      done;
      final (!joueur);;


nim_classique [|5;3;8;1;9|] 1 true;;


(*ia pour k tours*)

