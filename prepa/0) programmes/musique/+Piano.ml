#open "graphics";;
#open "sys";;
(*utile : 37-18000*)
(*-7 = pause*)

let pi = 3.141592654;;
let flo = float_of_int;;
let intof = int_of_float;;

let wait t (*en s*) =
let k = time() in
while time() < k+.t do () done;;

let a= 2.**(1./.12.);;
let b1=662.6;;
let b = 220.;;

let freq k(*freq de hauteur*)=
int_of_float(b*.(a**float_of_int(k)));;

let arrondi m=
if (abs_float(m-.ceil(m))-.abs_float(m-.floor(m)))<=.0. then
int_of_float(ceil m)
else int_of_float(floor m);;

let logb a x = log x /. log a;;
let log2 = logb 2.;;

let dton fre(*hauteur de frequence*)=
let f=float_of_int(fre) in
arrondi(12.*.log2(f/.b));;

let jouer2 v t(*code fréquence*)=
let n=vect_length v in
for i=0 to (n-1) do
sound (v.(i)) t done;;

let jouer v t(*code hauteur*)=
let v2 = map_vect freq v in
jouer2 v2 t;;

let note t a=sound (freq a) t;;
let notee = note 300;;

(*--------------------------*)

let int_of_clav l = (*tout sauf ²^$<*)
match l with
(*|``->*)
|`&`->(-13)
|`é`->(-12)
|`"`->(-11)
|`'`->(-10)
|`(`->(-9)
|`-`->(-8)
|`è`->(-7)
|`_`->(-6)
|`ç`->(-4)
|`à`->(-3)
|`)`->(-2)
|`=`->(-1)
|`a`->0
| `z` -> 1
| `e` -> 2
| `r` -> 3
| `t` -> 4
| `y` -> 5
| `u` -> 6
| `i` -> 7
| `o` ->  8
| `p` -> ( 9)
| `q` -> (10)
| `s` -> ( 11) 
| `d` -> ( 12) (*220hz*)
| `f` -> ( 13)
| `g` -> ( 14) 
| `h` -> ( 15) 
| `j` -> ( 16) 
| `k` -> (17) 
| `l` -> ( 18) 
| `m` -> ( 19) 
|`ù`-> (20) 
|`*`-> ( 21)
| `w` -> ( 22) 
| `x` -> ( 23) 
| `c` -> (24)(*440hz=la,~24*)
| `v` ->  25
| `b`->26
|`n`-> 27(*DO*)
|`,`-> (28) 
|`;`-> (29) 
|`:`-> (30) 
|`!`-> (31) 
|`1`-> (32) 
|`2`-> (33) 
|`3`-> (34) 
|`4`-> (35) 
|`5`-> (36) (*880*)
|`6`-> (37) 
|`7`-> (38) 
|`8`-> (39) 
|`9`-> (40) 
|`0`-> (41) 
|`°`-> 42
|`+`->43
|`A`-> (44) 
|`Z`-> (45) 
|`E`-> (46) 
|`R`-> (47) 
|`T`-> (48) (*1760*)
|`Y`-> (49) 
|`U`-> (50) 
|`I`-> (51) 
|`O`->  52 
|`P`-> 53
|`Q`-> (54) 
|`S`->  55
|`D`-> 56
|`F`-> 57
|`G`-> 58
|`H`-> 59
|`J`-> 60(*3520*)
|`K`-> 61
|`L`-> 62
|`M`-> 63
|`%`-> 64
|`µ`-> 65
|`W`-> 66
|`X`-> 67
|`C`-> 68
|`V`-> 69
|`B`-> 70
|`N`-> 71
|`?`-> 72(*7040*)
|`.`-> 73
|`/`-> 74
|`§`-> 75
| _ -> 0;;

let piano() =open_graph "";
let a= ref true and freq3 = fun x->freq (x-24) in
while (!a) do 
let e=wait_next_event [Button_down;Key_pressed] in
if e.keypressed then
match e.key with
|k-> sound (freq3 (int_of_clav k)) 100
else if e.button && (fst(mouse_pos()))<200 then a:= false;
done;;

(*piano();;*)

(*-------------------*)

let freq2 gamme n x =
  if x >= 0 || x mod n = 0 then
    let a = x / n and b = x mod n in
      freq ((12 * a) + gamme.(b) - 24)
  else let a = (x / n) - 1 and b = n + (x mod n) in
      freq ((12 * a) + gamme.(b) - 24);;

let piano2 gamme = open_graph "";
let n = vect_length gamme in
let a= ref true and 
freq3 = fun x->freq2 gamme n x in
while (!a) do 
let e=wait_next_event [Button_down;Key_pressed] in
if e.keypressed then
match e.key with
|k-> sound (freq3 (int_of_clav k)) 100
else if e.button && (fst(mouse_pos()))<200 then a:= false;
done;;

let gamme_jazz = [|0;3;5;6;7;10|];;
let gamme_maj =[|0;2;4;5;7;9;11|];;
let gamme_chro =[|0;1;2;3;4;5;6;7;8;9;10;11|];;
piano2 gamme_maj;;


