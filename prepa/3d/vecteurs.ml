
(*let m_off = 3;;*)
let n_off = 3;;

let mult_mat_a_gauche v m (*taille depart m_of, taille arrivée n_of*)=
let m_off = vect_length v in
(*let n_off = vect length m.(0) and m_off = vect_length v in*) 
let w = make_vect n_off 0. in 
for i = 0 to (m_off -1) do for j = 0 to (n_off - 1) do
w.(j)<- w.(j) +. (v.(i) *. m.(i).(j))
done done; w;;

let prefix $$ v m = mult_mat_a_gauche v m;;

[|2.;3.;1.|] $$ [| [|1.;1.;1.|];[|-.1.;1.;0.|];[|0.;0.;6.|] |];;

let add v1 v2 = let w = [|0.;0.;0.|] in
 for i = 0 to (n_off - 1) do w.(i)<-v1.(i)+.v2.(i) done;w;;
let prefix +$ v1 v2 = add v1 v2;;

let moins v1 = let w = make_vect n_off 0. in
for i = 0 to n_off-1 do w.(i)<-(-.v1.(i)) done; w;;
let prefix -$ v1 v2 = v1 +$ (moins v2);;

([|5.;2.;1.|] -$ [|1.;2.;3.|]);;

let flo = float_of_int;;
let intof = int_of_float;;

let prefix *$ k v= [| k*.v.(0);k*.v.(1);k*.v.(2)|];;

let prefix /$ v k = (*if k <> 0. then *)(1./.k)*$v;;

let prefix %$ a b = a.(0)*.b.(0)+.a.(1)*.b.(1)+.a.(2)*.b.(2) ;;

let prefix ^$ a b = let aux w x y z = (w*.z) -. (x*.y) in
[|aux a.(1) a.(2) b.(1) b.(2);
aux b.(0) b.(2) a.(0) a.(2);
aux a.(0) a.(1) b.(0) b.(1) |];;

let norme_carre v = v %$ v;;
let norme v = sqrt (norme_carre v);;
int_of_char `+`;;
let unitaire v = v /$ (norme v);;

let tourne t v= let a = v.(0) and b = v.(1) in 
let c = cos t and s = sin t in
[|a*.c -. b*.s ; a*.s +. b*. c|];;
(*let tourne_3d v0 t v = ;;*)

let v_rot r t p = let ct = cos t and st = sin t and cp = cos p and sp = sin p in
(r*$[|(ct*.cp) ; (st*.cp) ; sp|]) ;;
let v_rot_un = v_rot 1.;;

let base_orth t p t2 = 
let ct = cos t and st = sin t and
cp = cos p and sp = sin p 
and ct2 = cos t2 and st2 = sin t2 in
let ex = [|st; -. ct ;0.|] and ey =[|(-.sp *.ct );(-.sp *.st ) ; cp |]
 in [|(ct2 *$ ex) +$ (st2 *$ ey) ; (-.st2 *$ ex) +$ (ct2 *$ ey) ;
[|(cp*.ct)  ; (cp*.st) ; sp |] |];;

let base_orth_ex1 t p (* = base t p 0.*)=
let ct = cos t and st = sin t and
cp = cos p and sp = sin p in
[| [|st; -. ct ;0.|] ;
[|(-.sp *.ct );(-.sp *.st ) ; cp |];
[|(cp*.ct)  ; (cp*.st) ; sp |] |];;


#open "graphics";;
let ouvrir a b = let u = string_of_int a and v = string_of_int b in
open_graph (u^"x"^v);;

let pi = 3.14159265359(*4.*.atan 1.*);;

let devant OH OM = OH %$ (OM-$OH) >= 0.;;
(*let barycentre...*)

let intersec OH H a b (*possible de vérifier cas pas intersec, a<>b, OH<>0, a et b
des deux côtés,...*)= 
let k1 = abs_float ((H-$a)%$OH) and k2 = abs_float ((H-$b)%$OH) in 
((k2*$a) +$ (k1*$b))/$ (k1+.k2);;

(*gestion du temps*)
#open "sys";;

let wait t (*en s*) =
let k = time() in
while time() < k+.t do () done;;

let ecrire (mes: string) x y = set_color black; 
let t = text_size mes in
  fill_rect x y (fst t) (snd t); set_color white; moveto x y;
  draw_string mes; set_color black;;

let action_rec fonc l1 = 
let rec aux l = match l with []->()
| _ -> fonc (hd l); aux (tl l) in aux l1;;

let test() = open_graph""; let abc = ref false in
while true do
 let e = wait_next_event [Key_pressed] in
if e.keypressed then if (e.key) = `-` then abc := not !abc;
if !abc then set_color black
else set_color white; fill_rect 100 100 100 100;  done;;

let test2() = 
open_graph"";
while true do
 let e = wait_next_event [Button_down;Button_up] in
if not e.button then begin set_color white; fill_rect 100 100 100 100 end
else set_color black; fill_rect 100 100 100 100  done;;

let rec appartient a l =
match l with [] -> false 
|p::q -> a=p || appartient a q;;

let angle base = 
let t = atan (base.(2).(1)/.base.(2).(0)) in
let p=asin ( base.(2).(0)/. cos t) in
let t2 = 0.(*la flemme*)
in [|t;p;t2|];;

let projeter_plan_ex1 bougie point =  
if bougie.(2) -. point.(2) = 0. then [|0.;0.;0.|] else
let k = bougie.(2)/.(bougie.(2) -.point.(2)) in
let v = k*$ (point -$ bougie) in bougie +$ v;;

let projeter_plan O OH M =
(*on renvoie P et pas OP comme dans... car O n'est pas cette fois le pt de vue*)
(*alors l'ex1 revient à OH = [0;0;-bougie.2]*)
let OM = M -$ O in
let k1 = OH%$OH and k2 = OH %$ OM in if k2 <> 0.
then let OP =(k1 /. k2) *$ OM in O+$OP else [|0.;0.;0.|];;

(*let soleil pave =
let boule O base M r =
let tore O base P PK R r =*)


