(*3d*)

include "biblio2/3d/vecteurs";;

let O_ex1 = [|0.;0.;2.|];;
let OH_ex1 = [|1.;0.;0.|];;
let xecr = 800 and yecr = 800;;
let t2_ex1 = 0.;;
let ech_ex1 = 100.;;
let base_ex1 = base_orth_ex1 0. 0.;;

let reinit () = ouvrir 1100 800; clear_graph();;

let presente() = set_color white;
fill_rect 800 0 1100 800; set_color black; moveto 800 0; lineto 800 800;
let traith h = begin moveto 800 h; lineto 1100 h end and
traitv a b = begin moveto 950 a; lineto 950 b end in
for i = 1 to 5 do traith (100*i) done;
traith 650;
traitv 100 200; traitv 300 400; traitv 500 800;
moveto 875 340; draw_string "-"; moveto 1025 340; draw_string "+";
draw_arc 875 150 10 10 0 180; draw_arc 1025 150 10 10 0 180;
moveto 865 150; lineto 865 160; moveto 1035 150; lineto 1035 160;
moveto 940 770;draw_string "h"; moveto 940 510; draw_string "b";
moveto 810 622; draw_string "g"; moveto 1070 622; draw_string "d";;

let ecran_of_3d_proj_ex2 O base M= 
let OM = (M-$O) in let k = (base.(2) %$ OM) in
[|400 + intof ( base.(0) %$ OM  /.k*. ech_ex1); 
400+ intof ( base.(1) %$ OM /. k*. ech_ex1)|] ;;

let rec segment O base a b = (*let OH = v_rot_un t p in*)
let fonc = ecran_of_3d_proj_ex2 O base in
let r = ( devant base.(2) (a-$O) ) and q = ( devant base.(2) (b-$O) ) in
match (r,q) with 
|false,false->();
|true, false -> begin let M_0 = fonc a in moveto M_0.(0) M_0.(1);
 let M_1 = fonc (intersec base.(2) (O +$ base.(2)) a b) (*pas parallèle au plan car signe <> *)in 
lineto M_1.(0) M_1.(1) end;
|false,true -> segment O base b a; 
|true,true-> begin let M_0 = fonc a and M_1 = fonc b in
 moveto M_0.(0) M_0.(1);
 lineto M_1.(0) M_1.(1) end;;

let quadrillage O base v =
 let fonc x y = [|flo x; flo y;0.|] and
seg_ex1 = segment O base in
for i = v.(2) to v.(3) do
seg_ex1 (fonc v.(0) i) (fonc v.(1) i)
done;
for j = v.(0) to v.(1) do
seg_ex1 (fonc j v.(2)) (fonc j v.(3))
done;;

let faire_courbe f pas inf sup =
(*let OH = v_rot_un t p in*)let l = ref [] and i = ref inf 
and n = (sup -. inf) and v = ref (f inf) in 
while !i <= sup do l := f(!i) :: !l; i:= !i +. pas done; !l;;

let tracer_courbe O base l1 = let OH = base.(2) in
let fonc = ecran_of_3d_proj_ex2 O base (*t2 ech*) (*and
seg = segment O OH a b*) in
moveto (fonc(hd l1)).(0) (fonc(hd l1)).(1); let c = devant OH ((hd l1)-$O) in
let rec aux y l = match y,l with _,[] -> ()
|true, (a::b) -> begin let k = hd l in 
if not devant OH (k-$O) then aux false (tl l) 
else let M=fonc k in lineto M.(0) M.(1); aux true (tl l) end
|_-> begin let k = hd l in 
if not devant OH (k-$O) then aux false (tl l) else 
let M=fonc k in moveto M.(0) M.(1); aux true (tl l) end
in aux c l1;;

let faire_surface f pasx pasy infx supx infy supy = 
let nx = intof ((supx -. infx)/.pasx) and ny = intof ((supy -. infy)/.pasy) in 
let mat = make_matrix nx ny [|0.;0.;0.|] 
and i = ref infx and j = ref infy in
for a = 0 to nx-1 do
j := infy; 
for b = 0 to ny-1 do
mat.(a).(b)<- (f !i !j);
j := !j +. pasy
done; i:= !i +. pasx; done;(mat,nx,ny);;

let f_ex3 x y = [|2.+.x;y;sin (x)+. sin y |];;
let surf_ex1 = faire_surface f_ex3 0.4  0.4 0. (4.*.pi) 0. (4.*.pi);;
let f_ex2 x = [|3.+.(cos x); sin x; x/.10.|];;
let courbe_ex1 = faire_courbe f_ex2 0.2 0. (10.*.pi);;

let tracer_surface_ex1 O base (mat, nx, ny)= 
let fonc = ecran_of_3d_proj_ex2 O base in let M = ref (fonc (mat.(0).(0))) and g = ref true in
for i = 0 to nx-1 do let t = (fonc (mat.(i).(0) )) in moveto t.(0) t.(1);
for j = 0 to ny -1 do if devant base.(2) (mat.(i).(j) -$ O) then begin
M := (fonc ( mat.(i).(j) )); if !g then
lineto !M.(0) !M.(1) else g := true; moveto !M.(0) !M.(1)  end
else g := false done; done;
for j =  0 to ny-1 do let t = (fonc (mat.(0).(j) )) in moveto t.(0) t.(1);
for i = 0 to nx -1 do if devant base.(2) (mat.(i).(j) -$ O) then begin
M := (fonc ( mat.(i).(j) )); if !g then
lineto !M.(0) !M.(1) else g := true; moveto !M.(0) !M.(1)  end
else g := false done done;;

let rep_pave O base a b = let ax = a.(0) and ay = a.(1) and az=a.(2)
and bx = a.(0)+.b.(0) and by = a.(1)+.b.(1) and bz =a.(2)+. b.(2) in
let seg = segment O base in
seg a [|bx;ay;az|];seg a [|ax;by;az|];seg a [|ax;ay;bz|];
seg [|bx;by;bz|] [|ax;by;bz|];seg [|bx;by;bz|] [|bx;ay;bz|];seg [|bx;by;bz|] [|bx;by;az|];
seg [|bx;ay;az|] [|bx;by;az|];seg [|bx;by;az|] [|ax;by;az|];seg [|bx;ay;az|] [|bx;ay;bz|];
seg [|ax;ay ;bz|] [|ax;by ;bz|];seg [|ax;by ;bz|] [|ax;by;az|];seg [|ax;ay;bz|] [|bx;ay;bz|];;

reinit();;
rep_pave [|0.;0.;1.|] base_ex1 [|3.;1.;0.|] [|2.;2.;2.|];;

let rep_horizon p t2 = let r = -.100. *. tan p in
let ct2 = cos t2 and st2 = sin t2 in 
moveto (400+ intof ( (r*.st2) -. (1132.*.ct2) ) ) (400 + intof ( (r*.ct2) +. (1132.*.st2) ) ) ;
lineto (400+ intof ( (r*.st2) +. (1132.*.ct2) ) ) (400 + intof ( (r*.ct2) -. (1132.*.st2) ) ) ;;

(*let interactif_ex1 = interactif O_ex1 base_ex1*);;

let interactif ()(*lsurfaces lcourbes lpaves lquadrillages lampes(avec intensités qui se perd)*) = 
set_font "Arial"; set_text_size 20; (*let angle_dep = angle base_dep in*)
let O = ref O_ex1(*O_dep*) and x= ref (xecr/2) and y = ref (yecr/2)
and t = ref 0.(*angle_dep.(0)*) and p = ref 0.(*angle_dep.(1)*) and t2 = ref 0.(*angle_dep.(2)*) in 
let base = ref base_ex1(*base_dep*) (*base_orth !t !p !t2 *) and faux_button = ref false and faux_key = ref false in

let faire () =
clear_graph(); set_color red; draw_circle (xecr/2) (yecr/2) 3;  draw_circle !x !y 3; set_color black;
rep_horizon !p !t2;
(*quadrillage !O !base [|-15;15;-15;15|];*)
(*tracer_surface_ex1 !O !base surf_ex1;*)
rep_pave !O !base [|3.;1.;0.|] [|2.;-.2.;2.|];
ecrire (string_of_float !O.(0) ^"   " ^ string_of_float !O.(1)^"   "^string_of_float !O.(2)) 0 120;
ecrire (string_of_float !base.(2).(0) ^"   " ^ string_of_float !base.(2).(1)^"   "^
string_of_float !base.(2).(2)) 0 90; presente(); wait 0.1 in

try
faire();
while true do
let e = wait_next_event [Button_down;Button_up;Key_pressed] in

if e.button then begin
faux_button := true;
while !faux_button do
 x := fst (mouse_pos()) ;y := snd (mouse_pos());
 if !x < 800 then begin
 t:= !t -. ( ( (flo (!x - 400)) /. 400. )*.0.1 );
 p:= !p+.( ( (flo (!y - 400)) /. 400. )*.0.1 );
 base := base_orth !t !p !t2;
 end
 else if !y> 500 then begin 
 O := !O +$ ( ((flo (!x - 950)) /. 150.*. 0.4) *$ !base.(0) );
 O := !O +$ ( ( ( (flo (!y - 650)) /. 150. )*.0.4 ) *$ !base.(1) );
 end
 else if !y>300 then begin
 O := !O +$  ( ( ( (flo (!x - 950)) /. 150. )*.0.4 ) *$ !base.(2) );
 end
 else if !y> 100 then begin
 t2 := !t2 -. ( ( (flo (!x - 950)) /. 150. )*.0.1 ); base := base_orth !t !p !t2;
 end
 else begin t2:= 0.; base := base_orth !t !p !t2; faire(); end;
faire();
faux_button := button_down();
done
end

else if e.keypressed then begin 
faux_key := appartient (int_of_char (e.key)) [43;45;50;52;54;56];
while !faux_key do let lettre = read_key() in
 if lettre = `4` then O := !O +$ (0.4*$[|-.sin !t ; cos !t ;0.|])
else if lettre = `6` then O := !O +$ (0.4*$[| sin !t; -. cos !t;0.|])
else if lettre = `2` then O := !O +$ (0.4*$[|-.cos !t; -. sin !t ;0.|])
else if lettre = `8` then O := !O +$ (0.4*$[|cos !t ; sin !t ;0.|])
else if lettre = `-` then O := !O +$ [|0.;0.;-.0.4|]
else if lettre = `+` then O := !O +$ [|0.;0.;0.4|];
faire();
faux_key := key_pressed();
done;
end

done; ([||],[||])
with 
Graphic_failure "graphic screen not opened" -> (!O,!base)  ;;

reinit();;
interactif();;

let faire_cercle O C v r =;;
let rep_poly O t p l =
let fill_poly_3d col O t p ech l =
let paysage O t p ech polys_vide polys_pleins(*+coul*) segts_isoles(**) =
let bouge_interactif polys_vide polys_pleins segts_isoles courbes =

(*rep_horizon puis ...*)

let tangente_surf f x y ... =

