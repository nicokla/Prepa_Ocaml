

(*------------------------------*)
(*carrés emboités par anthony*)
let add (a, b) (c, d) =
	(a +. c, b +. d);;

let mult (a, b) (c, d) =
	(a *. c -. b *. d, a *. d +. b *. c);;

let inv (a, b) =
	(a /. (a *. a +. b *. b), (-. b) /. (a *. a +. b *. b));;

let div (a, b) (c, d) =
	mult (a, b) (inv (c, d));;

let opp (a, b) =
	(-. a, -. b);;

let soust (a, b) (c, d) =
	(a -. c, b -. d);;

let fun_a (a,b) (c,d) =
 div (soust (a,b) (c,d)) (a,b);;

let flo (a, b) =
	(float_of_int a, float_of_int b);;

let int (a,b) =
	(int_of_float a, int_of_float b);;


#open "graphics";;
open_graph "500 * 500";;

let moveto2 (a, b) =
	moveto (int_of_float a) (int_of_float b);;

let lineto2 (a, b) =
	lineto (int_of_float a) (int_of_float b);;

let iteration n =
	clear_graph();
	let zc = (500., 500.) in
		let coté = 2. *. (fst zc) in
			let z0 = ref (0., 0.)
			and z1 = ref (coté, 0.)
			and z2 = ref (coté, coté)
			and z3 = ref (0., coté) in
				moveto2 !z0; lineto2 !z1; lineto2 !z2; lineto2 !z3; lineto2 !z0;
				while button_down() = false do
					let z0 = ref (0., 0.)
					and z1 = ref (coté, 0.)
					and z2 = ref (coté, coté)
					and z3 = ref (0., coté) in
						moveto2 !z0; lineto2 !z1; lineto2 !z2; lineto2 !z3; lineto2 !z0;
						clear_graph();
						for i = 1 to n do
							let zs = ref (flo (mouse_pos())) in
								let a = (fun_a zc !zs) and b = !zs in
									z0 := (add (mult a !z0) b);
									z1 := (add (mult a !z1) b);
									z2 := (add (mult a !z2) b);
									z3 := (add (mult a !z3) b);
									moveto2 !z0; lineto2 !z1; lineto2 !z2; lineto2 !z3; lineto2 !z0;
						done; done;;

iteration 1000;;

(*--------------------------------*)
(*Sierpinski triangle, a faire mieux*)

#open "graphics";;
open_graph "";;

let bloc l x2 y2=
fill_rect (l*x2) ((l*y2)-1) l l;;



let repmat mat l =
open_graph "";
  let n = vect_length mat in
    (*open_graph ((str (n * l)) ^ "*" ^ (str (n * l)));*)
    clear_graph();
    for i = 0 to (n - 1) do
      for j = 0 to (n - 1) do
        set_color mat.(i).(j);
        bloc l i j;
      done; done;;


let coller m1 m2=
let a=(vect_length m1) and b=(vect_length m2) in
if (a=b) then
let m=make_vect a [||] in
for i=0 to (a-1) do m.(i)<-(concat_vect m1.(i) m2.(i)) done;
m
else [||];;


let sierpinsky n=
let m0 = [|[|black|]|] in
let rec sisi k=match k with
|0->m0
|_-> concat_vect (coller (sisi (k-1)) (sisi (k-1))) 
( coller (sisi (k-1)) 
(make_matrix (vect_length(sisi (k-1))) (vect_length(sisi (k-1))) white) ) in
sisi n;;


repmat (sierpinsky 8) 1;;

(*-------------------------*)