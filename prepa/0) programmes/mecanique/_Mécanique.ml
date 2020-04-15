#open "graphics";;
open_graph "1000*1000";; 
clear_graph();;

let int=int_of_float
and flo=float_of_int;;

(*------------------*)
(*Gravité made by anthony*)
#open "graphics";;
open_graph "500 * 1000";;

let int = int_of_float;;
let flo = float_of_int;;

let dist x y =
match (x,y) with
| (0.,0.) -> 0.
| (x,y) -> (((x *. x) +. (y *. y)) ** (3./.2.));;

let rec mod2 a b =
match a with
| a when a <. 0. -> (mod2 (a +. b) b)
| a when a <. b -> a
| _ -> (mod2 (a -. b) b);;

let ressort3 x y l dt =
  clear_graph();
  set_line_width 3;
  let x0 = ref x and y0 = ref y
  and x1 = ref 0. and y1 = ref 0.1
  and x2 = ref 0. and y2 = ref 0.
  and xf = ref 0. and yf = ref 0.
	and k=ref 0 and touche = ref false and tropvite = ref false in
    while button_down() = false && not (!touche || !tropvite) do
       let a = ref (flo (fst (mouse_pos())))
      and b = ref (flo (snd (mouse_pos())))
      in let d = dist (!a -. !x0) (!b -. !y0) in
          x2 := (!a -. !x0) /. d -. (l *. !x1);
          y2 := (!b -. !y0) /. d -. (l *. !y1);
          x1 := !x1 +. (dt *. !x2);
          y1 := !y1 +. (dt *. !y2);
					if (!k mod 4)=0 then begin xf:=!x0;
					yf:=!y0 ;end; k:=(!k+1) mod 4;
 					x0 := !x0 +. (dt *. !x1);
          y0 := !y0 +. (dt *. !y1);
          x0 := mod2 (!x0) 1200.;
          y0 := mod2 (!y0) 600.;      
let u = int !x0 and v = int !y0 
and uf= int !xf and vf = int !yf in
            touche := (u = fst (mouse_pos()) && v = snd (mouse_pos()));
            (*tropvite := (!x1 >. 1. || !y1 >. 1.);*)
set_color black;plot u v;(*fill_rect u v 3 3; set_color white;fill_rect uf vf 3 3;*)
    done;;

ressort3 200. 200. 0. 0.02;;
clear_graph();;

(*------------------------------*)
(*ressort made by anthony*)

let int=int_of_float
and flo=float_of_int;;

let ressort dt fr =open_graph "1000*1000";
  let mx = ref (flo (fst (mouse_pos()))) and
  my = ref (flo (snd (mouse_pos()))) and
  ax = ref 0. and ay = ref 0. and vx = ref 0. and vy = ref 0. (* and v=ref 0.*) in
    moveto (int (!mx)) (int (!my));
    while (button_down() = false) do
      let sx = flo (fst (mouse_pos())) and
      sy = flo (snd (mouse_pos()))
        (* and v:=sqrt((vx*vx)+(vy*vy)) *) in
        ax := sx -. (!mx) -. (fr *. (!vx)); vx := (!vx) +. (dt *. (!ax)); mx := (!mx) +. (dt *. (!vx));
        ay := sy -. (!my) -. (fr *. (!vy)); vy := (!vy) +. (dt *. (!ay)); my := (!my) +. (dt *. (!vy));
        plot (int (!mx)) (int (!my));
    done;;

ressort 0.0002 0.1;;

(*-----------------------------*)

