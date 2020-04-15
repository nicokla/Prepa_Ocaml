
(* Trace de fonctions f: x -> y *)

#open "graphics";;

open_graph;;

open_graph "";;

let orig_x = size_x()/2
and orig_y = size_y()/2;;

let efface () = clear_graph ();
             moveto 0 orig_y;lineto (size_x()) orig_y;
             moveto orig_x 0;lineto (orig_x) (size_x());;

let draw f =  let ech_x = 40.0 and ech_y = 40.0 
                     (* nbre de points graphiques par unite *)
 in for i = 0 to size_x() do 
   plot i (orig_y + int_of_float(ech_y*.(f ((float_of_int(i-orig_x))/.ech_x))))
 done;;



efface (); draw (fun x -> 2.0*.x+. 1.0);;

draw (fun x-> (atan2 (0.0-.1.0) x));;

close_graph ();;



(* Trace de fonctions f: (x,y) -> z *)

#open "graphics";;

open_graph;;

open_graph "";;

let efface () = clear_graph ();;

let orig_x = size_x()/2
and orig_y = size_y()/2

and x_x = minus_float(sqrt 0.5) and x_y = minus_float(sqrt 0.5)
and y_x = 1.0 and y_y = 0.0
and z_x = 0.0 and z_y = 1.0
and ech_x = 50.0 and ech_y = 50.0 and ech_z = 50.0

and x_min = -5.0 and x_max = 5.0 and pas_x = 100
and y_min = -5.0 and y_max = 5.0 and pas_y = 100

and x = ref 1.0
and y = ref 1.0
and z = ref 1.0;;


let point a b c =
 plot (orig_x + int_of_float(a*.x_x*.ech_x +.b*.y_x*.ech_y +.c*.z_x*.ech_z))
      (orig_y + int_of_float(a*.x_y*.ech_x +.b*.y_y*.ech_y +.c*.z_y*.ech_z));;


let draw f =  
    for i = 0 to pas_x do 
    for j = 0 to pas_y do 
begin
   x := x_min +. (x_max-.x_min)*.(float_of_int(i)/.float_of_int(pas_x));
   y := y_min +. (y_max-.y_min)*.(float_of_int(j)/.float_of_int(pas_y));
   z := f(!x,!y);
   point !x !y !z
end 
    done 
    done;;

efface ();;

draw (fun (x,y) -> x+.y);;

draw (fun (x,y) -> atan2 x y);;

close_graph ();;



