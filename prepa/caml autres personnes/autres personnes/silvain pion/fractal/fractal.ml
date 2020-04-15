
#open "graphics";;
open_graph"";;
  (* Programme pour tracer des fractales definies par un polygone (en fait la
      ligne brisee de depart, pas forcement fermee), et une ligne brisee, qui
      definit le motif recursif. *)

(* le polygone est defini comme une liste de points, dans [0;1]^2 *)
(* la ligne ca peut sortir, mais le premier est (0,0), *)
(* et le dernier est (1,0). *)

let tiers = 1. /. 3.;;
let dx_3  = 2. /. 3.;;
let pi = 3.1415927;;

(* Trois exemples calcules. *)

let koch_ligne =
  ref [(0. ,0.);(tiers,0.);(0.5 ,sqrt(3.) /. 6.);(dx_3,0.);(1. ,0.)];;
let koch_poly = ref [(0.2,0.6);(0.8,0.6);(0.5,0.05);(0.2,0.6)];;


let fili_ligne =
  ref [(0. ,0.);(tiers,0.);(tiers,tiers);(dx_3,tiers);(dx_3,0.);(1. ,0.)];;
let fili_poly = ref [(0.1,0.1);(0.9,0.1);(0.9,0.9);(0.1,0.9);(0.1,0.1)];;


let cercle_ligne =
  let p = 5. in
  let c = cos (pi /. p)
  and s = sin (pi /. p)
  in
  ref [(0.,0.);((1. -. c)/.2.,s /. 2.);((1. +. c)/.2.,0. -. s /. 2.);(1.,0.)];;

let cercle_poly = ref [(0.2,0.5);(0.9,0.5)];;



let ligne = cercle_ligne;;
let poly = cercle_poly;;

let homo =
  map (fun (x,y) -> (x *. float_of_int ( min (size_x()) (size_y())),
                     y *. float_of_int ( min (size_x()) (size_y())) ) );;

let des_homo =
  map (fun (x,y) -> (x /. float_of_int ( min (size_x()) (size_y())),
                     y /. float_of_int ( min (size_x()) (size_y())) ) );;

let go pt = moveto (int_of_float (fst pt)) (int_of_float (snd pt));;

let droite pt = lineto (int_of_float (fst pt)) (int_of_float (snd pt));;

  (* Alors, la on file les coordonnes d'un segment, et les coordonnes d'un
      point, mais qui est "par rapport au segment unite", et on calcule les
      coordonnes du nouveau point, par rapport au premier, c clair, non? *)

let coor ori arr (a,b) =
  let u = fst arr -. fst ori
  and v = snd arr -. snd ori
  in  (fst ori +. (u *. a) -. (v *. b),
       snd ori +. (v *. a) +. (u *. b));;

let rec trace orig arr =
  fun  _            0 -> ( go orig ; droite arr )
    | [a]           _ -> ()
    | (pta::ptb::l) n -> ( trace (coor orig arr pta) (coor orig arr ptb) !ligne
                                                                         (n-1);
                           trace orig arr (ptb::l) n );;

let rec global =
  fun (a::b::l) n -> trace a b !ligne n; global (b::l) n
    | _         _ -> ();;

(* Pour lancer: global (homo !poly) 5;; *)


global (homo !poly) 4;;

clear_graph();;
