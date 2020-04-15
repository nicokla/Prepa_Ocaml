(*------------------------*)
(*fonctions complexes, anthony*)
let flo=float_of_int;;
let Pi = 3.141592654;;
let pi = Pi;;
let e = 2.718281828;;

let add (a, b) (c, d) =
  (a +. c, b +. d);;

let prefix ^+ (a, b) (c, d) =
  (a +. c, b +. d);;

let prefix ^- (a, b) (c, d) =
  (a -. c, b -. d);;

let prod (a, b) (c, d) = (* c pour dire avec un complexe *)
  (a *. c -. b *. d, a *. d +. b *. c);;

let prefix ^* (a, b) (c, d) =  prod (a, b) (c, d);;

let prod2 a (c, d) =
(a *. c , a *. d );;

let prefix ^& a (c, d) = prod2 a (c, d);;

let norm (a, b) =
  sqrt (a *. a +. b *. b);;

let norm2 (a, b) =
 (a *. a +. b *. b);;

let norm3 (a,b) = max (abs_float a) (abs_float b) ;;
let norm4 (a,b) = max (abs a) (abs b) ;;
let dist3 (a,b) (c,d) = norm3 ((a,b)^-(c,d));;
let dist4 (a,b) (c,d) = norm4 (a-c,b-d);;

let dist (a,b) (c,d) = norm ((a,b)^-(c,d));;

let prefix ^? (a,b) (c,d) = dist (a,b) (c,d) ;;

let dist2 (a,b) (c,d)= norm2 (add (a,b) (-.c,-.d));;

let scal (a,b) (c,d) = (a*.c)+.(b*.d);;

let prefix ^. (a,b) (c,d)=(a*.c)+.(b*.d);;

let antiscal (a,b) (c,d) = (a*.d)-.(b*.c);;

let moins (a,b) = (-.a,-.b);;

let inv (a, b) = let k = ((a *. a) +. (b *. b)) in
    (a /. k, -. b /. k);;

let inv2 (a, b) = let k = ((a *. a) +. (b *. b)) in
    (a /. k, b /. k);;

let div (a,b) (c,d)=
prod (a,b) (inv (c,d));;

let prefix ^/ (a,b) (c,d)=div (a,b) (c,d);;

let f_rationnelle a b c d z = (* (az+b)/(cz+d) *)
  div (add (prod a z) b) (add (prod c z) d);;

let Id = fun (x,y)->(x,y);;

let carre (a, b) =
  (a *. a -. b *. b, 2. *. a *. b);;

let rec puiss z n=
  match n with
    | n when n < 0 -> puiss (inv z) (- n);
    | 0 -> (1., 0.)
    | 1 -> z
    | _ -> match n mod 2 with
          | 0 -> puiss (carre z) (n / 2);
          | _ -> prod z (puiss (carre z) (n / 2));;

let rec prefix ^^ z n=puiss z n;;
 

let racines_carres (a, b) =
  match (a, b) with
    | (0., 0.) -> [|(0., 0.)|];
    | (a, 0.) when (a >. 0.) -> [|(sqrt a, 0.); (-. (sqrt a), 0.)|];
    | (a, 0.) -> [|(0., sqrt a); (0., -. (sqrt a))|];
    | (a, b) -> let x = (sqrt ((a +. (norm (a, b))) /. 2.)) and y = (sqrt ((-. a +. (norm (a, b))) /. 2.)) in
          [|(x, y); (-. x, -. y)|];;

(*------------------------------*)
(*complexe polaire anthony*)

let arg (a, b) =
  match (a, b) with
    | 0., 0. -> 0.
    | a, 0. when a < 0. -> pi
    | a, b -> 2. *. atan (b /. (a +. (norm (a, b))));;

let cart_of_pol (r, th) =
  (r *. (cos th), r *. (sin th));;

let pol_of_cart (x, y) =
  (norm (x, y), arg (x, y));;

let rec mod2pi th =
  match th with
    | th when th >. (-. pi) && th <=. pi -> th
    | th when th >. pi -> mod2pi (th -. pi)
    | _ -> mod2pi (th +. pi);;

let prod_r_pol a (r, th) =
  match a with
    | 0. -> (0., 0.)
    | a when a >. 0. -> (a *. r, th)
    | _ -> (-. a *. r, mod2pi (th +. pi));;

let prod_c_pol (r1, th1) (r2, th2) =
  (r1 *. r2, mod2pi (th1 +. th2));;

let inv_pol (r, th) =
  (1. /. r, -. th);;

let div_pol (r1, th1) (r2, th2) =
  (r1 /. r2, mod2pi (th1 -. th2));;

let puiss_pol_int (r, th) n =
  (r ** (flo n), th *. (flo n));;
