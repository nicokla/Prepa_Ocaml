(* ************ algebre des nombres complexes ************ *)

let add (a, b) (c, d) =
  (a +. c, b +. d);;

let opp (a, b) =
  (-. a, -. b);;

let soust (a, b) (c, d) =
  (a -. c, b -. d);;

let prod_r a (b, c) = (* r pour dire avec un scalaire *)
  (a *. b, a *. c);;

let prod_c (a, b) (c, d) = (* c pour dire avec un complexe *)
  (a *. c -. b *. d, a *. d +. b *. c);;

let carre (a, b) =
  (a *. a -. b *. b, 2. *. a *. b);;

let norm (a, b) =
  sqrt (a *. a +. b *. b);;

let normcarre (a, b) =
  a *. a +. b *. b;;

let Re = fst and Im = snd;;

let conj (a, b) =
  (a, -. b);;

let inv (a, b) =
  (a /. (a *. a +. b *. b), -. b /. (a *. a +. b *. b));;

let div z1 z2 =
  prod_c z1 (inv z2);;

let f_rationnelle a b c d z = (* (az+b)/(cz+d) *)
  div (add (prod_c a z) b) (add (prod_c c z) d);;

let carre (a, b) =
  (a *. a -. b *. b, 2. *. a *. b);;

let rec puiss z n =
  match n with
    | n when n < 0 -> puiss (inv z) (- n);
    | 0 -> (1., 0.)
    | 1 -> z
    | _ -> match n mod 2 with
          | 0 -> puiss (carre z) (n / 2);
          | _ -> prod_c z (puiss (carre z) (n / 2));;

let racines_carres (a, b) =
  match (a, b) with
    | (0., 0.) -> [|(0., 0.)|];
    | (a, 0.) when (a >. 0.) -> [|(sqrt a, 0.); (-. (sqrt a), 0.)|];
    | (a, 0.) -> [|(0., sqrt a); (0., -. (sqrt a))|];
    | (a, b) -> let x = (sqrt ((a +. (norm (a, b))) /. 2.)) and y = (sqrt ((-. a +. (norm (a, b))) /. 2.)) in
          [|(x, y); (-. x, -. y)|];;



(* ************ polynomes complexes ************ *)

let polynome_f t z = (* t = [|an,a(n-1)...,a1,a0|] , P = anz^n + a(n-1)z^(n-1) + ... + a1z + a0 *)
  let n = (vect_length t) - 1 in
    let p = ref t.(0) in
      for i = 1 to n do
        p := (add (prod_c !p z) t.(i));
      done; !p;;

let int = int_of_float and flo = float_of_int;;

(* dérive le polynome vecteur *)
let polynome_derive t =
  let n = ((vect_length t) - 1) in
    match n with
      | 0 -> [|(0., 0.)|];
      | _ ->
          let v = make_vect n (0., 0.) in
            for k = 0 to n - 1 do
              v.(k) <- prod_r (flo (n - k)) t.(k);
            done; v;;

let polynome_derive_2 t =
  polynome_derive (polynome_derive t);;

let rec polynome_derive_nieme t n =
  match n with
    | 1 -> polynome_derive t;
    | n -> polynome_derive_nieme (polynome_derive t) (n - 1);;

(* image P(z) avec les racines t = [|a0,a1,...,a(n-1)|] *)
let polynome_racine_f t z =
  let n = (vect_length t) in
    let p = ref (soust z t.(0)) in
      for k = 1 to n - 1 do
        p := (prod_c !p (soust z t.(1)));
      done; !p;;

let prod_polynome_monome t r = (* t = [|an ,a(n-1),...,a1,a0|] , (anzn + a(n-1)z(n-1) + ... + a1z + a0) * (z - r) *)
  let n = (vect_length t) - 1 in
    let v = make_vect (n + 2) (0., 0.) in
      v.(0) <- t.(0);
      for i = 1 to n do
        v.(i) <- (soust t.(i) (prod_c r t.(i - 1)));
      done;
      v.(n + 1) <- opp (prod_c t.(n) r);
      v;;

let polynome_racine v = (* v = le vect des n racines, le programme donne le polynome qui a ces n racines *)
  let n = (vect_length v) in
    let p = ref [|(1., 0.)|] in
      for i = 0 to n - 1 do
        p := prod_polynome_monome !p v.(i);
      done; !p;;

let coef_dominant t = (* renvoie (0.,0.) si le polynome est nul *)
  let n = (vect_length t) - 1 in
    let i = ref 0 in
      let a = ref t.(0) in
        while (!a = (0.,0.)) && (!i < n) do
          i := !i + 1; a := t.(!i);
        done; !a;;

let prod_polynome_c a t = (* multiplie les coef du polynome par a *)
  let n = (vect_length t) in
    for i = 0 to n - 1 do
      t.(i) <- prod_c a t.(i);
    done; t;;

let opp_polynome t =
  let n = (vect_length t) in
    for i = 0 to n - 1 do
      t.(i) <- opp t.(i);
    done; t;;

let rendre_unitaire t =
  let a = (inv (coef_dominant t)) in
    prod_polynome_c a t;;

let racourcir t = (* suprime les termes dominants en (0.,0.), ex [|(0.,0.);(0.,0.);(1.,0.)|] -> [|(1.,0.)|] *)
  let n = (vect_length t) - 1 in
    let f t =
      let i = ref 0 in
        let a = ref t.(0) in
          while (!a = (0., 0.)) && (!i < n) do
            i := !i + 1; a := t.(!i);
          done; !i; in
      let ir = f t in
        let v = make_vect (n - ir + 1) (0.,0.) in
          for k = 0 to n - ir do
            v.(k) <- t.(ir + k);
          done; v;;

let add_polynome t1 t2 =
  let n1 = (vect_length t1) - 1 and n2 = (vect_length t2) - 1 in
    let n = (max n1 n2) in
      let v = make_vect (n + 1) (0., 0.) in
        for k = 0 to n do
          match k with
            | k when k > n1 -> v.(n - k) <- t2.(n2 - k);
            | k when k > n2 -> v.(n - k) <- t1.(n1 - k);
            | k -> v.(n - k) <- add t1.(n1 - k) t2.(n2 - k);
        done;
        racourcir v;;

let soust_polynome t1 t2 =
  let n1 = (vect_length t1) - 1 and n2 = (vect_length t2) - 1 in
    let n = (max n1 n2) in
      let v = make_vect (n + 1) (0., 0.) in
        for k = 0 to n do
          match k with
            | k when k > n1 -> v.(n - k) <- (opp t2.(n2 - k));
            | k when k > n2 -> v.(n - k) <- t1.(n1 - k);
            | k -> v.(n - k) <- add t1.(n1 - k) (opp t2.(n2 - k));
        done;
        racourcir v;;

let prod_polynome t1 t2 =
  let n1 = (vect_length t1) - 1 and n2 = (vect_length t2) - 1 in
    let v = make_vect (n1 + n2 + 1) (0., 0.) in
      for i = 0 to n1 do
        for j = 0 to n2 do
          v.(n1 + n2 - i - j) <- (add v.(n1 + n2 - i - j) (prod_c t1.(n1 - i) t2.(n2 - j)));
        done; done; v;;

let carre_polynome t =
  let n = (vect_length t) - 1 in
    let v = make_vect (2 * n + 1) (0., 0.) in
      for i = 0 to n do
        for j = 0 to n do
          v.(2 * n - i - j) <- (add v.(2 * n - i - j) (prod_c t.(n - i) t.(n - j)));
        done; done; v;;

let rec puiss_polynome t n =
  match n with
    | 0 -> [|(1., 0.)|]
    | 1 -> t
    | _ -> match n mod 2 with
          | 0 -> puiss_polynome (carre_polynome t) (n / 2)
          | _ -> prod_polynome t (puiss_polynome (carre_polynome t) (n / 2));;

let add_polynome_constante t a = (* P + a *)
  let n = (vect_length t) - 1 in
    let v = make_vect (n + 1) (0., 0.) in
      for i = 0 to n - 1 do
        v.(i) <- t.(i); done;
      v.(n) <- add t.(n) a;
      v;;

let compose_polynome p q = (* p rond q *)
  let np = (vect_length p) - 1 and nq = (vect_length q) - 1 in
    let v = make_vect (np * nq + 1) (0., 0.) in
      v.(np * nq) <- p.(0);
      for k = 1 to np do
        t = (add_polynome_constante (prod_polynome v q) p.(k));
      done; v;;



(* ************ fontcions rationnelles ************ *)

let rationel_f (t1,t2) z = (* t1 = [|an,...,a0|], t2 = [| bm,...,b0|], F = (anzn + ... + a0)/(bmzm + ... +b0) *)
  div (polynome_f t1 z) (polynome_f t2 z);;

let add_rationel (t1, t2) (t3, t4) =
  (add_polynome (prod_polynome t1 t4) (prod_polynome t2 t3), prod_polynome t2 t4);;

let opp_rationel (t1, t2) =
  (opp_polynome t1, t2);;

let soust_rationel r1 r2 =
  add_rationel r1 (opp_rationel r2);;

let prod_rationel (t1, t2) (t3, t4) =
  (prod_polynome t1 t3, prod_polynome t2 t4);;

let carre_rationel (t1, t2) =
  (carre_polynome t1, carre_polynome t2);;

let inv_rationel (t1, t2) =
  (t2, t1);;

let div_rationel r1 r2 =
  prod_rationel r1 (inv_rationel r2);;

let derive_rationel (t1, t2) =
  (add_polynome (prod_polynome (polynome_derive t1) t2) (prod_polynome t1 (polynome_derive t2)), carre_polynome t2);;



(* ************ fonctions complexes ************ *)

let exp_c (a, b) =
  ((exp a) *. (cos b), (exp a) *. (sin b));;

let ch x = ((exp x) +. (exp (-.x)))/.2. and sh x = ((exp x) -. (exp (-.x)))/.2.;; (* ch et sh dans R *)

let cos_c (a, b) =
  ((cos a) *. (ch b), -. (sin a) *. (sh b));;

let sin_c (a, b) =
  ((sin a) *. (ch b), (cos a) *. (sh b));;

let tan_c z =
  div (sin_c z) (cos_c z);;

let cotan_c z =
  div (cos_c z) (sin_c z);;

let ch_c (a, b) =
  ((ch a) *. (cos b), -. (sh a) *. (sin b));;

let sh_c (a, b) =
  ((sh a) *. (cos b), -. (ch a) *. (sin b));;

let th_c z =
  div (sh_c z) (ch_c z);;

let coth_c z =
  div (ch_c z) (sh_c z);;

let rotation_c w t z = (* centre w, angle t *) (* curifié pour pouvoir considérer la fonction rotation_c w t *)
  add (prod_c (soust z w) (cos t, sin t)) w;;

let homothetie_c w h z = (* cetre w, rapport h *)
  add (prod_r h (soust z w)) w;;

let similitude_c w h t z = (* centre w, rapport h, angle t *)
  homothetie_c w h (rotation_c w t z);;

let inversion_c w z =
  add (inv (soust z w)) w;;

let reflexion_angle_c w t z = (* reflexion par rapport a D passant par w et orienté d'angle t *)
  add (prod_c (conj (soust z w)) (cos (2. *. t), sin (2. *. t))) w;;

let pi = 3.141592653589793238462643383279502884197169399375105;;
let arg (a, b) =
  match (a, b) with
    | 0., 0. -> 0.
    | a, 0. when a <. 0. -> pi
    | a, b -> 2. *. atan (b /. (a +. (norm (a, b))));;

let reflexion_vecteur_c w v z = (* D orienté par v *)
  let t = arg v in
    reflexion_angle_c w t z;;

let reflexion_2points_c w1 w2 z = (* D passant par w1 et w2 *)
  reflexion_vecteur_c w1 (soust w2 w1) z;;



(* ************ complexes plaires ************ *)

let pi = 3.141592653589793238462643383279502884197169399375105;;

let arg (a, b) =
  match (a, b) with
    | 0., 0. -> 0.
    | a, 0. when a <. 0. -> pi
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

(* ************ affichage ************ *)

let print_complex (a, b) =
  let print_imaginaire b =
    match b with
      | 1. -> ();
      | b -> print_float b;
  in
    let p (a, b) =
      match a, b with
        | a, 0. -> print_float a
        | 0., b -> print_imaginaire b; print_char `i`
        | a, b when b <. 0. -> print_float a; print_string " - "; print_imaginaire (-. b); print_char `i`
        | a, b -> print_float a; print_string " + "; print_imaginaire b; print_char `i`;
    in
      p (a, b);
      print_newline();;
