include "biblio/utile/poly_complexes.ml";;

let plus_proche l (x, y) =
  let rec toubib2 l k e d = match l with
      [] -> k,d
      | a :: b -> let d2 = dist2 (x, y) a in
            if d2 < d then toubib2 b e (e + 1) d2
            else toubib2 b k (e + 1) d
  in toubib2 l 1 1 (dist2 (x, y) (hd l));;

let l = [(1.,0.);(-.0.5,-.(sqrt 3.)/.2.);(-.0.5,(sqrt 3.)/.2.)];;
let l2 =  [(1.,0.);(-.0.5,-.0.5);(-.0.5,0.5)];;

let test l ray f n (x, y) = let i = ref 0 in
    let x1 = ref x
    and y1 = ref y in
      let trouve = ref false in
        while not !trouve && (!i < n) do
          let z = (!x1, !y1) in
            let j = f z 
            in x1 := fst j; y1 := snd j;
              i := !i + 1;
              let a = plus_proche l (!x1, !y1) in
                if snd a <= ray then trouve := true
        done;
        if !trouve then fst (plus_proche l (!x1, !y1))
        else 0;;

let test2 l ray f1 f2 n (x, y) = let i = ref 0 in
    let a = ref (x, y) in
      let trouve = ref false in
        while not !trouve && (!i < n) do
          let z2 = f1 !a in
            i := !i + 1;
            a := !a ^- (z2 ^/ (f2 !a));
            if ((0., 0.) ^? z2) <= ray then trouve := true;
        done;
        if !trouve then fst (plus_proche l !a)
        else 0;;

let frac_zoom reg ray l n (tailx, taily) (debx, finx) (deby, finy) =
  open2 (tailx, taily);
  let coord = coord_of_pixel (tailx, taily) (debx, finx) (deby, finy)
  and f1 = poly_of_racinesc l in
    let f2 = derivec f1 in
      let f11 = fonc_of_polc f1 and f22 = fonc_of_polc f2 in
        let f x = x ^- ((f11 x) ^/ (f22 x)) in
          for i = 0 to tailx do
            for j = 0 to taily do
              let k = coord (i, j) in
                let m = (test l ray f n k)
                  (*test2 l ray f11 f22 n k*) in
                  reg m;
                  plot i j;
            done; done;;

let regle1 t= 
match t with
0->set_color white
|1->set_color blue
|2->set_color red
|_->set_color green;;

frac_zoom regle1 0.2 l2 15 (300, 300) (-. 1., 1.) (-. 1., 1.);;

(**)

(*derivee fonc quelconque epsilon*)