type arbgen = R of int * arbgen list;;

type arbbin = Nil|Noeud of int * arbbin * arbbin;;
R(1,[])  ;;
let arbre1 = (R(3,
[R(4 , [R(1,[])] );
R(1, [ R(1,[]) ; R(2,[]) ]) ;
R(0,[R(5,[ R(-1,[]);R(4,[]) ] ) ] ) ]) );;

let arbre2 =
(R(3,[ R(1,[R(2,[])]); R(4,[R(5,[]);R(-1,[])])]));;

let rec preordre arbre =
  let rec aux (a::b) =
    match b with [] -> [preordre a]
      | _ -> (preordre a) @ (aux b) in
    match arbre with
      | R (k, []) -> [k]
      | R (k, l) -> k :: aux l;;

preordre arbre1;;

let rec preordre2 (R (x, l)) =
  let res = ref [x] and lis = ref l in
    while !lis <> [] do
      res := (!res) @ preordre (hd (!lis));
      lis := tl !lis;
    done;
    !res;;

let preordre3 (R (x, lis)) =
  let rec pre = function
      | [] -> []
      | R (x, l) :: q -> x :: (pre l) @ (pre q)
  in x :: (pre lis);;

let preordre4 (R (x, lis)) (*le meilleur*)=
  let res = ref [] in
    let rec pre = function
        | [] -> ();
        | R (x, l) :: q -> begin (pre q); (pre l); res := x :: (!res); end;
    in pre lis; (x :: !res);;

let preordre5 (R (x, lis)) =
  let rec pre f acc = match f with [] -> acc
      | R (x, l) :: q -> x :: (pre l (pre q acc));
  in x :: (pre lis []);;

let rec postordre (R (x, lis)) =
  let res = ref [] in
    let rec post = function [] -> ();
        | R (a, l) :: q -> begin post q;
              res := x :: !res; post l; end;
    in post lis; !res;;

(*-------------------------*)

let rec f = function
    | [] -> []
    | R (x, l) :: q -> x :: f (q @ l);;

let rec present (R (x, l)) n =
  x = n ||
  (let ok = ref false and lis = ref l in
      while not !ok && !lis <> [] do
        ok := present (hd !lis) x;
        lis := tl !lis;
      done; !ok;);;

let rec adresse (R (x, l)) n =
  if x = n then (true, []) else
    (let ok = ref false and lis = ref l and nb = ref 0 and pos = ref [] in
        while not !ok && !lis <> [] do
          let v1, v2 = adresse (hd !lis) n in (!ok = v1, pos := v2);
            nb := !nb + 1;
            lis := tl !lis;
        done; if !ok then (!ok, !nb :: !pos) else (false, []));;

let rec donne (R (a, l)) pos = match pos with
    | [] -> a;
    | n :: q -> let i = ref (n - 1) and lis = ref l in
          while !i > 0 do lis := tl !lis; i := !i + 1 done;
          donne (hd !lis) q;;

let rec bin_of_foret foret =
  match foret with [] -> Nil
    | R (x, l) :: q -> Noeud (x, bin_of_foret l, bin_of_foret q);;

let rec foret_of_bin bin =
  match bin with Nil -> []
    | Noeud (x, l, q) -> R (x, foret_of_bin l) :: foret_of_bin q;; 

let representer arbre_bin =

