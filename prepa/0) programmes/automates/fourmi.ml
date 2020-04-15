include "bibliothèques/Graphique.ml";;
let coul_fond = black;;
let coul_dessin = rgb 90 235 235;;


let fourmi t a b (*pix,l,h,temps*)
 = let ploplo = bloc t in
    open2 (t * a, t * b); set_color coul_fond;
    fill_rect 0 0 (t * a) (t * b); set_color coul_dessin;
    let m = make_matrix a b false in
      let x = ref (a / 2) and y = ref (b / 2) and h = ref 1 in
        m.(!x).(!y) <- true; ploplo (!x) (!y);
        while true do
          let k = m.(!x).(!y) in
            m.(!x).(!y) <- not k;
            if k then set_color coul_fond else
              set_color coul_dessin;
            ploplo (!x) (!y);
            if k then begin h := mod2 4 (!h + 1) end
            else begin h := mod2 4 (!h - 1) end;
            if !h = 0 then y := mod2 b (!y + 1)
            else if !h = 1 then x := mod2 a (!x + 1)
            else if !h = 2 then y := mod2 b (!y - 1)
            else if !h = 3 then x := mod2 a (!x - 1);
        done;;(*faire sans mat*)

fourmi 2 300 301;;

(*marche aléatoire...*)
include "bibliothèques/aléatoire.ml";;

let marche_al n =
  open2 (500, 500);
  moveto 250 250;
  let u = random_list 0 3 n in
    let rec deplier l =
      match l with
        | [] -> ()
        | a :: b -> begin if a = 0 then deplacer2 0 1
              else if a = 1 then deplacer2 1 0
              else if a = 2 then deplacer2 0 (- 1)
              else deplacer2 (- 1) 0 end;
            deplier b in
      deplier u;;

marche_al 100000;;

