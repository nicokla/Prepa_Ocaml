
#open"graphics";;
let open2 (tailx, taily) =
let stri = string_of_int in
  let tailx1 = stri tailx and taily1 = stri taily in
    open_graph (tailx1 ^ "x" ^ taily1);;

let deplacer droite haut=
let k = current_point() in
moveto (fst k + droite) (snd k + haut);;

let marquer_échelle (*opthor optver*)(*a ameliorer !*)
tailx taily infx supx infy supy =
  let stri = string_of_int in
    set_color blue; set_font "Arial"; set_text_size 8;
    moveto 0 2;
    (*if coté = true then*)
    let a = ref infx and b = tailx / 12 in
      let c = (supx - infx) / b in
        while !a <= supx do
          draw_string (stri !a);
          deplacer 0 10; let g = current_point() in
            moveto 10 (snd g);
            a := !a + c done;
        moveto 0 (taily - 20);
        let d = ref infy and e = taily / 8 in
          let f = (supy - infy) / e in
            while !d <= supy do
              draw_string (stri !d);
              deplacer 4 0; let h = current_point() in
                moveto (fst h) (taily - 20);
                d := !d + f done;;

(*moveto 20 200;;
draw_string "8";;
current_point();;*)


let draw_discret f inf sup taily echy (*entiers nat supposés*) =
  let tailx1 = min (sup - inf + 1) 1000 in
    open2 (tailx1, taily); set_color black;
    let i = ref inf
    and pas = ((sup - inf + 1) / 1000) + 1 in
      while !i <= sup do
        moveto (!i / pas) 0; lineto (!i / pas) (f (!i) / echy);
        i := !i + pas
      done;
      marquer_échelle tailx1 taily inf sup 0 (echy * taily);;



