

include "bibliothèques\Graphique";;
(*------------------*)
(*courbe jolie*)

let courbe n dt =
open_graph "1000*1000";
  let flo = float_of_int and
  int = int_of_float in
    let v = make_vect n (flo (fst (mouse_pos())), flo (snd (mouse_pos()))) and
    coul = make_vect n (rgb 255 0 0) in
      for i = 0 to (n - 1) do
        coul.(i) <- (rgb 255 (255 - (i * (255 / n))) 0)
      done;
      while (button_down() = false) do
        let a = flo (fst (mouse_pos())) and
        b = flo (snd (mouse_pos())) in
          v.(0) <- (a, b);
          for i = 1 to (n - 1) do
            let m = fst (v.(i)) +. (dt *. (fst (v.(i - 1)) -. fst (v.(i))))
            and k = snd (v.(i)) +. (dt *. (snd (v.(i - 1)) -. snd (v.(i)))) in
              v.(i) <- (m, k)
          done;
          for i = 0 to (n - 1) do
            set_color coul.(i);
            plot (int (fst (v.(i)))) (int (snd (v.(i))))
          done;
      done;;

courbe 4 0.0002;;
clear_graph();;


(*-----------------------------------------*)
(*trait*)

let trait2 coul ep= 
open_graph "";
set_color coul;
set_line_width ep;
let a= ref true in
while (!a) do
let e=wait_next_event [Key_pressed;Button_down;Button_up;Poll] in
if e.button then let a=fst(mouse_pos()) and
	b= snd (mouse_pos()) in lineto a b;
else if e.key=`a` then a:=false
else if e.button=false then let a=fst(mouse_pos()) and
	b= snd (mouse_pos()) in moveto a b;
done;;

trait2 red 6;;

(*-----------------------------*)
(*deplacer carre*)

(*let deplacer_carre() =
open2(400,400); let a = ref 0 and b = ref 0 in
fill_rect !a !b 100 100;
let c= ref true in
while (!c) do
let e=wait_next_event [Button_down] in
if e.button then let k = mouse_pos() in
if zone a b 100 100 (fst k) (snd k) then
let j = wait_next_event [Button_up;Mouse_motion;Poll] in
*)


