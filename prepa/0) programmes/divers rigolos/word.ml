(*textes*)

#open "char";;
#open "graphics";;

(*version1 :
let ecrire taille  couleur (a, b) =
  open_graph "";
  set_color couleur;
  set_text_size taille;
  moveto a b;
  plot a b;
  let m = ref "" and c = ref true in
    try
      while !c do
        let e = wait_next_event [Button_down; Key_pressed] in
          if e.keypressed then let j = e.key in
              m := (!m) ^ (string_of_char j);
              draw_char j;
              
      done;!m
    with Graphic_failure "graphic screen not opened" -> !m;;
*)
char_of_int 8;;
int_of_char `\013`;;
set_text_size 20;;
text_size "b";;


let ecrire taille style couleur (a, b) =
  open_graph "";
  set_color couleur;
  set_font style;
  set_text_size taille;
  let q1 = snd (text_size "a") in
    let (x0, y0) = (size_x(), size_y()) in
      moveto a b;
      plot a b;
      let m = ref "" and c = ref true and n = ref 0 and k = ref [(a, b)] in
        try
          while !c do
            let e = wait_next_event [Button_down; Key_pressed] in
              if e.keypressed then let j = e.key in
                  match j with
                    | t when t = char_of_int 8 (*effacer*) ->
                        if !n <> 0 && !m.[!n] <> `\n` then
                          begin let (u, v) = current_point() in
                              let (p, q) = text_size (string_of_char (!m.[!n - 1])) in
                                let h = max (u - p) 0 in
                                  set_color white;
                                  fill_rect h v p q; moveto h v;
                                  m := sub_string (!m) 0 (!n - 1); n := !n - 1;
                                  set_color black end
                        else if !m.[(!n) - 1] = `\n` then begin moveto (fst (hd !k)) (snd (hd !k));
                            k := tl !k;
                            m := sub_string (!m) 0 (!n - 1);
                            n := !n - 1 end
                    | t when t =`\013`(*saute ligne*) -> let t = current_point() in
                          moveto a (snd (t) - q1);
                          m := (!m) ^ (string_of_char (`\n`));
                          n := !n + 1;
                          k := t :: (!k);
                    | t when t =`\027`(*echap*) -> c := false
                    | _ -> begin m := (!m) ^ (string_of_char j); n := !n + 1;
                          draw_char j;
                          (*draw_string (string_of_int (int_of_char j));*) end
          done; !m
        with Graphic_failure "graphic screen not opened" -> !m;;

ecrire 20 "Arial" black (50,300);;


let ecrire taille style couleur (a, b) =
  open_graph "";
  set_color couleur;
  set_font style;
  set_text_size taille;
  let q1 = snd (text_size "a") in
    let (x0, y0) = (size_x(), size_y()) in
      moveto a b;
      plot a b;
      let m = ref "" and c = ref true and n = ref 0 in
        try
          while !c do
            let e = wait_next_event [Button_down; Key_pressed] in
              if e.keypressed then let j = e.key in
                  match j with
                    | t when t = char_of_int 8  -> if !n <> 0 then
                          begin let (u, v) = current_point() in
                              let (p, q) = text_size (string_of_char (!m.[!n - 1])) in
                                let h = max (u - p) 0 in
                                  set_color white;
                                  fill_rect h v p q; moveto h v;
                                  m := sub_string (!m) 0 (!n - 1); n := !n - 1;
                                  set_color black; end
                    | t when t =`\013` ->
                        moveto a (snd (current_point()) - q1);
                        m := (!m) ^ (string_of_char (`\n`));
                        n := !n + 1;
                    | t when t =`\027` -> c := false
                    | _ -> begin m := (!m) ^ (string_of_char j); n := !n + 1;
                          draw_char j;
                          draw_string (string_of_int (int_of_char j)); end
          done; !m
        with Graphic_failure "graphic screen not opened" -> !m;;



