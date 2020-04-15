type etat = mur | source of float | niveau of float;;

#open "graphics";;
open_graph "1000 * 1000";;

let rec mod2 a b =
  match a with
    | a when a < 0 -> mod2 (a + b) b
    | a when a >= b -> mod2 (a - b) b
    | _ -> a;;

let t = make_matrix 50 50 mur;;

let etat_cellule x =
  match x with
    | niveau (a) -> (a, 1)
    | source (a) -> (a, 1)
    | _ -> (0., 0);;

let change_niveau i j v dt =
  let a = ref 0. and b = ref 0 and vxy = ref (0., 0) in
    vxy := (etat_cellule v.(i + 1).(j));
    a := !a +. (fst !vxy); b := !b + (snd !vxy);
    vxy := (etat_cellule v.(i).(j + 1));
    a := !a +. (fst !vxy); b := !b + (snd !vxy);
    vxy := (etat_cellule v.(i - 1).(j));
    a := !a +. (fst !vxy); b := !b + (snd !vxy);
    vxy := (etat_cellule v.(i).(j - 1));
    a := !a +. (fst !vxy); b := !b + (snd !vxy);
    vxy := (etat_cellule v.(i).(j));
    niveau ((fst !vxy) +. dt *. (!a -. (float_of_int !b) *. (fst !vxy)));;

let change i j t dt =
  match t.(i).(j) with
    | mur -> mur
    | source (_) -> t.(i).(j);
    | niveau (_) -> (change_niveau i j t dt);;

let couleur a =
match a with
        | niveau (_) -> set_color green;
        | mur -> set_color black;
        | source (1.) -> set_color yellow;
        | source (_) -> set_color red;;

let plot_t t coté =
  for i = 0 to 49 do
    for j = 0 to 49 do
      couleur t.(i).(j);
      fill_rect (i * 20) (j * 20) coté coté;
    done; done;;

clear_graph();;
plot_t t 19;;

let t = make_matrix 50 50 (niveau (0.5));;
t.(0).(0) <- mur; t.(49).(0) <- mur; t.(0).(49) <- mur; t.(49).(49) <- mur;
for i = 1 to 48 do
  t.(i).(0) <- mur; t.(0).(i) <- mur; t.(i).(49) <- mur; t.(49).(i) <- mur;
done;;

let aaa n = for i = 0 to n do (); done;;

let zone x y lx ly a b =
  match a, b with
    | a, b when (a >= x) && (a < (x + lx)) && (b >= y) && (b < (y + ly)) -> button_down();
    | _ -> false;;

let tout_vert t =
  for i = 1 to 48 do
    for j = 1 to 48 do
      t.(i).(j) <- niveau (0.5);
    done; done;;

let tout_noir t =
  for i = 1 to 48 do
    for j = 1 to 48 do
      t.(i).(j) <- mur;
    done; done;;

let affichage_1() =
  set_color (rgb 150 220 255); fill_rect 1010 590 220 240;
  
  set_color (rgb 255 127 0); fill_rect 1020 960 200 40;
  set_color black; moveto 1030 970; draw_string "Go go, action go!";
  
  set_color green; fill_rect 1020 900 200 40;
  set_color black; moveto 1030 910; draw_string "Tout vert";
  
  set_color black; fill_rect 1020 840 200 40;
  set_color white; moveto 1030 850; draw_string "Tout noir";
  
  set_color green; fill_rect 1020 780 200 40;
  set_color black; moveto 1030 790; draw_string "Vert";
  
  set_color black; fill_rect 1020 720 200 40;
  set_color white; moveto 1030 730; draw_string "Mur";
  
  set_color yellow; fill_rect 1020 660 200 40;
  set_color black; moveto 1030 670; draw_string "Source chaude";
  
  set_color red; fill_rect 1020 600 200 40;
  set_color black; moveto 1030 610; draw_string "Source froide";
  
  set_color cyan; fill_rect 1020 540 200 40; set_color black; fill_rect 1185 545 29 29;
  set_color yellow; moveto 1185 546; lineto 1185 575; set_color red; moveto 1213 546; lineto 1213 575;
  set_color black; moveto 1030 550; draw_string "fluide";
  
  set_color yellow; fill_rect 1020 480 200 40;
  set_color black; moveto 1030 490; draw_string "Paroi chaude";
  
  set_color red; fill_rect 1020 420 200 40;
  set_color black; moveto 1030 430; draw_string "Paroi froide";

  set_color black; fill_rect 1020 360 200 40;
  set_color white; moveto 1030 370; draw_string "Paroi normale";

  set_color (rgb 127 255 0); fill_rect 1020 300 200 40;
  set_color black; moveto 1030 310; draw_string "Labyrinthe";

  set_color (rgb 127 255 0); fill_rect 1020 240 200 40;
  set_color black; moveto 1030 250; draw_string "Labyrinthe2";

  set_color (rgb 127 255 0); fill_rect 1240 960 200 40;
  set_color black; moveto 1250 970; draw_string "Plein";

  set_color (rgb 127 255 0); fill_rect 1240 900 200 40;
  set_color black; moveto 1250 910; draw_string "quadrillage";;


let labyrinthe t =
  for i = 1 to 48 do
    for j = 1 to 48 do
      if ((i mod 3)=0) || (((j+2) mod 3)=0) then t.(i).(j) <- mur;
    done; done;;

let labyrinthe2 t =
  for i = 1 to 48 do
    for j = 1 to 48 do
      if ((i mod 4)=0) || (((j+3) mod 4)=0) then t.(i).(j) <- mur;
    done; done;;

let fluide t =
  for i = 1 to 48 do
    t.(0).(i) <- source (1.);
    t.(49).(i) <- source (0.);
  done;;

let paroi_chaude t =
  t.(0).(0) <- source (1.); t.(0).(49) <- source (1.); t.(49).(0) <- source (1.); t.(49).(49) <- source (1.);
  for i = 1 to 48 do
    t.(0).(i) <- source (1.); t.(i).(0) <- source (1.); t.(49).(i) <- source (1.); t.(i).(49) <- source (1.);
  done;;

let paroi_froide t =
  t.(0).(0) <- source (0.); t.(0).(49) <- source (0.); t.(49).(0) <- source (0.); t.(49).(49) <- source (0.);
  for i = 1 to 48 do
    t.(0).(i) <- source (0.); t.(i).(0) <- source (0.); t.(49).(i) <- source (0.); t.(i).(49) <- source (0.);
  done;;

let paroi_normale t =
  t.(0).(0) <- mur; t.(0).(49) <- mur; t.(49).(0) <- mur; t.(49).(49) <- mur;
  for i = 1 to 48 do
    t.(0).(i) <- mur; t.(i).(0) <- mur; t.(49).(i) <- mur; t.(i).(49) <- mur;
  done;;

let souris t =
  set_color black;
  let a = ref 0 and b = ref 0 and etat_clic = ref mur in
    let coté = ref 20 in
      plot_t t !coté;
      while not (zone 1020 960 200 40 !a !b) do
        let clp = mouse_pos() in
          a := fst clp; b := snd clp;
          if (zone 1020 900 200 40 !a !b) then begin tout_vert t; plot_t t !coté; end;
          if (zone 1020 840 200 40 !a !b) then begin tout_noir t; plot_t t !coté; end;
          if (zone 1020 780 200 40 !a !b) then etat_clic := niveau (0.5);
          if (zone 1020 720 200 40 !a !b) then etat_clic := mur;
          if (zone 1020 660 200 40 !a !b) then etat_clic := source (1.);
          if (zone 1020 600 200 40 !a !b) then etat_clic := source (0.);
          if (zone 1020 540 200 40 !a !b) then begin fluide t; plot_t t !coté; end;
          if (zone 1020 480 200 40 !a !b) then begin paroi_chaude t; plot_t t !coté; end;
          if (zone 1020 420 200 40 !a !b) then begin paroi_froide t; plot_t t !coté; end;
          if (zone 1020 360 200 40 !a !b) then begin paroi_normale t; plot_t t !coté; end;
          if (zone 1020 300 200 40 !a !b) then begin labyrinthe t; plot_t t !coté; end;
          if (zone 1020 240 200 40 !a !b) then begin labyrinthe2 t; plot_t t !coté; end;
          if (zone 1240 960 200 40 !a !b) then begin coté := 20; plot_t t !coté; end;
          if (zone 1240 900 200 40 !a !b) then begin coté := 19; set_color white; fill_rect 0 0 1000 1000; plot_t t !coté; end;
          
          if button_down() then
            match a, b with
              | a, b when (!a < 20) || (!a > 979) || (!b < 20) || (!b > 979) -> ();
              | _, _ -> begin
                    t.(!a / 20).(!b / 20) <- !etat_clic;
                    couleur t.(!a / 20).(!b / 20);
                    fill_rect ((!a / 20) * 20) ((!b / 20) * 20) !coté !coté; end;
                  aaa 500000;
      done; !coté;;

let affichage_2() =
set_color white; fill_rect 1010 0 700 1010;

  set_color (rgb 255 127 0); fill_rect 1020 900 200 40;
  set_color black; moveto 1030 910; draw_string "Lignes de champ";;

let plot_t_2 t i j coté =
      set_color begin
        match t.(i).(j) with
          | niveau (a) -> (rgb 255 (int_of_float (a *. 255.)) 0);
          | source (a) -> if a = 1. then white else (rgb 128 0 0);
          | mur -> black; end;
      fill_rect (i * 20) (j * 20) coté coté;;

let plot_t_2_2 t i j coté =
 let f a = (128 + ((int_of_float (a*.101.)) mod 2)*127); in
     set_color begin
        match t.(i).(j) with
          | niveau (a) -> (rgb (f a) (f a) (f a));
          | source (a) -> if a = 1. then white else (rgb 128 0 0);
          | mur -> black; end;
      fill_rect (i * 20) (j * 20) coté coté;;

let etape t v dt coté =
  for i = 0 to 49 do
    for j = 0 to 49 do
      v.(i).(j) <- change i j t dt;
    done; done;
  for i = 0 to 49 do
    for j = 0 to 49 do
      t.(i).(j) <- v.(i).(j);
      plot_t_2 t i j coté;
    done; done;;

let evolution t dt coté =
  let clp = ref (0, 0) in
    let a = ref 0 and b = ref 0 in
      let v = make_matrix 50 50 mur in
          while not (zone 1020 900 200 40 !a !b) do
            etape t v dt coté;
            if button_down() then begin clp := mouse_pos(); a := fst !clp; b := snd !clp; end;
          done;;

let vecteur t i j =
  let cell = (etat_cellule t.(i).(j)) in
    if (snd cell) = 1 then begin
        let cell_voisine = ref (0., 0) in
          let vx = ref 0. in
            cell_voisine := (etat_cellule t.(i + 1).(j));
            if (snd !cell_voisine) = 1 then begin vx := !vx +. (fst cell) -. (fst !cell_voisine); end;
            cell_voisine := (etat_cellule t.(i - 1).(j));
            if (snd !cell_voisine) = 1 then begin vx := !vx +. (fst !cell_voisine) -. (fst cell); end;
            let vy = ref 0. in
              cell_voisine := (etat_cellule t.(i).(j + 1));
              if (snd !cell_voisine) = 1 then begin vy := !vy +. (fst cell) -. (fst !cell_voisine); end;
              cell_voisine := (etat_cellule t.(i).(j - 1));
              if (snd !cell_voisine) = 1 then begin vy := !vy +. (fst !cell_voisine) -. (fst cell); end;
              (!vx, !vy); end else (0., 0.);;

let f n =
  match n with
    | n when n < 256 -> (rgb 0 0 n)
    | n when n < 512 -> (rgb 0 (n - 256) 255)
    | n when n < 768 -> (rgb (n - 512) 255 255)
    | n when n < 1024 -> (rgb 255 255 (1024 - n))
    | n -> (rgb 255 255 0);;

let champ_direction t coté =
  set_color black;
  let clp = ref (0., 0.) and x = ref 0. and y = ref 0. and r = ref 0. in
    for i = 1 to 48 do
      for j = 1 to 48 do
        clp := (vecteur t i j);
        x := (fst !clp); y := (snd !clp);
        r := sqrt (!x *. !x +. !y *. !y);
        if !x = 0. && !y = 0. then ()
        else begin
            x := !x /. !r; y := !y /. !r;
            set_color black;
            fill_rect (i * 20) (j * 20) coté coté;
            set_color yellow;
            moveto (i * 20 + 10 - (int_of_float (!x *. 10.))) (j * 20 + 10 - (int_of_float (!y *. 10.)));
            lineto (i * 20 + 10 + (int_of_float (!x *. 10.))) (j * 20 + 10 + (int_of_float (!y *. 10.)));
          end;
      done; done;;

let champ_intensité t coté =
  set_color black;
  let clp = ref (0., 0.) and x = ref 0. and y = ref 0. and r = ref 0. in
    for i = 1 to 48 do
      for j = 1 to 48 do
        clp := (vecteur t i j);
        x := (fst !clp); y := (snd !clp);
        r := sqrt (!x *. !x +. !y *. !y);
        begin match t.(i).(j) with
            | niveau (_) -> set_color (f (int_of_float (!r *. 1279.)));
            | source (_) -> set_color black;
            | mur -> set_color black; end;
        fill_rect (i * 20) (j * 20) coté coté;
      done; done;;

let champ_direction_intensité t coté =
  set_color black;
  let clp = ref (0., 0.) and x = ref 0. and y = ref 0. and r = ref 0. in
    for i = 1 to 48 do
      for j = 1 to 48 do
        clp := (vecteur t i j);
        x := (fst !clp); y := (snd !clp);
        r := sqrt (!x *. !x +. !y *. !y);
        set_color (f (int_of_float (!r *. 1279.)));
        fill_rect (i * 20) (j * 20) coté coté;
        if !x = 0. && !y = 0. then ()
        else begin
            x := !x /. !r; y := !y /. !r;
            set_color yellow;
            moveto (i * 20 + 10 - (int_of_float (!x *. 10.))) (j * 20 + 10 - (int_of_float (!y *. 10.)));
            lineto (i * 20 + 10 + (int_of_float (!x *. 10.))) (j * 20 + 10 + (int_of_float (!y *. 10.)));
          end;
      done; done;;

let affichage_3() =
  set_color white; fill_rect 1010 0 900 1010;
  
  set_color black; moveto 1030 910; draw_string "Lignes de champ";
  
  set_color (rgb 255 127 0); fill_rect 1020 840 250 40;
  set_color black; moveto 1030 850; draw_string "Direction";
  
  set_color (rgb 255 127 0); fill_rect 1020 780 250 40;
  set_color black; moveto 1030 790; draw_string "Intensité";
  
  set_color (rgb 255 127 0); fill_rect 1020 720 250 40;
  set_color black; moveto 1030 730; draw_string "Direction et intensité";
  
  set_color (rgb 255 127 0); fill_rect 1020 600 250 40;
  set_color black; moveto 1030 610; draw_string "Température";
  
  set_color (rgb 255 127 0); fill_rect 1020 540 250 40;
  set_color black; moveto 1030 550; draw_string "Direction";
  
  set_color (rgb 255 127 0); fill_rect 1280 960 250 40;
  set_color black; moveto 1290 970; draw_string "Stop";
  
  set_color (rgb 255 127 0); fill_rect 1280 900 250 40;
  set_color black; moveto 1290 910; draw_string "Continuer";;

let plot_temperature t coté =
  for i = 0 to 49 do
    for j = 0 to 49 do
      plot_t_2 t i j coté;
    done; done;;

let champ_temperature_direction t =
  set_color black;
  let clp = ref (0., 0.) and x = ref 0. and y = ref 0. and r = ref 0. in
    for i = 1 to 48 do
      for j = 1 to 48 do
        clp := (vecteur t i j);
        x := (fst !clp); y := (snd !clp);
        r := sqrt (!x *. !x +. !y *. !y);
        if !x = 0. && !y = 0. then ()
        else begin
            x := !x /. !r; y := !y /. !r;
            set_color black;
            moveto (i * 20 + 10 - (int_of_float (!x *. 10.))) (j * 20 + 10 - (int_of_float (!y *. 10.)));
            lineto (i * 20 + 10 + (int_of_float (!x *. 10.))) (j * 20 + 10 + (int_of_float (!y *. 10.)));
          end;
      done; done;;

let final t coté =
  plot_temperature t coté;
let c = ref true and d = ref true in
  let a = ref 0 and b = ref 0 in
    while !c do
      let clp = mouse_pos() in a := (fst clp); b := (snd clp);
        if (zone 1020 840 250 40 !a !b) then champ_direction t coté;
        if (zone 1020 780 250 40 !a !b) then champ_intensité t coté;
        if (zone 1020 720 250 40 !a !b) then champ_direction_intensité t coté;
        if (zone 1020 600 250 40 !a !b) then plot_temperature t coté;
        if (zone 1020 540 250 40 !a !b) then champ_temperature_direction t;
if (zone 1280 960 250 40 !a !b) then begin c := false; d := false; end;
if (zone 1280 900 250 40 !a !b) then c := false;
    done; !d;;

let automate dt =
  clear_graph();
  let a = ref true in
    let t = make_matrix 50 50 (niveau (0.5)) in
      t.(0).(0) <- mur; t.(49).(0) <- mur; t.(0).(49) <- mur; t.(49).(49) <- mur;
      for i = 1 to 48 do
        t.(i).(0) <- mur; t.(0).(i) <- mur; t.(i).(49) <- mur; t.(49).(i) <- mur;
      done;
      let coté = ref 19 in
        affichage_1();
        coté := souris t; aaa 5000000;
        while !a do
          affichage_2();
          evolution t dt !coté; aaa 5000000;
          affichage_3();
          a := (final t !coté);
        done;
        moveto 1020 0; draw_string "fini";;

clear_graph();;
automate 0.249;;
