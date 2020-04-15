#open "unix";;
#open "graphics";;

let copie_ecran2 fichier(*en format caml*)=
  let v = dump_image (get_image 0 0 (size_x()) (size_x()))
  in
    let canal_out = open_out_bin fichier
    in
      output_value canal_out v;
      close_out canal_out
;;

let ouvrir image (*pas bmp; format de caml*)=
let canal_in = open_in_bin image
in
  let v = (input_value canal_in: color vect vect)
  in
    let sx = string_of_int (vect_length v.(0))
    and sy = string_of_int (vect_length v)
    in
      open_graph (" " ^ sx ^ "x" ^ sy);
      draw_image (make_image v) 0 0;
      close_in canal_in
;;


