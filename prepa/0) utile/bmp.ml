include "biblio\utile\Graphique.ml";;
#open "sys";;

let dossier_im =
"C:/Program Files/WinCaml3_1/WinCaml/biblio/fichiers/images/";;
let chemin1 = dossier_im ^ "autosimil/arbre7.bmp";;
let dossier_txt =
"C:/Program Files/WinCaml3_1/WinCaml/biblio/fichiers/textes/";;
let chemin2 = dossier_txt^"bmp4.txt";;
let largeur mat = vect_length mat.(0);;
let hauteur mat = vect_length mat;;
(*makemat hauteur largeur*)

(*OUT, vers fichier*)

let obtient_mat (*pt de départ x, y*)k l (*largeur:*)a (*hauteur:*) b =
  let mat = make_matrix b a 0 in
    for i = 0 to b - 1 do
      for j = 0 to a - 1 do
        mat.(i).(j) <- point_color (k+j) (l+b - 1 - i)
      done; done;
    mat;;

let en_tete_bmp can a b =
  let action can nb =
    let a = ref nb in
      for i = 1 to 4 do
        output_byte can (!a);
        a := !a / 256; done; and

  action2 can v =
    for i = 0 to (vect_length v - 1) do
      output_byte can v.(i); done; in

    let v0 = [|0; 0; 0; 0; 0x36; 0; 0; 0; 0x28; 0; 0; 0|]
    and v1 = [|1; 0; 0x18; 0; 0; 0; 0; 0|]
    and v2 = [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|] in
      let k = b mod 4 in
        let tail = a * ((3 * b) + k) in
          output_byte can 0x42;
          output_byte can 0x4D;
          action can (54 + tail);
          action2 can v0;
          action can a;
          action can b;
          action2 can v1;
          action can tail;
          action2 can v2;;


let bmp_of_mat mat chemin =
  let action4 can nb =
    let a = ref nb in
      for i = 1 to 3 do
        output_byte can (!a);
        a := !a / 256; done; in

    let a = hauteur mat
    and b = largeur mat in
      let k = b mod 4 in begin
          let can = open_out_bin chemin in
            en_tete_bmp can b a;
            for i = a - 1 downto 0 do
              for j = 0 to b - 1 do
                action4 can (mat.(i).(j))
              done;
              for t = 1 to k do output_byte can 0 done;
            done;
            close_out can end;;

let copie_ecran chemin =
  let mat = obtient_mat 0 0 (size_x()) (size_y()) in
    bmp_of_mat mat chemin;;

let copie_ecran_gen chemin u v w x =
 let mat = obtient_mat u v w x in
    bmp_of_mat mat chemin;;

let bouge_out can h = let u = pos_out can in
    seek_out can (u + h);;

(**)
(*IN, vers caml*)

let bouge_in can h = let u = pos_in can in
    seek_in can (u + h);;

let taille_bmp chemin =
 let can = open_in_bin chemin in
    seek_in can 18;
    let a = ref (input_byte can) in
      a := !a + 256 * (input_byte can);
      seek_in can 22;
      let b = ref (input_byte can) in
        b := !b + 256 * (input_byte can);
close_in can;
(!a,!b)(*en x, en y*);;


let mat_of_bmp chemin =
  let tata = taille_bmp chemin in
  let a = fst tata and b = snd tata in
    let can = open_in_bin chemin in
      let m = make_matrix b a 0 in
        let h = b mod 4 in
          seek_in can 54;
          for j = 0 to b - 1 do
            for i = 0 to a - 1 do
              let k = ref 0 in
                k := input_byte can;
                k := !k + 256 * (input_byte can);
                k := !k + 65536 * (input_byte can);
                m.(b - 1 - j).(i) <- !k
            done;
            bouge_in can h;
          done;
          close_in can;
          m;;

let nb_octet fich =
  let can = open_in_bin fich in
    try
      while true do
        let k = input_byte can in ()
      done; 0;
    with | io__End_of_file -> pos_in can;;

(*nb_octet chemin1;;*)

let affiche_mat mat a b=
  let x = largeur mat and y = hauteur mat in
    let k = make_image mat in
      draw_image k a b;;

let affiche_fich chemin =
let mat = mat_of_bmp chemin
in let x = vect_length mat.(0)
  and y = vect_length mat in
    open2 (x, y);
    affiche_mat mat 0 0;;



