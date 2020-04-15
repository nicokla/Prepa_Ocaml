
  (* Programme principal. Gere le menu, et utilise les autres fichiers: *)
  (* ...  & .... pour afficher et rentrer des dessins.                  *)
  (* Il faut: pouvoir sauver et charger des fichiers (.lig, .pol).      *)
  (* Faire des sous-menus lignes et polygones.                          *)


#open "graphics";;
open_graph;;
open_graph "";;

include "mouse_list.ml";;
include "fractal.ml";;

let iter () =
  clear_graph();
  moveto 20 (size_y()-30);
  draw_string "Entrez l'iteration (0-9 : ; < = > ? @ A..).";
  int_of_char(read_key()) - int_of_char(`0`)
;;

let affiche_menu () =
  clear_graph();
  moveto 20 (size_y()-30);
  draw_string " 0 -> Quitter";
  moveto 20 (size_y()-50);
  draw_string " 1 -> Afficher la  fractale";
  moveto 20 (size_y()-70);
  draw_string "*2 -> Charger  une ligne";
  moveto 20 (size_y()-90);
  draw_string "*3 -> Sauver   une ligne";
  moveto 20 (size_y()-110);
  draw_string " 4 -> Dessiner une ligne";
  moveto 20 (size_y()-130);
  draw_string "*5 -> Charger  un  polygône";
  moveto 20 (size_y()-150);
  draw_string "*6 -> Sauver   un  polygône";
  moveto 20 (size_y()-170);
  draw_string " 7 -> Dessiner un  polygône";
  moveto 20 (size_y()-190);
  draw_string " 8 -> Help !";
  moveto 20 (size_y()-210);
  draw_string "*9 -> Modifier la taille de la fenetre";
  moveto 30 (size_y()-240);
  draw_string "Votre choix ? (sauf les *, non disponibles)"
;;

let affiche_aide () =
  clear_graph();
  moveto 20 (size_y()-30);
  draw_string "Ce programme permet de faire des fractales du type flocon de Koch.";
  moveto 20 (size_y()-45);
  draw_string "On peut fixer le type de polygône initial, à l'aide de la souris.";
  moveto 20 (size_y()-60);
  draw_string "Pour terminer le tracé, on appuie sur une touche, et la touche espace";
  moveto 20 (size_y()-75);
  draw_string "permet de boucler le polygône. De même pour le tracé de la ligne brisée.";
  moveto 20 (size_y()-90);
  draw_string "On peut également fixer le nombre d'itérations.";
  match read_key() with _ -> ()
;;

let rec go ()=
  affiche_menu();
  reagit()
where reagit () =
  match read_key()
  with `0` -> close_graph ()
     | `1` -> ( match iter()
                with it -> ( clear_graph (); global (homo !poly) it )
              );
              ( match read_key() with _ -> ();
                go ()
              )
     | `4` -> clear_graph();
              ligne := normalise (sous (record []));
              go()
     | `7` -> clear_graph();
              poly := des_homo (record []);
              go()
     | `8` -> affiche_aide();
              go()
     | _   -> go()
;;


(* Pour lancer: *)

go();;

clear_graph ();;

close_graph ();;

(* Fin. *);;
