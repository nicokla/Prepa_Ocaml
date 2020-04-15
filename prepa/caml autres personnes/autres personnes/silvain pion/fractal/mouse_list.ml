
  (* Ce programme rend la liste des points (x,y), successivement *)
  (* selectionnes a la souris dans la fenetre graphique. *)

  (* On selectionne les points a la souris, et quand on a fini, *)
  (* on appuie sur espace pour fermer le poly, ou autre touche, pour  *)
  (* finir sans boucler. *)

let rec record_int l =

  while button_down() or key_pressed()             (* Attend la relache. *)
  do () done;

  while not( button_down() or key_pressed() )      (* Attend l'appui. *)
  do () done;

  if button_down()
  then match mouse_pos()
       with (x,y) -> ( if l=[] then (moveto x y) else (lineto x y);
                       record_int (l @ [(x,y)]) )

  else if key_pressed()
  then match read_key()
       with ` ` -> ( match hd(l)
                     with (x,y) -> (lineto x y ; l @ [hd(l)])
                   )
          |  _  -> l
  else record_int l
;;

  (* Transforme une liste de (int*int) en (float*float). *)

let int2fl l = map (fun (x,y) -> (float_of_int x,float_of_int y)) l;;

let record l = int2fl (record_int l);;

  (* Soustrait l'origine. *)

let sous l = map (fun (x,y) -> (x -. (fst (hd l)), y -. (snd (hd l)))) l;;

  (* Tire le dernier element d'une liste. *)

let rec last =
  fun [a]  -> a
    | l1   -> last (tl l1);;

  (* Renvoie la liste privee de son dernier element. *)

let rec but_last =
  fun [a]     -> []
    | (a::l1) -> a::(but_last l1);;

  (* Inverse un nbre complexe. *)

let inv (x,y) = ( x /. (x*.x +. y*.y), (0. -. y) /. (x*.x +. y*.y) );;

  (* Multiplie deux nombres complexes. *)

let mult (a,b) (c,d) = (a *. c -. b *. d, a *. d +. b *. c);;

  (* A partir d'une liste de points, calcule les nouvelles coordonnees, *)
  (* par une similitude qui met les 2 extremites en (0,0) et (1,0).     *)

let normalise l = map (simili (last l)) (sous l)
  where rec simili (u,v) = mult (inv (u,v));;

