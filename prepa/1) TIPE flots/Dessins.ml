(*Graphes orientés aux arêtes étiquetées*)

(*Structure du programme principal : modif_dessin : 
découpé en menus correspondant aux effets de chaque boutons.
Attention la suppression, la création et la modification d'arcs en général sont liés au même bouton :
"Del/Cre/Modif arc" *)
(*les graphes représentés sont sans boucles, ie pas d'arc d'un sommet vers lui-même *)

include "biblio2/TIPE/Programmes/Algorithmes/maxflow et mincost";;

let new_gra n = let a = make_vect n [] in a;;
(*pour un vide, n=0*)
(*---------------------------------------*)

let taille = 7;;
let taille2 = taille * taille;;
let taille_texte = 24;;

let new_col n = make_vect n black;;
let new_nom n = let v = make_vect n "" in for i = 0 to n-1 do v.(i) <- string_of_int i done; v;;
let new_pos n = let v = make_vect n (-2,-2) in for i = 0 to n-1 do
      let x = random__int 1000 and y = random__int 600 in v.(i) <- (x, y) done; v;;
let new_des n = {tai = n; arc=new_gra n;pos=new_pos n;nom=new_nom n;col=new_col n};;
let k = new_des 0;;

let des_of_gra g = let n = vect_length g in 
{tai = n;arc=g;pos=new_pos n;nom=new_nom n;col=new_col n};;
let des_of_gra2 g v = let n = vect_length g in 
{tai = n;arc=g;pos=v;nom=new_nom n;col=new_col n};;
let des_of_gra3 g v w = let n = vect_length g in
{tai = n;arc=g;pos=(copy_vect v);nom=new_nom n;col=w};;

let dessiner (a,b) = fill_circle a b taille;;
let entourer k (a,b) = set_color k; draw_circle a b taille; set_color black;;
let plus_entouré v n = set_color black;
  for i = 0 to n - 1 do let (a, b) = v.(i) in draw_circle a b taille done;;

let ecrire (droite,haut) = let k = current_point() in lineto (fst k + droite) (snd k + haut);
moveto (fst k) (snd k);;
let bouger (droite,haut) = let k = current_point() in moveto (fst k + droite) (snd k + haut);;
let tourner30 c (a,b) = if c then (87*a/100 - b/2, a/2 + 87*b/100) else (87*a/100 + b/2, - a/2 + 87*b/100);;

let unit (a, b) (c, d) = let intof = int_of_float and floatof = float_of_int in
    let u = floatof (c - a) and v = floatof (d - b) in
      let e = (sqrt (u ** 2. +. v ** 2.)) in
        if e >= 200. then let w = 20. *. u /. e and x = 20. *. v /. e in
            (- intof w, - intof x) else (((a - c) / 10), ((b - d) / 10));;

let segment montrer_flèches (a, b) (c, d) = moveto a b; lineto c d;
if montrer_flèches then begin
  let e = ((3 * c + a) / 4) and f = ((3 * d + b) / 4) in moveto e f;
    let j, k = unit (a, b) (c, d) in
      let (u, v) = tourner30 true (j, k) and (w, x) = tourner30 false (j, k) in
        ecrire (u, v); ecrire (w, x) end;;

let openg (tailx,taily) = let tailx1 = string_of_int tailx and taily1 = string_of_int taily in
 open_graph (tailx1^"x"^taily1) ;;

(*---------------------*)

let mettre_bouton texte couleur a b = set_color couleur; set_text_size 16; set_font "Arial";
  (*let u, v = text_size texte in*) fill_rect a b 100 30;
    set_color black; moveto (a + 1) (b + 1); draw_string texte; set_text_size 24;;

let affiche_orien orien = if orien then mettre_bouton "Passer nonorien" (rgb 83 247 185) 900 631
else mettre_bouton "Passer orien" (rgb 255 255 0) 900 631;;

let affiche_capa etiq = if etiq then mettre_bouton "Cacher étiquettes" (rgb 185 122 87) 700 631
else mettre_bouton "Montrer étiquettes" (rgb 185 122 87) 700 631;;

let affiche_dist dist = if dist=0 then mettre_bouton "Passer dist" (rgb 255 128 0) 500 631
else if dist = 1 then mettre_bouton "Passer (cap; dist)" (rgb 255 128 0) 500 631
else mettre_bouton "Passer cap" (rgb 255 128 0) 500 631;;

let affiche_noms noms = if noms then mettre_bouton "Cacher noms" green 700 600
else mettre_bouton "Montrer noms" green 700 600;;

let affiche_montrer_flèches montrer_flèches = if montrer_flèches then 
mettre_bouton "Cacher flèches" blue 200 630
else mettre_bouton "Montrer flèches" blue 200 630;;

let affiche_au_milieu au_milieu = if au_milieu then 
mettre_bouton "Plus au milieu" red  100 630
else mettre_bouton "Au milieu" red 100 630;;

(*On affiche les boutons :*)
let afficher_dessus orien etiq dist noms montrer_flèches au_milieu= 
  moveto 0 600; lineto 1000 600; set_color black;
  fill_rect 0 630 1000 30; mettre_bouton "Supprimer point" blue 500 600; 
mettre_bouton "Del/Cre/Modif arc" red 600 600;
mettre_bouton "Changer couleur" yellow 800 600;
mettre_bouton "Créer point(s)" magenta 900 600; 
mettre_bouton "Changer nom" (rgb 220 220 220) 800 630;
mettre_bouton "Calculer flot" (rgb 33 200 218) 600 630;
mettre_bouton "Enregistrer" (rgb 125 0 125 ) 400 631;
mettre_bouton "Charger" (rgb 0 0 160) 300 631;
mettre_bouton "Actu dist" (rgb 39 215 30) 300 600;
mettre_bouton "Min dist" (rgb 119 44 211) 400 600;
affiche_orien orien;
affiche_capa etiq;
affiche_dist dist;
affiche_noms noms;
affiche_montrer_flèches montrer_flèches;
affiche_au_milieu au_milieu;;

let afficher_noms d = for i = 0 to d.tai-1 do set_color d.col.(i);
        let u, v = d.pos.(i) in moveto (u + taille) (v - 4); draw_string d.nom.(i) done;;

let string_of_nombre n = match n with
omega->"+infini"
|omega_m->"-infini"
|indefini->"indefini"
|ent k->string_of_int k
|flo h->string_of_float h;;

let afficher_capacités dessin dist au_milieu= set_color black;
  for i = 0 to dessin.tai - 1 do let (a, b) = dessin.pos.(i) and l = ref dessin.arc.(i) in
      while !l <> [] do let (u, v) = hd !l in let (c, d) = dessin.pos.(u) in
            if not au_milieu then moveto ((3 * c + a) / 4) ((3 * d + b) / 4)
            else  moveto ((c + a) / 2) ((d + b) / 2);
            if (v.(0)) >=& (ent 0) then
              if dist=0 then draw_string (string_of_nombre v.(0))
              else if dist = 1 then draw_string (string_of_nombre v.(1))
else draw_string ("(" ^ string_of_nombre (v.(0)) ^ "; " ^ string_of_nombre (v.(1)) ^ ")");
            l := tl !l done; done;;

let afficher_dessous d orien optdist affnoms affdistances num montrer_flèches au_milieu= 
  for i = 0 to d.tai-1 do set_color d.col.(i); let u,v = d.pos.(i) in dessiner (u,v); done;
  set_color black; let rec tracer m i =
match m with []->() |(a,b)::c-> segment montrer_flèches d.pos.(i) d.pos.(a); tracer c i in
  for i = 0 to d.tai-1 do tracer d.arc.(i) i done;
if affnoms then afficher_noms d; if affdistances then afficher_capacités d optdist au_milieu;
 if num>=0 then entourer red d.pos.(num);;

let afficher_tout d orien optdist affnoms affdistances num montrer_flèches au_milieu =
openg (1000, 660); clear_graph();
afficher_dessous d orien optdist affnoms affdistances num montrer_flèches au_milieu;
afficher_dessus orien affdistances optdist affnoms montrer_flèches au_milieu;;

let ecrire_message (mes: string) = set_color black; set_font "Arial"; set_text_size 18;
  fill_rect 0 631 1000 30; set_color white; moveto 5 632;
  draw_string mes; set_color black; set_text_size 24;;

let pret_pour_reponse () = set_color white; fill_rect 0 600 250 30; moveto 5 602; set_color black;;

let questionner (qu: string) (var: string ref) = ecrire_message qu;
pret_pour_reponse(); set_text_size 18;
      let m = ref "" and c = ref true and n = ref 0 in
        try
          while !c do
            let e = wait_next_event [Button_down; Key_pressed] in
              if e.keypressed then let j = e.key in
                  begin
                    if j <>`\013`&& j <> (char_of_int 8) && !n <= 26 then
                      begin m := (!m) ^ (string_of_char j); draw_char j; n := !n + 1 end
                    else if j = (char_of_int 8) then
                      begin pret_pour_reponse(); n := 0; m := ""; end
                    else if j =`\013`then
                      begin if !n <> 0 then begin var :=!m end;
                        c := false; set_color white; fill_rect 0 600 250 30; set_color black;
                        fill_rect 0 630 1000 30; end
                  end;
          done;set_text_size 24;
        with Graphic_failure "graphic screen not opened" -> ();;

let en_bas (a, b) = a < 1000 && b < 600;;
(*let verifier (a, b) (c, d) = max (abs (a - c)) (abs (b - d)) < taille;;*)

let verifier (a, b) (c, d) = let p = (a-c) and q = (d-b) in (p*p + q*q) <= taille2;; 

let appartient (*pour savoir si le point ab est dans v*) (a, b) v n =
  if v = [||] then false, -1 else begin let k = ref 0 and c = ref false in while !k < n && not !c do
          c := verifier (a, b) v.(!k); if not !c then k := !k + 1 done; !c, !k end;;

let est_dans a b (i, j) (k, l) = a >= i && a <= k && b >= j && b <= l;;

(*----------------------------------------------*)

let question_couleur dessin orien num = if num>=0 then begin
let couleur = ref black and a = ref "" in(*lancer les 2 questions couleurs et nom.*)
questionner "Choisissez un couleur pour le point. Rentrez la composante rouge :" a; couleur := 65280*int_of_string !a;
questionner "Rentrez maintenant la composante verte :" a; couleur:= !couleur + 256*int_of_string !a;
questionner "Enfin, rentrez la composante bleu :" a; couleur:= !couleur + int_of_string !a;
dessin.col.(num)<- !couleur; set_color !couleur; let u,v = dessin.pos.(num) in dessiner (u,v); set_color black
end;;

let question_nom dessin orien onlycapa num montrer_flèches au_milieu= if num >=0 then begin
let nom = ref "" in 
questionner ("Le nom actuel du point est '"^dessin.nom.(num)^"'. Rentrez son nouveau nom (moins de 26 lettres):") nom;
dessin.nom.(num)<- !nom;
afficher_tout dessin orien onlycapa true true num montrer_flèches au_milieu end;;

let changer_position dessin orien onlycapa num affnoms affdist (x,y) montrer_flèches au_milieu=
 dessin.pos.(num)<- (x,y); clear_graph(); 
afficher_tout dessin orien onlycapa affnoms affdist num montrer_flèches au_milieu;;

let creer_point dessin (x,y) =   dessin.tai<-dessin.tai+1;
										dessin.arc<- concat_vect dessin.arc [|[]|];
                    dessin.pos<- concat_vect dessin.pos [|(x,y)|];
										dessin.nom<- concat_vect dessin.nom [|string_of_int (dessin.tai -1)|];
                    dessin.col<-concat_vect dessin.col [|black|];
                    dessiner (x,y);;

let mode_creation dessin= ecrire_message "Cliquez sur une touche quand vous avez fini";
  let c = ref true and x = ref 0 and y = ref 0 in while (!c) do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then begin
            let t = (mouse_pos()) in x := fst t; y := snd t;
              creer_point dessin (!x, !y) end
        else if e.keypressed then c := false;
    done;;

let nombre_of_string a =try( try match a with "infini"-> omega
 |_-> ent(int_of_string a) with Failure "int_of_string"->flo(float_of_string a)) with
 Failure "float_of_string"-> omega;;

let demander_distance optdist noma nomb =
  let d = ref "infini" and e = ref "1" in
    if optdist = 0 || optdist = 2 then questionner
      ("Choisissez une capacité positive, nulle ou négative (pour infinie) du point " ^ noma ^ " au point " ^ nomb ^ " :") d;
    if optdist = 1 || optdist = 2 then questionner
      ("Choisissez une distance positive, nulle, négative ou infinie (taper infini) du point " ^ noma ^ " au point " ^ nomb ^ " :") e;
    let r = nombre_of_string !d
    and s = nombre_of_string !e in if r >=& (ent 0) then (r, s) else (omega, s);;

let creer_arc (*orienté*)dessin optdist a b montrer_flèches = 
segment montrer_flèches dessin.pos.(a) dessin.pos.(b); 
let d,e = demander_distance optdist dessin.nom.(a) dessin.nom.(b) in if (optdist=0) then
dessin.arc.(a)<- (b,[|d;e|])::dessin.arc.(a)
else dessin.arc.(a)<- (b,[|d;e|])::dessin.arc.(a);; 

let selectionnez (*avec confirmation par enter*) dessin num txt = ecrire_message txt;
  let c = ref true and x = ref 0 and y = ref 0 and f = ref num in while (!c) do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then begin
            let t = (mouse_pos()) in x := fst t; y := snd t;
              let u, v = appartient (!x, !y) dessin.pos dessin.tai in
              (*point touché*) if u = true then f := v; end
        else if e.keypressed then begin
            if e.key = `4` then begin if !f <> 0 then f := !f - 1 else f := dessin.tai - 1; end
            else if e.key = `6` then begin if !f = dessin.tai - 1 then f := 0 else f := !f + 1; end
            else if e.key = `\013` then c := false;
          end; (*<-fin du wait*)
          plus_entouré dessin.pos dessin.tai;
          entourer red dessin.pos.(!f); 
          pret_pour_reponse(); draw_string (string_of_int !f);
    done; !f;;

let selectionnez2 (*sans confirmations, le premier selectionné*) dessin num txt =
  ecrire_message txt;
  let c = ref true and x = ref 0 and y = ref 0 and v = ref num in
    while (!c) (*si point non encore trouvé*) do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then begin
            let t = (mouse_pos()) in x := fst t; y := snd t;
              let (a,b) = appartient (!x, !y) dessin.pos dessin.tai in
                c := not a; v := b end
    done; !v;;

let rec existe_lien a = function [] -> false | (u, v) :: y -> a = u || existe_lien a y;;

let rec enleve b l = match l with [] -> []
    | (c, d) :: e -> if c = b then enleve b e else (c, d) :: enleve b e;;

let supprimer_arc dessin a b montrer_flèches=
dessin.arc.(a)<-enleve b dessin.arc.(a);
set_color white; segment montrer_flèches dessin.pos.(a) dessin.pos.(b); set_color black;;

let rec distance_2 optdist b l u v= 
(*remplace la 1ere incidence de b dans l par la
 mm chose mais avec la cap ou la dist remplacée par u *)
match l with [] -> []
    | (c, d) :: e when c = b -> (c,[|u;v|])::e 
|_-> (hd l) :: distance_2 optdist b (tl l) u v;;

let creer_arc_non_orien dessin optdist a b montrer_flèches= 
segment montrer_flèches dessin.pos.(a) dessin.pos.(b); 
segment montrer_flèches dessin.pos.(b) dessin.pos.(a); 
let (d,e) = demander_distance optdist dessin.nom.(a) dessin.nom.(b) in
dessin.arc.(a)<- (b,[|d;e|])::dessin.arc.(a);
dessin.arc.(b)<- (a,[|d;e|])::dessin.arc.(b);;

let menu_arcs dessin num orien optdist montrer_flèches(*mode arcs*)=
  let a = ref 0 and b = ref 0 and c = ref false and d = ref "" in
    a:=selectionnez2 dessin num "Choisissez le point de départ";
    b:=selectionnez2 dessin num "Choisissez le point d'arrivée" ;
if orien then begin
    c := existe_lien !b dessin.arc.(!a);
    if not !c then creer_arc dessin optdist !a !b montrer_flèches
    else begin questionner "Voulez-vous supprimer cet arc : 1:oui / autre que 1:non" d;
        if !d = "1" then supprimer_arc dessin !a !b montrer_flèches
        else let u,v = demander_distance optdist dessin.nom.(!a) dessin.nom.(!b) in
         dessin.arc.(!a)<-distance_2 optdist !b dessin.arc.(!a) u v end
end
else begin
(*propose de supprimer les deux si un des deux au moins existe déjà, sinon crée les deux avec la mm dist*)
(*dans le cas où au - un des deux et pas supprimer, il modifie les longueurs de ce qui existe déjà*)
(*donc si on veut mettre les 2 et qu'il n'y en a qu'un, on doit supprimer et refaire*)
    c:= ( existe_lien !b dessin.arc.(!a) || existe_lien !a dessin.arc.(!b) );
    if not !c then creer_arc_non_orien dessin optdist !a !b montrer_flèches
    else begin questionner "Voulez-vous supprimer cet arc : 1:oui / autre que 1:non" d;
        if !d = "1" then begin supprimer_arc dessin !a !b montrer_flèches; supprimer_arc dessin !b !a montrer_flèches end
        else begin let t,s= demander_distance optdist dessin.nom.(!a) dessin.nom.(!b) in
          dessin.arc.(!a)<- distance_2 optdist !b dessin.arc.(!a) t s;
          dessin.arc.(!b)<- distance_2 optdist !a dessin.arc.(!b) t s end end
end;;

(*-------------------------------*)
(*suppression de points*)

let coupe_vect k n v = if k <= n-1 then
  concat_vect (sub_vect v 0 k) (sub_vect v (k+1) (n-k-1)) else v;;

let dernieres_modif k m v = 
   for i = 0 to m - 1 do
    let l = ref v.(i) and l2 = ref [] in 
     while !l <> [] do let r, s = (hd !l) in
          if r < k then l2 := (hd !l) :: !l2
          else if r > k then l2 := (r - 1, s) :: !l2;
          l := tl !l; done; 
     v.(i) <- !l2 done; 
v;;

let appartient (*pour savoir si le point ab est dans v*) (a, b) v n =
  if v = [||] then false, -1 else begin let k = ref 0 and c = ref false in while !k < n && not !c do
          c := verifier (a, b) v.(!k); if not !c then k := !k + 1 done; !c, !k end;;

let delete dessin k = (*if k<dessin.gra.tai then begin ...*) let n = dessin.tai in
dessin.tai<-n-1;
dessin.arc<-coupe_vect k n dessin.arc;
 dessin.pos<-coupe_vect k n dessin.pos;
 dessin.nom<-coupe_vect k n dessin.nom;
 dessin.col<-coupe_vect k n dessin.col;
dessin.arc<-dernieres_modif k (n-1) dessin.arc;;

let supprimer_point dessin num = let a = ref num in
    a:=selectionnez2 dessin num "Quel point voulez vous supprimer ?";
    delete dessin !a;;

(*-----------------*)
(*enregistrer et charger*)

let enregistrer d fichier =
    let canal_out = open_out_bin fichier
    in output_value canal_out d;
       close_out canal_out;;

let ouvrir fichier=
let canal_in = open_in_bin fichier in
let d = (input_value canal_in) in close_in canal_in;d;;

let chemin_des = "biblio2/TIPE/Programmes/sauvegardes/dessins/";;
let chemin_gra = "biblio2/TIPE/Programmes/sauvegardes/graphes/";;

let enregistrer_dessin d = let rep = ref "" in
questionner "Où voulez vous enregistrer ?" rep;
let fichier1 = chemin_des^(!rep) in
enregistrer d fichier1 ;;

let charger_dessin d = let rep = ref "" in
questionner "Quel fichier voulez vous charger ?" rep;
let fichier1= chemin_des^(!rep) in
let d2 = ouvrir fichier1 in
d.tai<-d2.tai; d.arc<-d2.arc;d.pos<-d2.pos;d.col<-d2.col;d.nom<-d2.nom;;

let vect_couleurs u c1 c2 =let n = (vect_length u) in
let v = make_vect n black in
for i = 0 to n - 1 do if u.(i) then v.(i)<-c1 else v.(i)<-c2 done;v;;


(*------------------------------------------*)
(*Distances*)

let sqrt_int a = (*arrondie au supérieur, 
sqrt_int 9 = 3 mais sqrt_int 10 = 4*)
  let i = ref 1 and k = ref 0 in
    while !k < a do
      k := !k + !i;
      i := !i + 2 done; !i / 2;;

let dist_int (a, b) (c, d) =
  let k = a - c and t = b - d in sqrt_int (k * k + t * t);;

let dist_float (a, b) (c, d) =
  let k = floatof (a - c) and t = floatof (b - d) in sqrt (k ** 2. +. t ** 2.);;

let vrai_dist (g:((int*nombre vect) list) vect) pos n e = 
(*renvoie le graphe g avec les distances réelles en pixel*)
let G = make_vect n [] in
  (* /!\  e si on veut une distance entière *)
    let rec aux v k l =
      match l with [] -> []
        | _ -> let u, w = hd l in let w2 = ref (ent 0) in
                if e then w2 := ent (dist_int v.(k) v.(u) (*/100*))
                else w2 := flo (dist_float v.(k) v.(u)(*/.100.*));
                (u, [|w.(0); !w2|]) :: (aux v k (tl l)) in
      for i = 0 to n - 1 do
        G.(i) <- (aux pos i g.(i)) done;
      G;;

let actu_dist dessin = 
let gr = vrai_dist dessin.arc dessin.pos dessin.tai true in
dessin.arc <- gr;;

let afficher_bizarre dessin r couleurs optdist montrer_flèches au_milieu= clear_graph ();
  let n = vect_length dessin.pos in
    for i = 0 to n - 1 do let (a, b) = dessin.pos.(i) in dessin.pos.(i) <- ((a / 2), b); done;
    let v = copy_vect dessin.pos in for i = 0 to (n - 1) do
        v.(i) <- (500 + fst v.(i), snd v.(i)) done;
      let dd = {tai = n; arc = r; pos = v; nom = dessin.nom; col = couleurs} in
        afficher_dessous dd true 0 false true 0 montrer_flèches au_milieu;
        afficher_dessous dessin true 2 false true 0 montrer_flèches au_milieu;
        for i = 0 to n - 1 do let (a, b) = dessin.pos.(i) in dd.pos.(i) <- (2 * a, b); done;
        dd.col <- dessin.col;
        dd;;
(*afficher_dessous d orien onlycapa affnoms affdistances num montrer_flèches*)

(*--------------------------------------*)

let afficher_gras (*ep*) pos chemin =
  (*affiche un chemin en gras, en fonction de l'épaisseur ep*)
  set_line_width 3 (*ep*);
  let rec continuer l = match l with
      a :: b -> lineto (fst pos.(a)) (snd pos.(a)); continuer b
      | _ -> set_line_width 1 in
    match chemin with a :: b -> moveto (fst pos.(a)) (snd pos.(a));
          continuer b | _ -> set_line_width 1;;

let plus_court_graphique dessin num a b =
  a := selectionnez2 dessin !num "Selectionnez le départ";
  b := selectionnez2 dessin !num "Selectionnez l'arrivée";
  let r = ref "" in questionner "1:PP // 2:PL // 3: Bellman // 4:Djikstra" r;
    if !r = "1" then let (chem, _) = (PP dessin.arc dessin.tai !a !b) in afficher_gras dessin.pos chem
    else if !r = "2" then let (chem, _) = (PL dessin.arc dessin.tai !a !b) in afficher_gras dessin.pos chem
    else if !r = "3" then let (chem, _, _) = bellman dessin.arc dessin.tai !a !b in afficher_gras dessin.pos chem
    else let (chem, _, _) = djikstra dessin.arc dessin.tai !a !b in afficher_gras dessin.pos chem;;

let demander_mincost arcs a b =
  let r = ref "" in questionner "1:maxflow_mincost // 2:coût fixé // 3: flot fixé" r;
    if !r = "1" then (maxflow_mincost arcs a b)
    else begin let s = ref "" in questionner "Valeur max :" s;
          let t = nombre_of_string !s in
            if !r = "2" then mincost_coût_fixé arcs a b t
            else mincost_flot_fixé arcs a b t; end;;

(*--------------------------------------*)

let menu_principal dessin =
  let c = ref true and num = ref 0 and x = ref 0 and y = ref 0 and affnoms = ref false and optdist = ref 0
  and a = ref 0 and b = ref 0 and affetiq = ref false and orien = ref true and montrer_flèches = ref true
  and au_milieu = ref true in
    while (!c) do
      afficher_tout dessin !orien (!optdist) !affnoms !affetiq !num !montrer_flèches !au_milieu;
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then (*1*) begin

            let t = (mouse_pos ()) in x := fst t; y := snd t;
              if en_bas (!x, !y) then begin let u, v = appartient (!x, !y) dessin.pos dessin.tai in
                  (*point touché*) if u then begin num := v; afficher_tout dessin !orien (!optdist) !affnoms !affetiq !num !montrer_flèches !au_milieu end
                      (*change pos*) else changer_position dessin !orien (!optdist) !num !affnoms !affetiq (!x, !y) !montrer_flèches !au_milieu end

                (*bouton modifier param*)
              else if est_dans !x !y (500, 600) (600, 630) then begin num := 0; supprimer_point dessin !num end
                (*attention : le point selectionné sera, après la suppression, le nouveau point 0,
                  donc on ne peut pas enlever tous les points, il doit en rester au moins 1.*)
              else if est_dans !x !y (600, 600) (700, 630) then menu_arcs dessin !num !orien !optdist !montrer_flèches
              else if est_dans !x !y (700, 600) (800, 630) then affnoms := not !affnoms
              else if est_dans !x !y (700, 631) (800, 660) then affetiq := not !affetiq
              else if est_dans !x !y (800, 600) (900, 630) then question_couleur dessin !orien !num
              else if est_dans !x !y (800, 631) (900, 660) then question_nom dessin !orien (!optdist) !num !montrer_flèches !au_milieu
              else if est_dans !x !y (900, 631) (1000, 660) then orien := not !orien
              else if est_dans !x !y (900, 600) (1000, 630) then mode_creation dessin
              else if est_dans !x !y (500, 631) (600, 660) then optdist := (!optdist + 1) mod 3
              else if est_dans !x !y (400, 631) (500, 660) then enregistrer_dessin dessin
              else if est_dans !x !y (300, 631) (400, 660) then charger_dessin dessin
              else if est_dans !x !y (400, 600) (500, 630) then plus_court_graphique dessin num a b
              else if est_dans !x !y (300, 600) (400, 630) then actu_dist dessin
              else if est_dans !x !y (600, 631) (700, 660) then c := false
              else if est_dans !x !y (200, 631) (300, 660) then montrer_flèches := not !montrer_flèches
              else if est_dans !x !y (100, 631) (200, 660) then au_milieu := not !au_milieu
          end (*cas du clic fini fini*)
        else if e.keypressed then begin
            if e.key = `4` then begin if !num <> 0 then num := !num - 1 else num := dessin.tai - 1; end
            else if e.key = `6` then begin if !num = dessin.tai - 1 then num := 0 else num := !num + 1; end;
          end;
    done;

    a := selectionnez2 dessin !num "Selectionnez la source";
    b := selectionnez2 dessin !num "Selectionnez le puit";
    if (!optdist) = 0 then begin
        let (p, q, r, s) = (maxflow dessin.arc !a !b) in
          let couleurs = vect_couleurs p green red in
            let flot = afficher_bizarre dessin r couleurs !optdist !montrer_flèches !au_milieu in
              enregistrer_dessin flot; (ent 0), (p, q, r, s); end
    else begin let (m, (p, q, r, s)) = demander_mincost dessin.arc !a !b in
          let couleurs = vect_couleurs p green red in
            let flot = afficher_bizarre dessin r couleurs !optdist !montrer_flèches !au_milieu in
              enregistrer_dessin flot; (m, (p, q, r, s)); end;;
(*<-flot, (coupe, valeur du flot, graphe des écarts, graphe du flot max) *)

let essai n = let des = (new_des n) in (menu_principal des);;

let essai2 n = let des = (new_des n) in
try (menu_principal des), des; with 
Graphic_failure "graphic screen not opened"->(ent 0, ([||], ent 0,[||],[||])),des;;

essai2 5;;

(*-------------------------------*)

menu_principal {tai = 8; arc =
   [|[5, [|omega; ent 3|]; 6, [|omega; ent 2|]; 4, [|omega; ent 2|]];
     [4, [|omega; ent 1|]; 5, [|omega; ent 4|]; 7, [|omega; ent 6|]];
     [7, [|omega; ent 1|]; 6, [|omega; ent 3|]; 3, [|omega; ent 4|]];
     [7, [|omega; ent 5|]; 5, [|omega; ent 3|]; 2, [|omega; ent 4|];
      6, [|omega; ent 2|]];
     [0, [|omega; ent 2|]; 1, [|omega; ent 1|]; 5, [|omega; ent 3|]];
     [0, [|omega; ent 3|]; 4, [|omega; ent 3|]; 1, [|omega; ent 4|];
      7, [|omega; ent 2|]; 3, [|omega; ent 3|]; 6, [|omega; ent 1|]];
     [0, [|omega; ent 2|]; 5, [|omega; ent 1|]; 2, [|omega; ent 3|];
      3, [|omega; ent 2|]];
     [5, [|omega; ent 2|]; 1, [|omega; ent 6|]; 2, [|omega; ent 1|];
      3, [|omega; ent 5|]]|];
  pos =
   [|307, 501; 349, 266; 649, 426; 513, 398; 180, 343; 343, 402; 460, 501;
     509, 291|];
  nom = [|"1"; "7"; "2"; "3"; "4"; "5"; "6"; "7"|];
  col = [|0; 0; 0; 0; 0; 0; 0; 0|]};;