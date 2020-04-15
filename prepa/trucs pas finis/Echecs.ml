
include "biblio/utile/bmp.ml";;
include "biblio/utile/_Ensembles.ml";;

type piece = | cb | fb | db | pb | rb | tb
  | cn | fn | dn | pn | rn | tn | rien;;

type position == piece vect vect;;

let pos_base()=
  let k = make_matrix 9 9 rien in
    for i = 1 to 8 do
      k.(i).(2) <- pb;
      k.(i).(7) <- pn
    done;
    k.(1).(1) <- tb; k.(1).(8) <- tn;
    k.(2).(1) <- cb; k.(2).(8) <- cn;
    k.(3).(1) <- fb; k.(3).(8) <- fn;
    k.(4).(1) <- rb; k.(4).(8) <- rn;
    k.(5).(1) <- db; k.(5).(8) <- dn;
    k.(6).(1) <- fb; k.(6).(8) <- fn;
    k.(7).(1) <- cb; k.(7).(8) <- cn;
    k.(8).(1) <- tb; k.(8).(8) <- tn;
    k;;

let pos_deb =pos_base();;

let chemin_piece = dossier_im ^ "pions/";;
let listt = ["cb";"fb";"db";"pb";"rb";"tb";"cn";"fn";"dn";
"pn";"rn";"tn";"rien"];;
let listat =let couc = (fun t->chemin_piece^t^".bmp") in
map couc listt;;

let traite_mat mat=
map_vect (map_vect (fun 16777215 -> transp |a->a)) mat;;

let charge = 
vect_of_list (map traite_mat (map mat_of_bmp listat));;

let num_of_piece pie= match pie with
|cb ->1 |fb->2 |db->3
|pb->4 |rb->5 |tb->6
|cn ->7 |fn ->8 |dn->9
 |pn ->10 |rn->11 |tn ->12|rien->13;;

let mat_of_piece pie = charge.(num_of_piece pie -1);;

let affiche_mat2 mat a b (*pour transparent*) =
  let x = largeur mat and y = hauteur mat in
    for i = 0 to x - 1 do
      for j = 0 to y - 1 do let k = mat.(y - 1 - j).(x - 1 - i)
        in if k <> (- 1) then begin
              set_color k;
              plot (a + i) (b + j) end done; done;;

let mettre pie a b = let m = mat_of_piece pie in
    affiche_mat2 m (40 * (a-1)) (40 * (b-1));;

let bouger_piece_graphique pos (depx, depy) (arrx, arry) =
  if (depx + depy) mod 2 = 0 then set_color white else
    set_color black; bloc 40 (depx - 1) (depy - 1);
  let p = pos.(depx).(depy) in
    mettre p arrx arry;
    pos.(depx).(depy) <- rien; pos.(arrx).(arry) <- p;;

let bouger_piece pos (depx, depy) (arrx, arry) =
let p = pos.(depx).(depy) in
pos.(depx).(depy) <- rien; pos.(arrx).(arry) <- p;;

let affiche_mat2 mat a b=
  let x = largeur mat and y = hauteur mat in
    let k = make_image mat in
      draw_image k a b;;

let repfond pos = (*mettre lettres, chiffres !*)
  open2 (400, 400);
  clear_graph();
  for i = 1 to 8 do
    for j = 1 to 8 do
      if (i + j) mod 2 = 0 then set_color white else
        set_color black; bloc 40 (i - 1) (j - 1);
      mettre pos.(i).(j) i j;
    done;
  done;;

repfond pos_deb;;
bouger_piece_graphique pos_deb (1,2) (1,3);;

let copier a b =
get_image (40*(a-1)) (40*(b-1)) 40 40;;

let mouv_roi = [1, 1; 1, 0;0, 1];;
let mouv_caval=[2,1;1,2];;
let mouv_pionb=[0,1;0,2];;
let mouv_pionn=[0,-1;0,-2];;

let autorisé p (a, b) (c, d) =
  let (u, v) = ((c-a), (d-b)) in
    match p with
      | rien -> false
      | k when (k = rn || k = rb) -> appartient (abs u, abs v) mouv_roi
      | k when (k = cb || k = cn) -> appartient (abs u, abs v) mouv_caval
      | k when (k = pb) -> appartient (u, v) mouv_pionb
      | k when (k = pn) -> appartient (u, v) mouv_pionn
      | k when (k = tb || k = tn) -> (u = 0 || v = 0)&& (u,v)<>(0,0)
      | k when (k = fb || k = fn) -> u<>0&&(abs u = abs v)
      | _ -> (u,v)<>(0,0) && (u = 0 || v = 0 || abs u = abs v);;

let autorisé2 pos (a, b) (c, d)=
let pie = pos.(a).(b) in
if not autorisé pie (a, b) (c, d) then false
else if (pos.(c).(d)<>rien) then false
(*else if not appartient pie [rb;rn;cb;cn] then
let k = ref 0 and (u, v) = ((c-a), (d-b)) in
if abs u>=abs v then k:=*)
else true;;


let attend_joueur pos =
  let c = ref true and select = ref false and x = ref 0 and y = ref 0
  and g (a, b) = ((a / 40) + 1, (b / 40) + 1) (*and pie = ref rien*) in
    while (!c) do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.button then
          let k = mouse_pos() in
            let (u, v) = g (k) in
              if (not !select) then begin
                  x := u;
                  y := v;
                  let pie = pos.(!x).(!y) in
                    if pie <> rien then select := true; end
              else if autorisé2 pos (!x, !y) (u, v) then begin
                  bouger_piece_graphique pos (!x, !y) (u, v);
                  c := false end;
        else if e.key = `a` then select := false;
    done;;

repfond (pos_deb);;
attend_joueur pos_deb;;

(*--------------------------*)

let tour_joueur m=
;;


let fini_ou_pas1 m=false;;
let tour_ia1 m= m;;
let final joueur= clear_graph();;

let echecs ia=
  open_graph "400*400";
let m=jeu_début in
  let fin = ref (fini_ou_pas1 m) and
  joueur = ref "blancs" in
    while (!fin = false) do
      représenter2 m;
      if ia = false || !joueur = "blancs" then
        tour_joueur m
      else tour_ia1 m;
      fin := fini_ou_pas1 m;
      if (!joueur = "blancs") then joueur := "noirs"
      else joueur := "blancs";
    done;
    final (!joueur);;

(*-----------------------------------------*)
