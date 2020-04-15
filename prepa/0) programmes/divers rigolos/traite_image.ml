
include 
"C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/bmp.ml";;

let chem =
"C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/images/photos/";;
let fichier1 =
chem^"Photo1.bmp";;
let fichier2 =
chem^"Photo2.bmp";;
let chemin n = chem^(string_of_int n)^".bmp";;



let rec converto base l = match l with
|[]->0
|x::q-> x+ (base*(converto base q));;

let modif_pix1 n =(-n);;
let modif_pix2 n =
let b = n mod 256 (*unité*) in
let g= (n/256) mod 256 (*dizaines*)in
let r= (n/65536) in
(*converto 256 [b;g;r] identité*)
converto 256 [r;b;g];;

let traiter modif_pix mat (*modifie la mat*)=
  let x = largeur mat and
  y = hauteur mat in
    for i = 0 to y - 1 do
      for j = 0 to x - 1 do
        mat.(i).(j) <- (modif_pix mat.(i).(j));
      done; done;;

let traiter2 modif_pix depart arrive 
(*met le modif mat dans arrive*)=
  let mat = mat_of_bmp depart in
    traiter modif_pix mat;
    bmp_of_mat mat arrive;;

let traiter3 modif chemin1 chemin2 (*en temps réel*) =
  let tata = taille_bmp chemin1 in
    let a = fst tata and b = snd tata in
      let can1 = open_in_bin chemin1 and
      can2 = open_out_bin chemin2 in
        en_tete_bmp can2 a b;
        let h = b mod 4 in
          seek_in can1 54;
          for j = 0 to b - 1 do
            for i = 0 to a - 1 do
              let u = input_byte can1 and
              v = input_byte can1 and
              w = input_byte can1 in
                let (x, y, z) = modif (u, v, w) in
                  output_byte can2 x;
                  output_byte can2 y;
                  output_byte can2 z;
            done;
            bouge_in can1 h; bouge_out can2 h;
          done;
          close_in can1; close_out can2;;

let gen f (a,b,c) = (f a, f b, f c);;
let maxa = gen (min 255);;
let maxon = gen (max 0);;
let maxi a b x = if x>b then b else if x<a then a
else x;;
let maxou =gen(maxi 0 255);;
let absou = gen abs;;
let coacoa n x = (256/n)* x mod n;;

let modif1 (u,v,w) =maxa (2*u,2*v,2*w);;
let modif2 (u, v, w) =
  maxa (v + w, u + w, u + v);;
let modif3 (u,v,w) =
maxa (100+u,100+v,100+w);;
let modif4 (u,v,w) = maxon (u-100,v-100,w-100);;
let modif5 (u,v,w) =absou (v-w,w-u,u-v);;
let modif6 (u,v,w) = 
let k =abs (v-w)+abs(w-u)+abs(u-v) in (k,k,k);;
let modif7 = gen (coacoa 100);;
let modif8= gen (fun x->(-x));;

traiter3 (modif7) fichier1 (chemin 7);;
(*faire modif dans tel rectangle*)
(*moyenne entre deux images*)
(*torsion polynomiale*)