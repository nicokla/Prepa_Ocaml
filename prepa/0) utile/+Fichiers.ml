
let fichier1= "C:/Documents and Settings/All Users/Bureau/test.txt";;
let fichier2= "C:/test.txt";;
let fichier3= "C:/Documents and Settings/All Users/Bureau/test2.txt";;

let chemin1="C:/Documents and Settings/All Users/Bureau/";;
let chemin2="C:/";;
let dossier_sauv= 
"C:/Program Files/WinCaml3_1/WinCaml/bibliothèques/sauvegardes/";;

let string_of_vect v =
  let n = vect_length v 
and st = string_of_int in
match n with
|0-> "[||]"
|1->"[|"^st v.(0)^"|]"
|_ ->let chaine = ref "[|" in
    for i = 0 to (n - 2) do chaine := (!chaine) ^ st (v.(i)) ^ ";"; done;
    (!chaine) ^ st (v.(n - 1)) ^ "|]";;

let string_of_list l=
let rec string_of_list2 l=
match l with
|[]->""
|[a]-> string_of_int a
|a::b-> string_of_int a ^ ";" ^ string_of_list2 b
in "["^string_of_list2 l^"]";;

let rec str_list2 l=
match l with
|[]->""
|a::b->string_of_int a ^ "\n" ^ str_list2 b;;

#open "sys";;
let ecrire quoi ou (*sans effacer ancien contenu*)=
  let m = open ou [O_CREAT; O_TEXT; O_RDWR] s_iwall in
    let f = open_descriptor_out m in
      let bb = out_channel_length f in
        seek_out f (bb);
        (*let g=open_out_gen [O_CREAT ;O_TEXT; O_RDWR] s_iwall "C:/baaaaa.txt";;*)
        output_string f quoi;
        close_out f;;

(*ecrire "fdf" "C:/essai.txt";;*)

let ecrire2 quoi ou (*efface ancien contenu*)=
let m=open_out ou in
output_string m quoi;
close_out m;;

let effacer ou ()= ecrire2 "" ou ;;

(*cd "C:/" ;;*)

let sauvegarder truc ou =
  let can = open_out ou in
    output_value can truc;
    close_out can;;

let avoir ou=
let can = open_in ou in
    let truc = input_value can in
    close_in can;truc;;


