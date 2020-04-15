#open "num";;

(* affiche les chiffres décimaux de 4444^4444 *)

let bignum = (num_of_int 4444) **/ (num_of_int 4444);;


(* calcule la somme des chiffres décimaux de 4444^4444 *)

let somme s = 
let val c = int_of_char c - 48 in 
let t = ref 0 in 
for k = 0 to string_length s - 1 do t := !t + val s.[k] done;
 !t;;

somme (string_of_num bignum);;

let k = ratio_of_num (num_of_float 8.12);;
float_of_num (num_of_ratio k);;

