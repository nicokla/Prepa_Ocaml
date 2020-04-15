
      	(* Projet de programmation: outils de compr�ssion.         *)
        (* fichier desadapt.ml : d�compression du codage adaptatif *)


  (* Description globale:
      pour d�compresser un fichier .zzz, je lis tout d'abord la longueur du
      fichier de d�part, qui est inscrite en t�te du fichier .zzz.

      Puis je lis chaque octet, et je le transforme en une liste de 0/1, qui
      vont s'ajouter � la liste !tampon.
      Je vide cette liste en faisant �voluer arbre_huff, qui contient l'arbre
      de huffman corespondant aux occurences des pr�c�dents caracat�res
      d�cod�s.
      Une fois arriv� "au fond de l'arbre", je trouve le nouveau caract�re,
      et je modifie l_v et l_p en cons�quence, et je recr�e un nouvel
      arbre de Huffman.
  *)


type 'a arbre  = Node of ('a arbre * 'a arbre) | Leaf of 'a ;;


let ch_e       = ref std_in      (* contiendra le canal du fichier compr�ss� *)
and ch_s       = ref std_out   (* contiendra le canal du fichier d�compr�ss� *)
and l_v        = ref [(0,0)]      (* pour les caract�res non encore utilis�s *)
and l_p        = ref [(0,0)]                              (* pour les autres *)
and arbre_huff = ref (Leaf 0)                            (* arbre de Huffman *)
and arbre_vide = ref (Leaf 0)                     (* arbre complet contenant *)
                                              (* les caract�res pas utilis�s *)
and tampon     = ref [0]               (* contiendra les 0/1 � coder en bits *)
and longueur   = ref 0          (* contiendra la longueur du fichier d�compr *)
and compteur   = ref 0;;         (* pour savoir o� on en est dans la d�compr *)


    (* Prend une liste de (int1,int2) et rend (int1,Leaf int2). *)

let int2leaf = map (fun (a,b) -> (a, Leaf b));;


    (* Cr��e la liste: [(1,255);(1,254);...;(1,0)]. *)

let rec fait_liste_vide =
  fun 0 -> [(1,0)]
    | i -> (1,i)::(fait_liste_vide (i-1));;


      (* Rend la liste de 0/1 corespondant � l'octet. *)

let transforme octet =
  let accu1 = ref []
  and accu2 = ref 0
  in
    begin
      accu1 := [];
      accu2 := octet;
      for j = 1 to 8 do
      	accu1 := (!accu2 mod 2)::(!accu1);
      	accu2 := !accu2 / 2
      done;
      !accu1
    end;;


    (* Insertion d'un �l�ment dans une liste de paires *)
    (* tri�es par ordre croissant du premier �l�ment.  *)

let rec insere a =
  fun []     -> [a]
    | (b::l) -> if fst a <= fst b then a::b::l
                                  else b::(insere a l);;


    (* Cr��e l'arbre de Huffman, � partir d'une liste tri�e de (occ, arbre). *)

let construit_huff l = snd(hd(const l))
  where rec const =
  fun []                -> []
    | [(p,q)]           -> [(p,q)]
    | ((p,q)::(r,s)::l) -> const (insere (p+r,Node (q,s)) l);;


    (* Regarde si le caract�re n est absent de la liste donn�e d�croissante. *)

let rec pas_element n =
  fun [] -> true
    | ((a,m)::l) -> if m=n
                    then false
                    else if m<n then true
               	    else pas_element n l;;

    (* Retire le caract�re n de la liste l (utilis� pour l_v). *)

let rec retire n =
  fun ((a,m)::l) -> if m=n
                    then l
                    else (a,m)::(retire n l);;

    (* Modifie la liste pleine, l_p, en incr�mentant l'occurence de n.  *)
    (* Cette liste n'est jamais vide, donc pas de probl�me de matching. *)

let rec modifie n =
  fun ((occ,m)::l) -> if m=n then insere (occ+1,n) l
      	       	       	     else (occ,m)::(modifie n l);;

    (* Modifie l_v, l_p, arbre_vide, arbre_huff, en fct du caract�re trouv�. *)

let suivant car = 
  if (pas_element car !l_v)
  then ( l_p := modifie car !l_p;
         arbre_huff := construit_huff ( if !l_v = []
                                        then int2leaf !l_p
                                        else (0,!arbre_vide)::(int2leaf !l_p)
                                       )
        )
  else ( l_v := retire car !l_v;
         l_p := (1,car)::(!l_p);
	 if !l_v = []
	 then arbre_huff := construit_huff (int2leaf !l_p)
         else arbre_vide := construit_huff (int2leaf !l_v);
              arbre_huff := construit_huff ( (0,!arbre_vide)::(int2leaf !l_p) )
        );;


    (* A partir de la 'direction' donn�e par la t�te de tampon, fait avancer *)
    (* arbre_huff, ou inscrit dans le fichier d�compress� le caract�re       *)
    (* trouv�, ainsi qu'il modifie la liste des occurences, et cr�� le       *)
    (* nouvel arbre_huffman, et le place dans arbre_huff.                    *)

let rec decode () =
  match !arbre_huff
  with
    Leaf l ->
    begin
      suivant l;
      output_byte (!ch_s) l;
      incr compteur
    end

  | Node n ->
    begin
      (match hd !tampon
      with
        0 -> arbre_huff := fst n
      | 1 -> arbre_huff := snd n );
      tampon := tl !tampon;
      match !arbre_huff
      with
        Leaf l -> decode()
      | Node n -> ()
    end;;


      (* S'occupe d'ouvrir les fichiers. *)

let ouvre nom =
  begin
    print_newline();
    print_string ("Decompression en cours de: " ^ nom);
    ch_e := (open_in_bin nom);
    ch_s := (open_out_bin (sub_string nom 0 ((string_length nom)-4)));
    seek_out !ch_s 0;
    seek_in !ch_e 0
  end;;


      (* S'occupe de fermer les fichiers. *)

let ferme nom =
  begin
    print_newline();
    print_string ("Decompression de " ^ nom ^ " termin�e.");
    print_newline();
    close_in !ch_e;
    close_out !ch_s
  end;;


  (* Prend "nom_du_fichier_�_d�compr�sser.zzz" en argument, *)
  (* rend unit, en produisant le fichier d�compr�ss�.       *)

let expanse nom_fich =
  begin

      (* Initialisation. *)

    compteur := 0;
    tampon := [];
    l_v := fait_liste_vide 255;
    l_p := [];
    ouvre nom_fich;
    longueur := input_binary_int !ch_e;
    print_newline ();
    print_string "Longueur du fichier originel: ";
    print_int !longueur;
    print_newline ();

      (* Calcul de l'arbre de Huffman de d�part. *)

    arbre_vide := construit_huff (int2leaf !l_v);
    arbre_huff := !arbre_vide;

      (* Boucle de d�codage. *)

    for i = 5 to (in_channel_length !ch_e) do
      tampon := (!tampon @ (transforme (input_byte !ch_e)));
      while ((!compteur < !longueur) & (!tampon <> []))
      do decode() done
    done;

    ferme nom_fich
  end;;

(* Fin. Pour d�compr�sser: expanse "nom";; *)
