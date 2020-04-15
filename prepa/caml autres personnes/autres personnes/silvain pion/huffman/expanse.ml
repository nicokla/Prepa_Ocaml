
      	(* Projet de programmation: outils de compression.
              fichier expanse.ml : decompression  *)



type 'a arbre  = Node of mutable ('a arbre * 'a arbre) | Leaf of mutable 'a
and  direction = Gauche | Droite;;


let ch_e          = ref std_in      (* contiendra le canal du fichier a comp *)
and ch_s          = ref std_out       (* contiendra le canal du fichier comp *)
and occurences    = make_vect 256 0  (* les occurences des divers caracteres *)
and arbre_huffman = ref (Leaf 0)            (* contiendra l'arbre de Huffman *)
and arbre_partiel = ref (Leaf 0)                   (* variable intermediaire *)
and li            = ref [(0,0)]                              (* accumulateur *)
and tampon        = ref [Gauche]       (* contiendra les G/D a coder en bits *)
and longueur      = ref 0       (* contiendra la longueur du fichier decompr *)
and compteur      = ref 0        (* pour savoir ou on en est dans la decompr *)
and carac         = ref 0;;                        (* variable intermediaire *)



  (* Insertion d'un element dans une liste de paires triee par ordre *)
  (* croissant du second element, en otant les occurences vides.     *)

let rec insert =
  fun (_,0) l1      -> l1
    |  a    []      -> [a]
    |  a    (b::l2) -> if (snd a) <= (snd b) then a::b::l2
                                             else b::(insert a l2);;


  (* Rend le 1er element d'une liste non vide *)

let head =
  fun [] -> (Leaf 0,0)
    | l  -> hd l;;



  (* Parcours l'arbre de huffman, et rend la liste des paires *)
  (* composees des feuilles et de leurs direction list.       *)

let rec construit_huff =
  fun []                -> []
    | [(p,q)]           -> [(p,q)]
    | ((p,q)::(r,s)::l) -> construit_huff (insert (Node (p,r),q+s) l);;



  (* Prend "nom_du_fichier_a_decompresser.zz" en argument, *)
  (* rend unit, en produisant le fichier decompresse.      *)

let expanse nom_fich =
  begin

      (* Clair. *)

    let rec change =
      fun 0 -> Gauche
        | _ -> Droite


      (* S'occupe d'ouvrir les fichiers. *)

    and ouvre ()=
      begin
        print_newline();
        print_string ("Decompression en cours de: " ^ nom_fich);
        print_newline();
        ch_e := (open_in_bin nom_fich);
        ch_s :=
            (open_out_bin (sub_string nom_fich 0 ((string_length nom_fich)-3)))
      end


      (* S'occupe de fermer les fichiers. *)

    and ferme ()=
      begin
        print_newline();
	print_string ("Decompression de " ^ nom_fich ^ " terminée.");
	print_newline();
	close_in !ch_e;
        close_out !ch_s
      end


      (* Remplit les occurences en lisant le fichier a decompresser. *)

    and remplit_occ () =
      begin
        for i = 0 to 255 do
          occurences.(i) <- (input_binary_int !ch_e);
	  longueur := (!longueur + occurences.(i));
          (if (occurences.(i) <> 0) then (carac := i) else ())
        done
      end


      (* Prend un octet, et rend la direction list de longueur 8 associee. *)

    and transforme octet =
      let liste = ref []
      and accu  = ref 0
        in
	begin
          liste := [];
	  accu := octet;
          for i=1 to 8 do
            liste := (change (!accu mod 2))::(!liste);
            accu  := (!accu / 2)
          done;
          !liste
        end


      (* Produit la liste des paires (caracteres, occurences), *)
      (* triee par occurences croissantes.                     *)

    and liste_croissante ()=
      begin
        for i=0 to 255 do (li:= insert (i,occurences.(i)) !li) done;
        !li
      end


    (* A partir de la 'direction' donnee par la tete de tampon, fait avancer *)
    (* l'arbre partiel (sous arbre de arbre_huffman), ou inscrit dans le     *)
    (* fichier decompresse le caractere trouve, et remet arbre_huffman dans  *)
    (* l'arbre partiel.                                                      *)

    and decode () =
      let dir = (ref Gauche) in
      begin
        match !arbre_partiel with

          (Leaf l) ->
            begin
              arbre_partiel := !arbre_huffman;
              output_byte (!ch_s) l;
              incr compteur
            end

         | (Node n) ->
            (begin
              dir := hd !tampon;
              tampon := tl !tampon;
              (if !dir = Gauche then (arbre_partiel := fst n)
                                else (arbre_partiel := snd n));
              match !arbre_partiel with
                 (Leaf l) -> decode()
               | (Node n) -> ()

            end)
      end


  (* Corps imperatif de la fonction expanse: string -> unit. *)

    in
    begin


        (* Initialisation des variables. *)

      li:=[];
      longueur := 0;
      compteur := 0;
      tampon := [];


        (* Lecture des occurences. *)

      ouvre();
      seek_in !ch_e 0;
      remplit_occ ();
      print_string "Longueur du fichier originel: ";
      print_int !longueur;


        (* Calcul de l'arbre de Huffman. *)

      arbre_huffman :=
        fst (head (
          construit_huff (
            map
              (fun (l,i) -> (Leaf l,i))
              (liste_croissante()) ) ) );


        (* Boucle de decodage, si le fichier comporte plus d'un type *)
        (* de caractere. (ie longueur du fichier compresse >1024)    *)
        (* Dans ce dernier cas, le caractere en question est !carac. *)

      arbre_partiel := !arbre_huffman;
      if ((in_channel_length !ch_e) > 1024) then
        for i = 1025 to (in_channel_length !ch_e) do
          tampon := (!tampon @ (transforme (input_byte !ch_e)));
          while ((!compteur < !longueur) & (list_length !tampon <> 0))
          do decode() done
        done
      else
        for i=1 to occurences.(!carac)
        do (output_byte !ch_s !carac) done;

      ferme ()
    end
  end;;


(* Code fini.

   Exemple:

   expanse "essai_huff0_d.zz";;
*)
