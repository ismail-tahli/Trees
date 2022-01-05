(* Projet Fait individuellement en 2 Jours
 par : Ismail TAHLI TP3 *)


(* Interface du type ordonné *)
module type Type_ordonne =
  sig
    type t
    val comp : t->t->int
    val alea :  t->t
    val min : t
  end

(* Type entier ordonné avec l'ordre usuel *)
module EntierOrdreNaturel =
    struct
        type t = int;;
        (*Fonction de comparaison pour les entiers*)
        let comp e1 e2 =
            if e1 < e2 then -1
            else if e1 > e2 then 1
            else 0;;  
        let alea a = Random.int a;;
        let min = 0;;
    end


(* Interface du foncteur *)
module type SignatureEnsembleA234 = 
    functor (Type : Type_ordonne) ->
        sig
            type element = Type.t
            type a234 = VideA234
                            | Noeud2 of element * a234 * a234
                            | Noeud3 of (element * element) * a234 * a234 * a234
                            | Noeud4 of (element * element * element) * a234 * a234 * a234 * a234
            type ab
            val search : a234 -> element -> bool 
            val id : a234 -> a234 
            val a234_vers_abic : a234 -> ab 
            val abic_vers_a234 : ab -> a234 
            val insert : element -> a234 -> a234 
            val arbre_alea : element -> int -> a234 
            val min_a234 : a234 -> element 
            val max_a234 : a234 -> element 
            val creer_vide : unit -> a234 
            val est_vide : a234 -> bool 
            val nb_noeuds : a234 -> int 
            val compint : 'a -> 'a -> int 
            val union : a234 -> a234 -> a234 
            val intersection : a234 -> a234 -> a234 
            val difference : a234 -> a234 -> a234 
            val difference_symetrique : a234 -> a234 -> a234 
            val est_inclus_dans : a234 -> a234 -> bool 
            val est_egal_a : a234 -> a234 -> bool 
            val fold_r : (element -> 'a -> 'a) -> a234 -> 'a -> 'a 
            val fold_l : ('a -> element -> 'a) -> 'a -> a234 -> 'a 
            val cardinal : a234 -> int 
            val cardinal_iterer : a234 -> int 
            val filtrer_selon : (element -> bool) -> a234 -> a234 
            val separation : element -> a234 -> a234 * bool * a234 
end

(* Représentation des ensemble avec un arbre 2-3-4 *)
module Creer_mod_ens_A234 : SignatureEnsembleA234 =
  functor (Type : Type_ordonne) ->
    struct
      open Type;;
      type element = t;;
      (* I -  Définition du type et propriétés de base*)
      (* 1 - Type de données arbre234*)
      type a234 = VideA234
                  | Noeud2 of element * a234 * a234
                  | Noeud3 of (element * element) * a234 * a234 * a234
                  | Noeud4 of (element * element * element) * a234 * a234 * a234 * a234;;
      (* En général un nœuds est étiqueté par 1, 2 ou 3 éléments et chaque nœud d’étiquettes 
        ek avec (0 < k < 4) admet k + 1 sous-arbres*)

      (*Définir un opérateur de comparaison à l'aide de la fonction comp qui est propre à chaque ensemble.*)
      let ( $<$ ) x y = comp x y < 0;;
      let ( $>$ ) x y = comp x y > 0;;
      let ( $=$ ) x y = comp x y = 0;;
      (* Type de donnée arbre bicolore*)
      type color = Rouge | Noir | DoubleNoir;;
      type ab = Vide | VideNoir | Noeud of element * color * ab * ab;;
      
      (* 2 - N'est pas fait *)

      (* 3 - Fonction de recherche *)
      let rec search a x = 
        match a with
      | VideA234 -> false
      | Noeud2(r,g,d) -> x = r || search (if x $<$ r then g else d) x
      | Noeud3((r1,r2),a1,a2,a3) -> x = r1 || x = r2 || search (if x $<$ r1 then a1 else if x $<$ r2 then a2 else a3) x
      | Noeud4((r1,r2,r3),a1,a2,a3,a4) -> x = r1 || x = r2 || x = r3 || search (if x $<$ r1 then a1 else if x $<$ r2 then a2 else if x $<$ r3 then a3 else a4) x
      ;;

      (* II - Conversion *)

      (* 1 et 2 - Fait en PDF*)

      (* 3 - 1 Conversion arbre 234 en arbre bicolore *)

      let id a =
        match a with
        | VideA234 -> VideA234
        | Noeud2(e, sg, sd) -> Noeud2(e, sg, sd)
        | Noeud3((e1, e2), a1, a2, a3) -> Noeud3((e1, e2), a1, a2, a3)
        | Noeud4((e1, e2, e3), a1, a2, a3, a4) -> Noeud4((e1, e2, e3), a1, a2, a3, a4);;

      let rec a234_vers_abic a =
        match a with
        | VideA234 -> Vide
        | Noeud2(e1, sa1, sa2) -> Noeud(e1, Noir, (a234_vers_abic sa1), (a234_vers_abic sa2))
        | Noeud3((e1, e2), sa1, sa2, sa3) -> Noeud(e2, Noir, Noeud(e1, Rouge, (a234_vers_abic sa1), (a234_vers_abic sa2)), (a234_vers_abic sa3))
        | Noeud4((e1, e2, e3), sa1, sa2, sa3, sa4) -> 
            Noeud(e2, Noir, Noeud(e1, Rouge, (a234_vers_abic sa1), (a234_vers_abic sa2)), Noeud(e3, Rouge, (a234_vers_abic sa3), (a234_vers_abic sa4)))
        ;;

      (* Conversion arbre bicolore en arbre 234 *)

      (*1 - 2  on démontrons à l'envers*)
      let rec abic_vers_a234 a =
        match a with
        | Vide -> VideA234
        | Noeud(e2, Noir, Noeud(e1, Rouge, a1, a2), Noeud(e3, Rouge, a3, a4)) ->
                Noeud4((e1, e2, e3), (abic_vers_a234 a1), (abic_vers_a234 a2), (abic_vers_a234 a3), (abic_vers_a234 a4))
        | Noeud(e1, Noir, a1, Noeud(e2, Rouge, a2, a3))
        | Noeud(e2, Noir, Noeud(e1, Rouge, a1, a2), a3) -> Noeud3((e1, e2), (abic_vers_a234 a1), (abic_vers_a234 a2), (abic_vers_a234 a3))
        | Noeud(e, Noir, sg, sd) -> Noeud2(e, (abic_vers_a234 sg), (abic_vers_a234 sd))
        | _ -> failwith "Erreur de conversion";;

      (*Insertion dans un arbre 2–3–4*)  

      let rec insert x a =
        match a with
        | VideA234 -> Noeud2(x, VideA234, VideA234)
        | Noeud2(e1, Noeud4((e2, e3, e4), a1, a2, a3, a4), a5) when (e1 $>$ x) ->
            (insert x (Noeud3((e3, e1), Noeud2(e2, a1, a2), Noeud2(e4, a3, a4), a5)))
        | Noeud2(e1, a1, Noeud4((e2, e3, e4), a2, a3, a4, a5)) when (e1 $<$ x) ->
            (insert x (Noeud3((e1, e3), a1, Noeud2(e2, a2, a3), Noeud2(e4, a4, a5))))
        | Noeud3((e1, e2), Noeud4((e3, e4, e5), a3, a4, a5, a6), a1, a2) when (e1 $>$ x) ->
            (match (comp e4 x) with
            | 1 -> Noeud4((e4, e1, e2), (insert x (Noeud2(e3, a3, a4))), Noeud2(e5, a5, a6), a1, a2)
            | -1 -> Noeud4((e4, e1, e2), Noeud2(e3, a3, a4), (insert x (Noeud2(e5, a5, a6))), a1, a2)
            | _ -> a)
        | Noeud3((e1, e2), a1, Noeud4((e3, e4, e5), a3, a4, a5, a6), a2) when (e1  $>$ x)&& (e2 $>$ x) ->
            (match (comp e4 x) with
            | 1 -> Noeud4((e1, e4, e2), a1, (insert x (Noeud2(e3, a3, a4))), Noeud2(e5, a5, a6), a2)
            | -1 -> Noeud4((e1, e4, e2), a1, Noeud2(e3, a3, a4), (insert x (Noeud2(e5, a5, a6))), a2)
            | _ -> a)
        | Noeud3((e1, e2), a1, a2, Noeud4((e3, e4, e5), a3, a4, a5, a6)) when (e2 $<$ x) ->
            (match (comp e4 x) with
            | 1 -> Noeud4((e1, e2, e4), a1, a2, (insert x (Noeud2(e3, a3, a4))), Noeud2(e5, a5, a6))
            | -1 -> Noeud4((e1, e2, e4), a1, a2, Noeud2(e3, a3, a4), (insert x (Noeud2(e5, a5, a6))))
            | _ -> a)
        | Noeud4((e1, e2, e3), a1, a2, a3, a4) as racine when ((id a) = racine) ->
            (insert x (Noeud2(e2, Noeud2(e1, a1, a2), Noeud2(e3, a3, a4))))
        | Noeud4((e1, e2, e3), a1, a2, a3, a4) ->
            (match (comp e1 x) with
            | -1 -> (match (comp e2 x) with 
                            | -1 -> (match (comp e3 x) with
                                            | -1 -> Noeud4((e1, e2, e3), a1, a2, a3, (insert x a4))
                                            | 1 -> Noeud4((e1, e2, e3), a1, a2, (insert x a3), a4)
                                            | _ -> a)
                            | 1 -> Noeud4((e1, e2, e3), a1, (insert x a2), a3, a4)
                            | _ -> a)
            | 1 -> Noeud4((e1, e2, e3), (insert x a1), a2, a3, a4)
            | _ -> a)                   
        | Noeud2(e, VideA234, VideA234) -> 
            (match (comp e x) with
            | -1 -> Noeud3((e, x), VideA234, VideA234, VideA234)
            | 1 -> Noeud3((x, e), VideA234, VideA234, VideA234)
            | _ -> a) 
        | Noeud2(e, sg, sd) -> 
            (match (comp e x) with
            | 1 -> Noeud2(e, (insert x sg), sd) 
            | -1 ->  Noeud2(e, sg, (insert x sd))
            | _ -> a)
        | Noeud3((e1, e2), VideA234, VideA234, VideA234) -> 
            (match (comp e1 x) with
            | -1 -> 
                (match (comp e2 x) with
                | -1 -> Noeud4((e1, e2, x), VideA234, VideA234, VideA234, VideA234)
                | 1 -> Noeud4((e1, x, e2), VideA234, VideA234, VideA234, VideA234)
                | _ -> a)
            | 1 -> Noeud4((x, e1, e2), VideA234, VideA234, VideA234, VideA234)
            | _ -> a)
        |   Noeud3((e1, e2), sa1, sa2, sa3) ->
            (match(comp e1 x) with
            | -1 ->
                (match (comp e2 x) with
                | -1 -> Noeud3((e1, e2), sa1, sa2, (insert x sa3))
                | 1 -> Noeud3((e1, e2), sa1, (insert x sa2), sa3)
                | _ -> a)
            | 1 -> Noeud3((e1, e2), (insert x sa1), sa2, sa3)
            | _ -> a)
        ;;

      (*Fonction qui retourne des arbres aléatoires d'un ensemble ordonné "PAS FORCEMENT D'ENTIERS"*)
      let arbre_alea bound nb = (*Avec nb Nombre de valeur et bound la valeur maximal*)
        let rec aux acc = function
          | 0 -> acc
          | n -> aux (insert (alea bound) acc) (n - 1) in (*La fonction alea est propre à chaque ensemble ordonné 
                                                            et elle retoune une valeur aléatoire de cet ensemble*)
        aux VideA234 nb;;

      (*Suppression dans un arbre 2–3–4 | INCOMPLETE*)  
      
      let min_a234 a =
        let rec aux a min =
            match a with
            | VideA234 -> min
            | Noeud2(e, a1, a2) -> (aux a1 e)
            | Noeud3((e1, e2), a1, a2, a3) -> (aux a1 e1)
            | Noeud4((e1, e2, e3), a1, a2, a3, a4) -> (aux a1 e1)
              in (aux a min);;

      let max_a234 a =
        let rec aux a max =
            match a with
            | VideA234 -> max
            | Noeud2(e, a1, a2) -> (aux a2 e)
            | Noeud3((e1, e2), a1, a2, a3) -> (aux a3 e2)
            | Noeud4((e1, e2, e3), a1, a2, a3, a4) -> (aux a4 e3)
        in (aux a min);;

(*     let rec delete_a234 x a =
           match a with
          | VideA234 -> VideA234
          | Noeud2(e1, Noeud2(e2, a1, a2), Noeud2(e3, a3, a4)) ->
                  (delete_a234 x (Noeud4((e1, e2, e3), a1, a2, a3, a4)))
          | Noeud3((x, e), VideA234, VideA234, VideA234)
          | Noeud3((e, x), VideA234, VideA234, VideA234) -> 
                  Noeud2(e, VideA234, VideA234)
          | Noeud4((x, e1, e2), VideA234, VideA234, VideA234, VideA234)
          | Noeud4((e1, x, e2), VideA234, VideA234, VideA234, VideA234)
          | Noeud4((e1, e2, x), VideA234, VideA234, VideA234, VideA234) ->
                  Noeud3((e1, e2), VideA234, VideA234, VideA234)
          | Noeud3((e1, e2), a1, a2, a3) ->
              (match (compare e1 x) with
              | -1 -> (match (compare e2 x) with
                              | -1 -> Noeud3((e1, e2), a1, a2, (delete_a234 x a3))
                              | 1 -> Noeud3((e1, e2), a1, (delete_a234 x a2), a3)
                              | _ -> a)) 
          ;;  *)

      (*5 - Manipulation d’ensembles*)

      (* REMARQUE : les manipulations sont faite grâce à un arbre bicolore correspondant à l'arbre 2-3-4 courant 
                    (utilisation des fonctions de conversion a234_vers_abic et abic_vers_a234) *)
      (* La conversion des arbres 2-3-4 en arbres bicolores facilite le traitement. *)

      (* Quelques Outlis*)
      (*Créer un arbre 2-3-4 vide*)
      let creer_vide() = VideA234;;
        
      (*Determine si l'abre 2-3-4 passée en paramétre est vide*)
      let est_vide = function
      | VideA234 -> true
      | _ -> false;;
      
      (*Fonction qui calcule le nombre de noeuds de l'arbre 2-3-4 passée en paramétre.*)
      let nb_noeuds arbre = 
        let rec aux a =
        match a with
          | Vide | VideNoir -> 0
          | Noeud(_, _, g, d) -> 1 + (aux g) + (aux d)
        in aux (a234_vers_abic arbre);;
      
      (*Fonction de comparaison des entiers.*)
      let compint a b =
        if a < b then -1
        else if a > b then 1
        else 0;;


      (* Les Fonctions ensemblistes *)

      (* L'union EN UTLISANT QUE LES ARBRES 2-3-4 *)
      let union arbre_a arbre_b =
        let rec aux a b =
            match a with
            | VideA234 -> b
            | Noeud2(e, a1, a2) -> aux a1(aux a2 (insert e b))
            | Noeud3((e1, e2), a1, a2, a3) -> aux a1 (aux a2(aux a3 (insert e1 (insert e2 b))))
            | Noeud4((e1, e2, e3), a1, a2, a3, a4) -> aux a1(aux a2 (aux a3(aux a4 (insert e1(insert e2 (insert e3 b))))))
        in match compint (nb_noeuds  arbre_a) (nb_noeuds arbre_b) with 
        | 1 -> aux arbre_b arbre_a (*si le cardinal de arbre_a est supérieur strictement à arbre_b *)
        | _ -> aux arbre_a arbre_b (*Sinon*)
        ;;

      (* Intersection *)
      let intersection a b =
        let rec aux a b c =
            match a with
            | Vide | VideNoir -> c  
            | Noeud(x, _, Vide, Vide) -> if (search (abic_vers_a234 b) x) then (a234_vers_abic (insert x (abic_vers_a234 c))) else c
            | Noeud(x, _, g, d) -> if(search (abic_vers_a234 b) x) then
                                                            (aux g b (aux d b (a234_vers_abic (insert x (abic_vers_a234 c)))))
                                                        else
                                                            (aux g b (aux d b c))
        in match (compint (nb_noeuds a) (nb_noeuds b)) with 
        | 1 -> (abic_vers_a234 (aux (a234_vers_abic b) (a234_vers_abic a) Vide))
        | _ -> (abic_vers_a234 (aux (a234_vers_abic a) (a234_vers_abic b) Vide));;

      (* Différence *)     
      let difference a b =
        let rec aux a b c =
            match a with
            | Vide | VideNoir -> c
            | Noeud(x, _, Vide, Vide) -> if (search (abic_vers_a234 b) x) then c else (a234_vers_abic (insert x (abic_vers_a234 c)))
            | Noeud(x, _, g, d) -> if((search (abic_vers_a234 b) x)) then                                                       
                                                            (aux g b (aux d b c))
                                                        else
                                                            (aux g b (aux d b (a234_vers_abic (insert x (abic_vers_a234 c)))))
            in (abic_vers_a234 (aux (a234_vers_abic a) (a234_vers_abic b) Vide));;
      
      (* Différence Symétrique*)     
      let difference_symetrique a b = (union (difference a b) (difference b a));;

      (* Inclusion *)
      let est_inclus_dans e f = ((union e f) = f);;
      
      (* Egalité *)
      let est_egal_a e f = (est_inclus_dans e f) && (est_inclus_dans f e);;


      (* Question 6 - Fonctions de haut niveau *)

      (* Les équivalents de la fonction fold sur les listes.*)
      let fold_r g e x =
        let rec aux g e x =
        match e with
        | Vide | VideNoir -> x
        | Noeud(elt, _, sg, sd) -> g elt (aux g sg (aux g sd x))
        in (aux g (a234_vers_abic e) x);;

      let fold_l g x e =
        let rec aux g x e =
        match e with
        | Vide | VideNoir -> x
        | Noeud(elt, _, sg, sd) -> aux g (aux g (g x elt) sg) sd
        in (aux g x (a234_vers_abic e));;
        
      (*Le cardinal*)
      let cardinal_iterer e =
        (fold_r (function a -> function x -> x+1) e 0);;
        (* Ou fold_l (function a -> function x -> a+1) 0 e ;;*)
      let cardinal e =
        let rec aux e =
        match e with
        | Vide | VideNoir -> 0
        | Noeud(_, _, g, d) -> 1 + (aux g) + (aux d)
        in (aux (a234_vers_abic e));;


      (*La fonction de filtrage *)

      let filtrer_selon g e =
        let rec aux g e acc =
            match e with
            | Vide | VideNoir -> acc
            | Noeud(x, _, Vide, Vide) -> if (g x) then (insert x acc) else acc
            | Noeud(x, _, sg, sd) -> if (g x) then (aux g sg (aux g sd (insert x acc)))
                                                            else (aux g sg (aux g sd acc))
        in (aux g (a234_vers_abic e) VideA234);;

      (*La fonction de séparation*)

      let separation x e =
        let inf = function y -> y < x
        and sup = function y -> y > x
        in ((filtrer_selon inf e), (search e x), (filtrer_selon sup e));;
    end;;

(* Ensemble d'entiers représenté avec un arbre 2-3-4 *)
module Int234 = Creer_mod_ens_A234(EntierOrdreNaturel);;
(*Un arbre aleatoire de 20 éléments compris en 0 et 100.*)
let ens = (Int234.arbre_alea 100 20);; 
let ens2 = (Int234.arbre_alea 100 10);;     
(* Test du programme : 
  #use "rendu.ml";; *)
