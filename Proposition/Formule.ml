(** Type des formules. *)
type formule =
  | Bot
  | Top
  | Atome of string
  | Imp of (formule * formule)
  | Ou of (formule * formule)
  | Et of (formule * formule)
  | Non of formule

(** Fonction de construction d'atome. *)
let atome x = Atome x

(* ----------------- Représentation en chaîne de caractères ----------------- *)

(** Conversion d'une formule en chaîne de caractères. *)
let string_of_formule (_ : formule) : string = failwith "à faire"

(* ----------------- Opérateurs de simplification ----------------- *)

(** Opérateur disjonction, associatif à gauche. *)
let ( + ) (_ : formule) (_ : formule) : formule = failwith "à faire"

(** Opérateur de conjonction, associatif à gauche. *)
let ( * ) (_ : formule) (_ : formule) : formule = failwith "à faire"

(** Opérateur d'implication, associatif à droite. *)
let ( ^-> ) (_ : formule) (_ : formule) : formule = failwith "à faire"

(** Opérateur de négation. *)
let ( ~~ ) (_ : formule) : formule = failwith "à faire"

(* ----------------- Lecture depuis un fichier ----------------- *)

(** Transforme une chaine +at en Atome at et -at en Non (Atome at). *)
let string_to_lit (_ : string) : formule = failwith "à faire"

(** Transforme une chaine contenant des éléments de la forme
    +at ou -at séparés par des espaces ou tabulations en une disjonction des
    formules obtenues en appliquant string_to_lit sur chaque élément. *)
let string_to_disj (_ : string) : formule = failwith "à faire"

(** Transforme un fichier texte dont le nom est donné en paramètre et dont chaque ligne est une chaine
    contenant des éléments de la forme
    +at ou -at séparés par des espaces ou tabulations en la conjonction des formules obtenues
    en appliquant string_to_disj sur chaque ligne. *)
let from_file (_ : string) : formule = failwith "à faire"
