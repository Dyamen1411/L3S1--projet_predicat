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
let rec string_of_formule (f : formule) : string = 
  match f with
  | Atome s -> s
  | Et (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " * "; string_of_formule g; ")" ]
  | Ou (f, g) -> 
      String.concat ""
        [ "("; string_of_formule f; " | "; string_of_formule g; ")" ]
  | Imp (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " -> "; string_of_formule g; ")" ]
  | Non(f) -> 
      String.concat ""
        [ "(! "; string_of_formule f;")" ]
  | Top -> "T"
  | Bot -> "⊥"

(* ----------------- Opérateurs de simplification ----------------- *)

(** Opérateur disjonction, associatif à gauche. *)
let ( + ) (f : formule) (g : formule) : formule =
  match (f, g) with
  | Bot, _ -> g
  | _, Bot -> f
  | Top, _ 
  | _, Top -> Top
  | _ -> Ou (f, g)

(** Opérateur de conjonction, associatif à gauche. *)
let ( * ) (f : formule) (g : formule) : formule =
    match (f, g) with
    | Bot, _
    | _, Bot -> Bot
    | Top, _ -> g
    | _, Top -> f
    | _ -> Et (f, g)

(** Opérateur d'implication, associatif à droite. *)
let ( ^-> ) (f : formule) (g : formule) : formule =
    match (f, g) with
    | Bot, _ -> Top
    | Top, g -> g
    | _ -> Imp (f, g)

(** Opérateur de négation. *)
let ( ~~ ) (f : formule) : formule =
    match f with
    | Bot -> Top
    | Top -> Bot
    | Non(Non f) -> f
    | _ -> Non f

(* ----------------- Lecture depuis un fichier ----------------- *)

(** Transforme une chaine +at en Atome at et -at en Non (Atome at). *)
let string_to_lit (str : string) : formule =
  let l = String.length str in
  if l < 2
    then failwith ("Expected signed atom, got " ^ str)
    else
      let (s, a) = String.(sub str 0 1, Atome (sub str 1 (l - 1))) in
      match s with
        | "+" -> a
        | "-" -> Non a
        | _ -> failwith ("Excected sign, got " ^ s)

(** Transforme une chaine contenant des éléments de la forme
    +at ou -at séparés par des espaces ou tabulations en une disjonction des
    formules obtenues en appliquant string_to_lit sur chaque élément. *)
let string_to_disj_opt (str : string) : formule option =
  let atomes =
    List.map
      (string_to_lit)
      (List.filter
        (fun s -> "" <> s)
        (List.fold_left
          (@)
          []
          (List.map
            (String.split_on_char '\t')
            (String.split_on_char ' ' str)
          )
        )
      )
  in match atomes with
  | [] -> None
  | h::t -> Some (List.fold_left (fun f a -> Ou (f, a)) h t)

(** Transforme un fichier texte dont le nom est donné en paramètre et dont chaque ligne est une chaine
    contenant des éléments de la forme
    +at ou -at séparés par des espaces ou tabulations en la conjonction des formules obtenues
    en appliquant string_to_disj sur chaque ligne. *)
let from_file (str : string) : formule = 
  let lines = String.split_on_char '\n' str in
  let disjs' = List.map (string_to_disj_opt) lines in
  let disjs = List.filter_map (Fun.id) disjs' in
  match disjs with
  | [] -> Top
  | h::t -> List.fold_left (fun f d -> Et (f, d)) h t
