open Formule

(** Signe d'un littéral. *)
type signe = Plus | Moins

type litteral = signe * string
(** Type d'un littéral : produit d'un signe et d'un atome (string). *)

(** Le module Clause permet de manipuler les ensembles
    de littéraux. Il est généré via le foncteur Set.Make. *)
module Clause = Set.Make (struct
  type t = litteral

  let compare = Stdlib.compare
end)

type clause = Clause.t
(** Type synonyme : une clause est un ensemble de littéraux. *)

(** Le module FormeClausale permet de manipuler les ensembles
    de clauses. Il est généré via le foncteur Set.Make. *)
module FormeClausale = Set.Make (struct
  type t = clause

  let compare = Clause.compare
end)

type forme_clausale = FormeClausale.t
(** Type synonyme : une forme clausale est un ensemble de clauses. *)

(** Transforme un signe en un string *)
let string_of_signe : (signe -> string) = function
  | Plus -> ""
  | Moins -> "¬"

(** Transforme un litteral en string *)
let string_of_litteral ((s, n) : litteral) : string = (string_of_signe s) ^ n

(** Transforme une clause en string *)
let string_of_clause (c : clause) =
  (Clause.fold (fun l r -> r ^ (string_of_litteral l) ^ "; ") c "{") ^ "}"

(** Transforme une forme clausale en string. *)
let string_of_fcc (fc : forme_clausale) : string = 
  (FormeClausale.fold (fun c r -> r ^ (string_of_clause c) ^ "; ") fc "[") ^ "]"

module StringSet = Set.Make(struct
  type t = string
  let compare = String.compare
end)

let atomes_of_clause (c : clause) : string list =
  StringSet.elements (
    Clause.fold
      (fun (_, l) r -> StringSet.add l r)
      c StringSet.empty
  )

(** Renvoie la liste des atomes d'une FCC. *)
let atomes_of_fcc (fcc : forme_clausale) : string list =
  StringSet.elements (
    FormeClausale.fold
      (fun c r -> StringSet.union r (StringSet.of_list (atomes_of_clause c)))
      fcc StringSet.empty
  )

(** 'Optimise' une FCC en:
    - Utilisant le tiers exclu *)
let optimize_fcc : forme_clausale -> forme_clausale =
  let opp = function
    | Plus -> Moins
    | Moins -> Plus
  in
  FormeClausale.filter
    (fun c -> Clause.fold
      (fun (s, n) r -> r || not (Clause.mem (opp s, n) c))
      c true
    )
;;  

(** Mise en FCC, étape 1 : Transforme une formule en une formule équivalente avec des opérateurs 
    de conjonction, de disjonction, de négation, Bot et Top uniquement. *)
let rec retrait_operateurs : formule -> formule = function
  | Imp (f, g) -> Ou (Non (retrait_operateurs f), retrait_operateurs g)
  | Ou (f, g)  -> Ou (retrait_operateurs f, retrait_operateurs g)
  | Et (f, g)  -> Et (retrait_operateurs f, retrait_operateurs g)
  | Non f -> retrait_operateurs f
  | f -> f

(** Mise en FCC, étape 2 : Descend les négations dans une formule au plus profond de l'arbre syntaxique,
    en préservant les évaluations. *)
let rec descente_non (formule : formule) : formule = 
  match formule with
  | Et (f, g) -> Et (descente_non (Non f), descente_non (Non g))
  | Ou (f, g) -> Ou (descente_non (Non f), descente_non (Non g))
  | Non formule' ->
  (
    match formule' with
    | Et (f, g) -> Ou (descente_non (Non f), descente_non (Non g))
    | Ou (f, g) -> Et (descente_non (Non f), descente_non (Non g))
    | Non f -> descente_non f
    | Top -> Bot
    | Bot -> Top
    | _ -> Non formule'
  )
  | _ -> formule

(** Calcule la conjoncion de deux FCC. *)
let fcc_conj = FormeClausale.union

(** Calcule la disjonction de deux FCC. *)
let fcc_disj f1 f2 = FormeClausale.fold
  (fun c1 acc ->
    FormeClausale.fold
      (fun c2 acc' -> FormeClausale.add (Clause.union c1 c2) acc')
      f2 acc
  )
  f1 FormeClausale.empty

(** Mise en FCC, étape 3 : calcule la forme clausale associée à une formule. *)
let rec formule_to_fcc' (formule : formule) : forme_clausale = 
  match formule with
  | Et (f, g) -> fcc_conj (formule_to_fcc' f) (formule_to_fcc' g)
  | Ou (f, g) -> fcc_disj (formule_to_fcc' f) (formule_to_fcc' g)
  | Non (Atome a) -> FormeClausale.singleton (Clause.singleton (Moins, a))
  | Atome a -> FormeClausale.singleton (Clause.singleton (Plus, a))
  | Top -> FormeClausale.empty
  | Bot -> FormeClausale.singleton Clause.empty
  | _ -> failwith "How did you get here ?"

(** Convertit une formule en une forme clausale conjonctive équivalente.*)
let formule_to_fcc f = optimize_fcc
  (formule_to_fcc' (descente_non (retrait_operateurs f)))

(* ----------------- From file ----------------- *)

(** Transforme une chaine +at en (Plus, at) et -at en (Moins, at) *)
let string_to_lit (str : string) : litteral = 
  let l = String.length str in
  if l < 2
    then failwith ("Expected signed atom, got " ^ str)
    else
      let (s, a) = String.(sub str 0 1, sub str 1 (l - 1)) in
      let signe = match s with
        | "+" -> Plus
        | "-" -> Moins
        | _ -> failwith ("Excected sign, got " ^ s)
      in (signe, a)
  
(** Transforme une chaine contenant des éléments de la forme
    +at ou -at séparés par des espaces ou tabulations en une clause contenant
    les littéraux obtenus en appliquant string_to_lit sur chaque élément *)
let string_to_disj (str : string) : clause =
  Clause.of_list
  (List.map
    string_to_lit
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
  )

(** Transforme un fichier texte dont le nom est donné en paramètre et dont chaque ligne est une chaine
    contenant des éléments de la forme
    +at ou -at séparés par des espaces ou tabulations en la FCC contenant les clauses obtenues
    en appliquant string_to_disj sur chaque ligne *)
let from_file (str : string) : forme_clausale = 
  let lines = String.split_on_char '\n' str in
  optimize_fcc (FormeClausale.of_list (List.map (string_to_disj) lines))
