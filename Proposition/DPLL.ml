open FCC;;

module StringSet = Set.Make(struct
  type t = string
  let compare = String.compare
end)

(** Renvoie la liste des atomes d'une FCC. *)
let atomes_of_fcc (fcc : forme_clausale) =
  StringSet.elements
  (
    FormeClausale.fold
      (
        fun c r ->
          StringSet.union
          r
          (
            Clause.fold
              (fun (_, l) r' -> StringSet.add l r')
              c
              StringSet.empty
          )
      )
      fcc
      StringSet.empty
  )

(** Simplifie la forme clausale fcc en considérant que le littéral lit est vrai *)
let simplif_fcc (fcc : forme_clausale) ((s, n) : litteral) : forme_clausale =
  FormeClausale.filter_map
    (fun c ->
      if Clause.mem (s, n) c
        then None
        else Some (Clause.filter (fun (_, n') -> n' <> n) c)
    )
    fcc
    
(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable. *)
let rec dpll_sat (fcc : forme_clausale) : bool = 
  let atomes = atomes_of_fcc fcc in
  match atomes with
  | [] -> FormeClausale.cardinal fcc = 0
  | (n::_) ->
    dpll_sat (simplif_fcc fcc (Plus, n))
    || dpll_sat (simplif_fcc fcc (Moins, n))


(** Applique l'algorithme DPLL pour déterminer si une fcc est une tautologie. *)
let dpll_tauto (fcc : forme_clausale) : bool =
  let atomes = atomes_of_fcc fcc in
  match atomes with
  | [] -> FormeClausale.cardinal fcc = 0
  | (n::_) ->
    dpll_sat (simplif_fcc fcc (Plus, n))
    && dpll_sat (simplif_fcc fcc (Moins, n))

(** Applique l'algorithme DPLL pour déterminer si une fcc est une contradiction. *)
let dpll_contra (fcc : forme_clausale) : bool =
  let atomes = atomes_of_fcc fcc in
  match atomes with
  | [] -> FormeClausale.cardinal fcc != 0
  | (n::_) ->
    dpll_sat (simplif_fcc fcc (Plus, n))
    && dpll_sat (simplif_fcc fcc (Moins, n))

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let rec dpll_ex_sat (fcc : forme_clausale) : (string * bool) list option =
  match atomes_of_fcc fcc with
  | [] ->
    if FormeClausale.cardinal fcc = 0
      then Some []
      else None
  | (n::_) ->
    match dpll_ex_sat (simplif_fcc fcc (Plus, n)) with
    | Some s -> Some ((n, true)::s)
    | None ->
      match dpll_ex_sat (simplif_fcc fcc (Moins, n)) with
      | Some s -> Some ((n, false)::s)
      | None -> None

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon l'algorithme DPLL. *)
let rec dpll_all_sat (fcc : forme_clausale) : (string * bool) list list =
  match atomes_of_fcc fcc with
  | [] ->
    if FormeClausale.cardinal fcc = 0
      then [[]]
      else []
  | (n::_) ->
    let p = List.map
      (fun s -> (n, true)::s)
      (dpll_all_sat (simplif_fcc fcc (Plus, n)))
    and m = List.map
      (fun s -> (n, false)::s)
      (dpll_all_sat (simplif_fcc fcc (Moins, n)))
    in
    p @ m
