open Proposition
open Formule

(* Adapteurs de code *)

(** Création d'une formule équivalente à la formule UnSeul xs, depuis une liste de formule xs.  *)
let unSeul = List.fold_left ( ^+ ) Bot

(** Création d'une formule équivalente à la formule Tous xs, depuis une liste de formule xs.  *)
let tous = List.fold_left ( * ) Top
