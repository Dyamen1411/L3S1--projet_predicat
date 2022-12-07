open Proposition
open Formule

(* Adapteurs de code *)

(** Création d'une formule équivalente à la formule UnSeul xs, depuis une liste de formule xs.  *)
let unSeul (fs : formule list) : formule = 
  let mapping (i : int) : formule = 
    (function
    | [] -> Bot
    | h::t -> List.fold_left (fun f f' -> Et (f, f')) h t)
    (List.mapi (fun i' f -> if i != i' then Non f else f) fs)
  in
  match fs with
  | [] -> Bot
  | _::t ->
    let (_, r) = List.fold_left
      (fun (i, f) _ -> ((succ i), (Ou (f, (mapping i)))))
      (1, mapping 0)
      t
    in r

(** Création d'une formule équivalente à la formule Tous xs, depuis une liste de formule xs.  *)
let tous (_ : formule list) : formule = failwith "à faire"
