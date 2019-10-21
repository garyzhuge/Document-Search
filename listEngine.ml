(* Note that there is nothing you need to complete in this file. 
   All the work you need to do for engines is in [engine.ml]. *)

open Dictionary

(** [StringKey] provides the necessary definitions to use strings
    as keys in dictionaries. *)
module StringKey
  : KeySig with type t = string
=
struct
  (** keys are strings *)
  type t = string

  (** [compare s1 s2] implements a comparison function, as 
      required by [Dictionary.Comparable.compare]. *)
  let compare s1 s2 =
    match Stdlib.compare s1 s2 with
    | x when x < 0 -> LT
    | x when x > 0 -> GT
    | _ -> EQ

  (** [format fmt s] prints string [s] on formatter [fmt] *)
  let format fmt s =
    Format.fprintf fmt "\"%s\"" s
end

(** [S] is a dictionary set implemented with a [ListDictionary]
    whose keys are strings. *)
module S = DictionarySet.Make(StringKey)(ListDictionary.Make)

(** [D] is a [ListDictionary] whose keys are strings. *)
module D = ListDictionary.Make(StringKey)(S)


module ListEngine = Engine.Make(S)(D)
