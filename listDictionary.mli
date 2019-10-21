(** Dictionaries implemented as association lists. *)

open Dictionary

(** [MakeListDictionary] makes a [Dictionary] implemented
    with association lists.  *)
module Make : DictionaryMaker
