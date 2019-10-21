(** Search engine. *)

(** An [Engine] indexes words found in text files and answers
    queries about which files contain which words.  Words are 
    stored as lowercase by the engine, regardless of their 
    original case in the files.  For example, [OCaml] is
    stored as [ocaml].  Filenames are stored with their original 
    case and extension, but without any path.  For example, 
    [dir/A.txt] is stored as [A.txt]. 

    A larger example: Suppose a directory named [d] contains just two files: 
    [a.txt] and [b.txt].  Suppose that [a.txt] contains just the text
    [OCaml is bae].  Suppose that [b.txt] contains just the
    text [salt BAE].  Then ["d" |> index_of_dir |> to_list] could be
    [
      [("ocaml", ["a.txt"]); 
        ("is", ["a.txt"]);
        ("bae", ["a.txt"; "b.txt"]);
        ("salt", ["b.txt"])]
    ]
    or
    [
      [("ocaml", ["a.txt"]);
        ("bae", ["b.txt"; "a.txt"]);
        ("is", ["a.txt"]);
        ("salt", ["b.txt"])]
    ]
    or several other association lists, but none of those could
    include "OCaml" or "BAE" as a key, and none of the values
    could be "d/a.txt" or "a". *)
module type Engine = sig

  (** The type of an index. *)
  type idx

  (** [index_of_dir d] is an index of the files in [d].  Only files whose
      names end in [.txt] are indexed.  Only [d] itself, not any
      of its subdirectories, is indexed.
      Raises: Not_found if [d] is not a valid directory. *)
  val index_of_dir : string -> idx

  (** [words idx] is a set-like list of the words indexed in [idx]. *)
  val words : idx -> string list

  (** [to_list idx] is a representation of [idx] as an association
      list.  Order in the list is irrelevant.  The keys in the list
      must be equivalent to [words idx], ignoring order.  The value 
      corresponding to a key is a set-like list of the files in which 
      that word appears. *)
  val to_list : idx -> (string * string list) list

  (** [or_not idx ors nots] is a set-like list of the files that contain
      any of the words in [ors] and none of the words in [nots].
      Requires: [ors] is not empty. *)
  val or_not  : idx -> string list -> string list -> string list

  (** [and_not idx ands nots] is a set-like list of the files that contain
      all of the words in [ands] and none of the words in [nots].
      Requires: [ands] is not empty. *)
  val and_not : idx -> string list -> string list -> string list

  (** [format] is a printing function suitable for use
      with the toplevel's [#install_printer] directive.
      It outputs a textual representation of an index
      on the given formatter. *)
  val format : Format.formatter -> idx -> unit

end

(** [Make(S)(D)] makes an [Engine] out of a [Set] and a [Dictionary].  The
    dictionary [D] is used to map from words (i.e., strings) to sets of 
    filenames.  The set [S] is used as the type of values in the 
    dictionary (i.e., the sets of filenames). *)
module Make :
  functor (S:DictionarySet.Set with type Elt.t = string)
    -> functor (D:Dictionary.Dictionary
                with type Key.t = string
                 and type Value.t = S.t) 
    -> Engine