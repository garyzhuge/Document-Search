open Dictionary

module type ElementSig = sig
  type t
  include Dictionary.KeySig with type t := t
end

module type Set = sig
  module Elt : ElementSig
  type elt = Elt.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module Make =
  functor (E : ElementSig) -> functor (DM : DictionaryMaker) ->
  struct
    module Elt = E
    type elt = Elt.t

    (* TODO: change type [t] to something involving a dictionary *)
    (** AF: The dictionary is represented as a set [e1, e2, ...] where each 
        element is the key in the dictionary. The corresponding binding values 
        are irrelevant.
        [] represents the empty dictionary.
        RI: No duplicate keys are allowed in the set. *)
    type t = elt list

    (** [compare_keys k1 k2] compares [k1] and [k2] and returns integer 
        comparator results. *)
    let rec compare_keys k1 k2 = 
      match Elt.compare k1 k2 with 
      | LT -> -1
      | EQ -> 0
      | GT -> 1

    let rep_ok s =
      let orig_length  = List.length s in
      let sorted_length = s |> List.sort_uniq compare_keys |> List.length in 
      if not (orig_length = sorted_length) then raise (Failure "rep inv")
      else s

    let empty = []

    let is_empty s =
      match s with 
      | [] -> true
      | h :: t -> false

    let size s =
      List.length s

    let insert (x:elt) (s:t) =
      if not(List.mem x s) then rep_ok (x :: s)
      else let s' = List.filter (fun y -> not(y = x)) s in 
        rep_ok (x :: s')

    let member x s =
      List.mem x s

    let remove x s =
      List.filter (fun y -> not(y = x)) s

    let choose s =
      match s with 
      | [] -> None
      | h :: _ -> Some h

    (** [fold_helper f acc s] is the helper function for fold which applies [f]
        to [d]. *)
    let rec fold_helper f acc s = 
      match s with 
      | [] -> acc
      | h :: t -> fold_helper f (f h acc) t

    let fold f init s =
      let sorted_list = List.sort compare_keys s in 
      fold_helper f init sorted_list

    let union s1 s2 =
      List.merge compare_keys s1 s2

    let intersect s1 s2 =
      let union_s1_s2 = union s1 s2 in
      List.filter (fun x -> member x s1 && member x s2) union_s1_s2

    let difference s1 s2 =
      List.filter (fun x -> member x s1 && not(member x s2)) s1

    let to_list s =
      List.sort_uniq compare_keys s

    let format_list format_elt fmt lst = 
      Format.fprintf fmt "[";
      List.iter (fun x -> Format.fprintf fmt "%a; " format_elt x) lst;
      Format.fprintf fmt "]"

    let format fmt d =
      match d with 
      | [] -> Format.fprintf fmt "[]"
      | h :: t -> format_list Elt.format fmt t
  end

