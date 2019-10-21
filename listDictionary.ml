 
open Dictionary



(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association 
    list [lst] as a dictionary.  The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively.  The
    [fmt] argument is where to put the formatted output. *)
let format_assoc_list format_key format_val fmt lst =

  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a -> %a; "
                format_key k format_val v) lst;
  Format.fprintf fmt "]"

module Make : DictionaryMaker
  = functor (K : KeySig) -> functor (V : ValueSig) ->
  struct
    module Key = K
    module Value = V
    type key = K.t
    type value = V.t

    (** Abstraction Function: the associated list [(k1,v1);...(kn, vn)] 
      * represents the key value pair containing all keys and values. 
      * [] represents the empty list.
        Representation Invariant: There may be no duplicate key  *)
    type t = (key * value) list

    (** [get_key d acc] gets the list of keys in [d] by appending to the 
        accumulator [acc]. *)
    let rec get_keys (d:t) (acc: key list) : (key list) = 
      match d with 
      | [] -> acc
      | (k,v) :: t -> get_keys t (k::acc)

    (** [rec_compare k1 k2] compares [k1] and [k2] and returns integer 
        comparator results. *)
    let rec rec_compare k1 k2 = 
      match Key.compare k1 k2 with 
      | LT -> -1
      | EQ -> 0
      | GT -> 1

    let rep_ok d = 
      let key_list_length = List.length (get_keys d []) in
      let new_list = List.sort_uniq rec_compare (get_keys d []) in 
      if key_list_length != List.length(new_list) then raise (Failure "rep inv") 
      else d

    let empty =  []

    let is_empty d =
      match d with 
      | [] -> true
      | h :: t -> false

    let size d =
      List.length (rep_ok d)

    let insert k v d = 
      let keys = get_keys d [] in 
      if not(List.mem k keys) then rep_ok ((k,v)::d)
      else rep_ok ((k,v):: (List.remove_assoc k d) )

    let remove k d =
      rep_ok(List.remove_assoc k d)

    let find k d =
      List.assoc_opt k d 

    let member k d =
      List.mem_assoc k d

    let choose d =
      match d with 
      | [] -> None
      | (k, v) :: t -> Some (k, v) 

    (** [to_list_helper key_list d acc] is the helper function for to_list
        which takes [key_list] and finds all the bindings of each key in [d] and 
        returns the result appended onto the accumulator [acc]. *)
    let rec to_list_helper key_list (d : t) (acc : (key * value) list) =
      match key_list with 
      | [] -> acc
      | h :: t -> 
        match (find h d) with 
        | None -> to_list_helper t d acc
        | Some v -> to_list_helper t d ((h, v) :: acc)

    let to_list d =
      let key_list = get_keys d [] in 
      let sorted_list = List.sort_uniq rec_compare (key_list) in 
      to_list_helper sorted_list d []

    (** [fold_helper f acc d] is the helper function for fold which applies [f] 
        to [d]. *)
    let rec fold_helper f acc d  =
      match d with 
      | [] -> acc
      | (k, v) :: t -> fold_helper f (f k v acc) t

    let fold f init d =
      let sorted_list = to_list d in 
      fold_helper f init sorted_list

    let format fmt d =
      match d with 
      | [] -> Format.fprintf fmt "[]"
      | (k, v) :: t -> format_assoc_list Key.format Value.format fmt t

  end