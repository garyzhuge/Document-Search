module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val words : idx -> string list
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end
(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"
module Make = 
  functor (S:DictionarySet.Set with type Elt.t = string)
    -> functor (D:Dictionary.Dictionary with type Key.t = string
                                         and type Value.t = S.t) 
    -> struct

      (* TODO: replace [unit] with a type of your own design. *)
      (** AF: Index is a dictionary where each key is a valid word and its 
          corresponding values is a set of files that the word is found in.
          RI: 
            No duplicate keys are allowed
            Every elemeent in the values list must be distinct. *)

      type idx = D.t


      (** [get_file_names path d] returns all the .txt files in [d] from [path]
          by checking whether each file name in d contains .txt and appending 
          to acc if it does.*)
      let rec get_file_names path (d:Unix.dir_handle) (acc:string list) = 
        try(
          let () = Printf.printf("IN GET File NAMEs\n") in
          let name = Unix.readdir d in 
          let () = Printf.printf("Directory item name = %s\n") (name) in
          if Filename.check_suffix name ".txt" then 

            let () = Printf.printf("ACC = %s\n") (path^Filename.dir_sep^name) in
            get_file_names path d ((path^Filename.dir_sep^name)::acc)
          else get_file_names path d acc)
        with
        |End_of_file -> Unix.closedir d; acc


      (** [read_line in acc] reads a line of [ic] and concatenates it to acc *)
      let rec read_line (ic:in_channel) acc = 
        try 
          let line = input_line ic in 
          let () = Printf.printf("INDIVIDUAL LINE= %s") (line) in
          read_line ic acc^line^" "
        with 
          End_of_file -> acc

      (** [get_file_content file_name] returns a string of the entire content in 
          [file_name] without newline characters *)
      let get_file_content (file_name : string) : string = 
        let input_channel = open_in file_name in 


        read_line input_channel ""

      (**[left_boundary alphanumeric s index] returns the index of the starting 
         boundary of a valid word [s], which is a character in regular expression
          [rg]. Index is the starting place.

         Requires: [rg] is valid regular expression
            [s] is a string of length > 0
            [index] is an int. *)
      let rec left_boundary rg s index =
        try begin
          if Str.string_match rg s index then index
          else left_boundary rg s (index + 1)
        end 
        with Invalid_argument s -> -1
      (**[left_boundary alphanumeric s index] returns the index of the ending 
         boundary of a valid word [s], which is a character in regular expression
          [rg]. Index is the starting place.

         Requires: [rg] is valid regular expression
           [s] is a valid string of length > 0
           [index] is a valid int. *)
      let rec right_boundary rg s index = 
        try(
          if Str.string_match rg s index then index
          else right_boundary rg s (index - 1))
        with Invalid_argument s -> -1

      (** [make_valid_word str] transforms str into a valid word. *)
      let make_valid_word str = 
        let () = Printf.printf("IN MAKE VALID WORD\n") in
        let trimmed_word = String.trim str in 
        let () = Printf.printf("PRINT TRIMMED WORD= %s\n") (trimmed_word) in
        let alphanumeric = Str.regexp "[A-Za-z0-9]" in 
        (* Index of the starting character *)
        let s = left_boundary alphanumeric trimmed_word 0 in
        let () = Printf.printf("index of starting char= %d\n") (s) in
        (* Index of the ending character *)
        let e = right_boundary alphanumeric trimmed_word  ((String.length trimmed_word) - 1) in
        let () = Printf.printf("index of ending char= %d\n") (e) in 
        if s = -1 && e = -1 then ""

        else if s = e then Char.escaped (String.get trimmed_word s)
        else
          let sum = e-s+1 in 
          let () = Printf.printf("SUM is= %d\n") (sum) in 
          let trimmed = String.sub trimmed_word s sum in 
          let () = Printf.printf("Trimmed is = %s\n") (trimmed) in 
          String.lowercase_ascii trimmed 

      (** [make_word_lst lst acc] transforms each element of [lst] into a valid 
          word and returns the new list of valid words. *)
      let rec make_word_lst lst acc = 
        let () = Printf.printf("IN MAKE WORD LST\n") in
        let ()= Printf.printf("MAKE_WORD_LST = %s\n") (pp_list pp_string lst) in
        match lst with 
        | [] -> acc
        | h :: t -> 
          let word = make_valid_word h in 
          let () = Printf.printf("MAKE VALID WORD %s\n") (word) in
          if List.mem word acc || h = "" then make_word_lst t acc
          else make_word_lst t (word :: acc)
      let replace input output = 
        Str.global_replace (Str.regexp_string input) output
      (** [remove_dups str_lst] returns a string list with all duplicates removed
          from [str_lst] *)
      let remove_dups (str_lst : string list) = 
        List.sort_uniq compare str_lst

      (** [get_words file_name] returns a (str list * str) tuple where the first
          element is the file name and the second elements is a string list of 
          all parsed words from [file_name]. *)
      let get_words file_name =
        let content = get_file_content file_name in
        let () = Printf.printf("CONTENT*********************** = %s") (content) in
        let whitespaces = Str.regexp "[\t\n]" in 
        let repl_cont_with_space = (Str.global_replace whitespaces " " content) in 
        let lst = repl_cont_with_space |> String.split_on_char ' ' |> 
                  List.filter(fun x -> not(x = "")) in 
        let () = Printf.printf("WORD LIST AFTER SPLIT = %s") (pp_list pp_string lst) in
        let word_lst = make_word_lst lst [] in
        let ()= Printf.printf("MAKE_WORD_LST = %s\n") (pp_list pp_string lst) in
        (word_lst, file_name)

      (** [insert_pair idx word_list file_name] inserts each word in [word_list]
          as keys to [idx], where each key has value [file_name]. *)
      let rec insert_pair idx word_list file_name = 
        let () = Printf.printf("IN INSERT PAIR\n") in
        let value_set = S.insert file_name S.empty in
        match word_list with 
        | [] -> idx
        | h :: t -> insert_pair (D.insert h value_set idx) t file_name

      (** [make_dict file_name] makes an index dictionary for every word in 
          [file_name]. *)
      let make_dict file_name idx = 
        let () = Printf.printf("IN MAKE DICT\n") in
        let (fst, snd) = get_words file_name in 
        insert_pair idx fst snd

      (** [make_index idx] iterates through the list of [txt_files] and inserts
          the contents of each file into [idx]. *)
      let rec make_index txt_files idx = 
        let () = Printf.printf("IN MAKE INDEX\n") in
        match txt_files with
        | [] -> idx
        | h :: t -> make_index t (make_dict h idx)

      let index_of_dir d = 

        let () = Printf.printf("IN MAKE INDEX\n") in
        let dirhandle = Unix.opendir d in 

        let txt_files = get_file_names d dirhandle [] in 
        let () = Printf.printf("AFTER TXT\n") in
        make_index txt_files D.empty


      (** [words_helper idx acc] recursively appends each key in [idx] to the 
          accumulator [acc]. *)
      let rec words_helper idx acc = 
        let opt = D.choose idx in 
        match opt with 
        | None -> acc
        | Some (k, v) -> words_helper (D.remove k idx) (k :: acc)

      let words idx = 
        words_helper idx []

      (** [get_some opt] returns the value attached to [opt] if [opt] is Some 
          Raises : Not_found if opt is None*)
      let get_some opt =
        match opt with
        | None -> raise Not_found
        | Some t -> t

      (** [to_list_helper key_lst acc idx] returns (string * string list) list
          for each key in [key_lst] from [idx] by appending to [acc]. *)
      let rec to_list_helper key_lst acc idx = 
        match key_lst with 
        | [] -> acc
        | h :: t ->
          let opt = D.find h idx in 
          let val_set = get_some opt in 
          let val_lst = S.to_list val_set in 
          to_list_helper t ((h, val_lst) :: acc) idx

      let to_list idx =
        let words_lst = words idx in 
        to_list_helper words_lst [] idx

      (** [to_lower str_lst acc] returns a list of strings of all words in 
          [str_lst] transformed to lower cases, removing duplicates in the 
          process. *)
      let rec to_lower str_lst acc = 
        match str_lst with 
        | [] -> acc
        | h :: t -> 
          if List.mem h acc then to_lower t acc 
          else to_lower t (String.lowercase_ascii h :: acc)

      (** [or_not_helper key_lst acc idx] returns a set-like-list of all files 
          that contain at least one word in [key_lst] from [idx] by appending to
          [acc]. *)
      let rec or_not_helper key_lst acc idx = 
        match key_lst with 
        | [] -> acc
        | h :: t ->
          let files_set = get_some (D.find h idx) in 
          let acc' = S.union files_set acc in 
          or_not_helper t acc' idx

      let or_not idx ors nots =
        let ors_files = or_not_helper (to_lower ors []) S.empty idx in
        let nots_files = or_not_helper (to_lower nots []) S.empty idx in 
        let set = S.difference ors_files nots_files in 
        S.to_list set

      (** [and_not_helper key_lst acc idx] returns a set-like-list of all files 
          that contain all words in [key_lst] from [idx] by appending to [acc]. *)
      let rec and_not_helper key_lst acc idx = 
        match key_lst with 
        | [] -> acc
        | h :: t ->
          let files_set = get_some (D.find h idx) in 
          let acc' = S.intersect files_set acc in 
          and_not_helper t acc' idx

      let and_not idx ands nots =
        let and_files = and_not_helper (to_lower ands []) S.empty idx in 
        let nots_files = or_not_helper (to_lower nots []) S.empty idx in 
        let set = S.difference and_files nots_files in 
        S.to_list set

      let format fmt idx =
        D.format fmt idx

    end

