(* You must [make build] before this file will work. 
   Then run [utop] and [#use] this file. 
   VS Code will complain about the # character below.
   That is expected behavior: this file is 
   meant to be used, not compiled.*)

#directory "_build";;
#load_rec "listDictionary.cmo";;

open Dictionary;;

module Int = struct
  type t = int
  let compare x y =
    match Stdlib.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
  let format fmt x =
    Format.fprintf fmt "%d" x
end;;

(* The next line creates a dictionary that maps ints to ints. *)
module IntIntDictionary = ListDictionary.Make(Int)(Int);;

open IntIntDictionary;;

(* The next line installs the custom printer. *)
#install_printer IntIntDictionary.format;;

(* If you evaluate [d], you will note that utop prints it
   as "<unimplemented>".  After completing [ListDictionary.format] 
   you will get more helpful output. *)
let d = empty;;
