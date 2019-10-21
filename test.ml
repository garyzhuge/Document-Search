(**open OUnit2
   open ListDictionary
   open Dictionary;;
   (* of course add whatever code you want *)
   let cmp_set_like_lists lst1 lst2 =
   let uniq1 = List.sort_uniq compare lst1 in
   let uniq2 = List.sort_uniq compare lst2 in
   List.length lst1 = List.length uniq1
   &&
   List.length lst2 = List.length uniq2
   &&
   uniq1 = uniq2

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

   module String = struct
   type t = string
   let compare x y =

    match Stdlib.compare x y with

    | x when x < 0 -> LT
    | 0 -> EQ
    | _ -> GT
   let format fmt z =
    Format.fprintf fmt "%s" z
   end;;

   let cmp_set_like_lists lst1 lst2 =
   let uniq1 = List.sort_uniq compare lst1 in
   let uniq2 = List.sort_uniq compare lst2 in
   List.length lst1 = List.length uniq1
   &&
   List.length lst2 = List.length uniq2
   &&
   uniq1 = uniq2


   module String_ElementSig = struct
   type t = string

   let compare x y =
    match Stdlib.compare x y with
    | x when x < 0 -> LT
    | 0 -> EQ
    | _ -> GT
   let format fmt z =
    Format.fprintf fmt "%s" z
   end 




   module TestEngine = functor
   (S:DictionarySet.Set with type Elt.t = string)
   -> functor (D:Dictionary.Dictionary with type Key.t = string
                                       and type Value.t = S.t) 
    -> struct
      module Engines = Engine.Make(S)(D)

      let make_words_test (name:string) (idx:Engines.idx) (predicted: string list)= 
        name >:: (fun _ ->
            assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) 
              (Engines.words idx )(predicted));
    end

   module TestDictSet = functor (E : DictionarySet.ElementSig) -> functor (DM : DictionaryMaker) ->
   struct
   module Dictionary_Set= DictionarySet.Make(E)(DM)
   end


   module TestDict = functor (K : KeySig) -> functor (V : ValueSig) ->
   struct
   module Dict = ListDictionary.Make(K)(V)

   let make_empty_test (name : string) (dict : Dict.t) (predicted : bool) = 
    name >:: (fun _ -> 
        assert_equal predicted (Dict.is_empty dict) ~printer:string_of_bool)

   let make_size_test (name : string) (dict : Dict.t) (predicted : int) = 
    name >:: (fun _ -> 
        assert_equal predicted (Dict.size dict) ~printer:string_of_int)

   let make_member_test (name : string) (k : Dict.key) (dict : Dict.t) 
      (predicted : bool) = name >:: (fun _ -> 
      assert_equal predicted (Dict.member k dict) ~printer:string_of_bool)

   let make_find_test (name : string) (k : Dict.key) (dict : Dict.t) 
      (predicted : Dict.value option) =  name >:: (fun _ -> 
      assert_equal predicted (Dict.find k dict))

   let make_choose_test (name : string) (dict : Dict.t) 
      (predicted : (Dict.key * Dict.value) option) =  name >:: (fun _ -> 
      assert_equal predicted (Dict.choose dict))

   let make_fold_test name f (init:'acc) (dict : Dict.t) (predicted : 'acc) =
    name >:: (fun _ -> assert_equal predicted (Dict.fold f init dict))

   let make_tolist_test name dict (predicted : (Dict.key * Dict.value) list) =
    name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists predicted (Dict.to_list dict))

   end


   module Dict_throw = ListDictionary.Make(Int)(Int)

   module Value_t_set = DictionarySet.Make(String_ElementSig)(ListDictionary.Make)

   module TestDictInt = TestDict(Int)(Int)


   (** Set_Valuesig is used to create a Value.t with type S.t to pass into
    the dictionary*)
   module Set_Valuesig = 
   struct

   type t = DictionarySet.Make()

   let format fmt z =
    DictionarySet.format fmt z

   end


   module TestSet = TestDictSet(String_ElementSig)(ListDictionary.Make)
   (** TestSet is used as first input of Engine*)
   module TestDict_Str_Set = TestDict(String)(Set_Valuesig)

   open TestDictInt;;

   let int_dict_1 = Dict.insert 1 1 Dict.empty
   let int_dict_1_2 = Dict.insert 2 2 int_dict_1
   let int_dict_1_1 = Dict.insert 1 1 int_dict_1
   let int_dict_1' = Dict.remove 2 int_dict_1_2

   module EngineSTR = TestEngine(TestSet)()

   let tests = [
   TestDictInt.make_empty_test "true for empty dict" Dict.empty true;
   TestDictInt.make_empty_test "false for non-empty dict" int_dict_1 false;

   TestDictInt.make_size_test "size of int_dict_1 is 1" int_dict_1 1;
   TestDictInt.make_size_test "size of int_dict_1_2 is 2" int_dict_1_2 2;

   TestDictInt.make_size_test 
    "inserting duplicate k,v pair does not increase size" 
    (Dict.insert 1 1 int_dict_1) 1;
   TestDictInt.make_size_test
    "inserting different value for key does not increase size" 
    (Dict.insert 1 2 int_dict_1) 1;

   TestDictInt.make_size_test "removing key decreases size" int_dict_1' 1;
   TestDictInt.make_empty_test "removing only key results in empty dict" 
    (Dict.remove 1 int_dict_1) true; 

   TestDictInt.make_member_test "inserting (k, v) results in it being a member 
   of d" 1 int_dict_1 true;
   TestDictInt.make_member_test "removing (k, v) results in it no longer being
   a member of d" 2 int_dict_1' false;

   TestDictInt.make_find_test "find 2 int_dict_1 is None" 2 int_dict_1 None;

   (*make choose test
    make fold test
    make to_list test *)












   ]

   let suite = "search test suite" >::: tests

   let _ = run_test_tt_main suite
*)

let copyall character_list : string  = 
  character_list

let paste (storage:string ) (current_list: string ) : string = 
  current_list^storage

let rec  fixed_two (n: int): int =
  if n =1 then 0 
  else if n = 2 then 2
  else if n mod 2 = 0 then fixed_two(n/2)+2
  else  fixed_two(n-1)+1