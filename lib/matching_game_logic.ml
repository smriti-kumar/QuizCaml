(*Matching game backend*)
(*Randomly choose terms + defs *)
(*Give correct/inc*)

let sample_terms : (string * string) list =
  [
    ("Term1", "Def1");
    ("Term2", "Def2");
    ("Term3", "Def3");
    ("Term4", "Def4");
    ("Term5", "Def5");
    ("Term6", "Def6");
    ("Term7", "Def7");
    ("Term8", "Def8");
    ("Term9", "Def9");
    ("Term10", "Def10");
    ("Term11", "Def11");
    ("Term12", "Def12");
    ("Term13", "Def13");
    ("Term14", "Def14");
    ("Term15", "Def15");
  ]

(*Hold all chosen words used for the game*)
let game_arr : (string * string) array = Array.make 10 ("", "")

(*Hold words*)
let word_arr : string array = Array.make 10 ""

(*Hold all defs*)
let def_arr : string array = Array.make 10 ""

(*Hold number-word associations*)
let word_assn : (int * string) array = Array.make 10 (0, "")

(*Hold char-def associations*)
let def_assn : (string * string) array = Array.make 10 ("z", "")

(*Array.make ideal_length default_elem*)
(*Array.iter function array*)
(*Array.init function_taking_index array*)
(*Array.map2 function array1 array2*)

(*Randomly choose 10 word-def pairs*)

(*Associate each element in word_arr and def_arr with a character*)
let assign_print () : unit =
  begin
    (*assign words to a number from [1,10]*)
    let all_nums : int array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |] in
    for i = 0 to 9 do
      word_assn.(i) <- (all_nums.(i), word_arr.(i))
    done;

    (*assign words to a number from [1,10]*)
    let all_strs : string array =
      [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" |]
    in
    for i = 0 to 9 do
      def_assn.(i) <- (all_strs.(i), word_arr.(i))
    done
    (*Check assignments*)
    (* print_endline "Word assignments";
    print_endline
      (Array.fold_left
         (fun acc (num, word) -> acc ^ string_of_int num ^ ": " ^ word ^ "; ")
         "" word_assn);

    print_endline "Def assignments";
    print_endline
      (Array.fold_left
         (fun acc (str, def) -> acc ^ str ^ ": " ^ def ^ "; ")
         "" def_assn); *)
  end

let start_game_logic () : unit =
  begin
    let terms_arr : (string * string) array = Array.of_list sample_terms in
    (*Initialize random*)
    let () = Random.self_init () in
    Array.shuffle ~rand:Random.int terms_arr;
    (*From the shuffles array, choose 1st 10 pairs*)
    for i = 0 to 9 do
      let game_term : string * string = terms_arr.(i) in
      game_arr.(i) <- game_term
    done;

    (*Make word_arr*)
    for i = 0 to 9 do
      let word_term : string = fst terms_arr.(i) in
      word_arr.(i) <- word_term
    done;

    (*Make def_arr*)
    for i = 0 to 9 do
      let def_term : string = snd terms_arr.(i) in
      def_arr.(i) <- def_term
    done;
    assign_print ()
  end
