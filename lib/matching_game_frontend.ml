(*frontend code*)

open Matching_game_logic

(*Print word assn on LHS and def Assn on RHS*)
let print_match_choices () : unit =
  (*Shuffle order of defs*)
  Array.shuffle ~rand:Random.int def_assn;
  for i = 0 to 9 do
    Printf.printf "%d) %-20s %-4s %s) %s\n\n"
      (fst word_assn.(i))
      (snd word_assn.(i))
      " "
      (fst def_assn.(i))
      (snd def_assn.(i))
  done

(*Startup matching*)
let start_matching () : unit =
  begin
    print_endline "Match each word on the left to a definition on the right\n\n";
    start_game_logic ();
    print_match_choices ()
    (*Just make sure pairs were correctly selected*)
    (* Print all pairs
    print_endline
      (Array.fold_left
         (fun acc (term, def) -> acc ^ term ^ " = " ^ def ^ ";  ")
         "" game_arr) *)
    (*Print words*)
    (* print_endline "Words";
    print_endline
      (Array.fold_left (fun acc word -> acc ^ word ^ " ;  ") "" word_arr);
    (*Print defs*)
    print_endline "Defs";
    print_endline
      (Array.fold_left (fun acc def -> acc ^ def ^ " ;  ") "" def_arr); *)
  end
