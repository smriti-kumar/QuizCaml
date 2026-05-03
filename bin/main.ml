open Quizcaml.Flashcard_review
open Quizcaml.Matching_game_logic
open Quizcaml.Flashcards
open Quizcaml.Quiztest

(*General frontend*)

(** [clear ()] clears the terminal screen by moving the current output to the
    top of the user's screen for Unix operating systems or clears the screen
    entirely for any other operating system type. *)
let clear () =
  if Sys.os_type = "Unix" then ignore (Sys.command "clear -x")
  else ignore (Sys.command "cls")

(* matching game frontend *)

(*save scores to CSV*)
let save_matching_scores (username : string) : unit =
  begin
    (*file for each user*)
    let filename = "matching_scores/" ^ username ^ "matching_scores.csv" in
    let score_data : string list list =
      [ [ username; string_of_int !num_corr; string_of_int !num_inc ] ]
    in
    Csv.save filename score_data
  end

(*Find scores*)
let give_matching_score (filename : string) : unit =
  if Sys.file_exists filename = false then
    print_endline "User hasn't played yet"
  else begin
    let score_info : string list = List.nth (Csv.load filename) 0 in
    print_endline "\n Matching game scores \n";
    print_endline ("Username : " ^ List.nth score_info 0 ^ "\n");
    print_endline ("\nCorrect : " ^ List.nth score_info 1 ^ "\n");
    print_endline ("\nIncorrect : " ^ List.nth score_info 2 ^ "\n")
  end

(*Print word assn on LHS and def Assn on RHS*)
let print_match_choices () : unit =
  for i = 0 to Array.length !word_assn - 1 do
    Printf.printf "%d) %-20s %-4s %s) %s\n\n"
      (fst !word_assn.(i))
      (snd !word_assn.(i))
      " "
      (fst !def_assn.(i))
      (snd !def_assn.(i))
  done

(*Give feedback on guesses*)
let guess_feedback (guess : string) () : unit =
  begin
    let corr : bool = check_guess guess in
    if corr then print_endline "\nCorrect" else print_endline "\nIncorrect";
    print_endline
      ("\nYou have " ^ string_of_int !num_corr ^ " correct guesses\n");
    print_endline
      ("\nYou have " ^ string_of_int !num_inc ^ " incorrect guesses\n")
  end

(*Check if guess is made up of valid choices*)
let check_guess_exists (guess : string) : bool =
  begin
    let guess_info : string list = String.split_on_char ' ' guess in
    let word_num : int = int_of_string (List.nth guess_info 0) in
    let def_str : string = List.nth guess_info 1 in
    (*all number choices*)
    let all_nums : int list =
      Array.to_list (Array.map (fun (a, b) -> a) !word_assn)
    in
    (*al letter choices*)
    let all_lets : string list =
      Array.to_list (Array.map (fun (a, b) -> a) !def_assn)
    in
    List.mem word_num all_nums && List.mem def_str all_lets
  end

(*Loop through the game until all pairs are correctly matched*)
let rec round_loop (username : string) : unit =
  while Array.length !word_assn > 0 do
    print_match_choices ();
    print_endline "\n\nGuess: ";
    let guess : string = read_line () in
    (*If guess is inalid, make them redo it*)
    if check_guess_exists guess = false then
      print_endline "\nInvalid guess. Please try again.\n"
    else begin
      guess_feedback guess ();
      (*Give users 3 s to read feedback, before going to next round*)
      Unix.sleep 1;
      (*Clear the screen before the next round*)
      clear ();
      print_endline "\nTerms left to match: \n\n"
    end
  done;

  (*All pairs are matched*)
  (*Add score to csv*)
  save_matching_scores username;
  (*Success message*)
  print_endline "\n\nCongratulations on finishing all matches!\n";
  print_endline
    ("Final results: \n Correct matches: " ^ string_of_int !num_corr
   ^ "\n Incorrect matches: " ^ string_of_int !num_inc)

(*Startup matching*)
let start_matching (flashcards : (string * string) list) (username : string) :
    (string * string) list =
  begin
    print_endline
      "\n\n\
       Goal: Match every word on the left to a definition on the right\n\n\
       Guess fomat: Enter a number on the left, then a space, followed by a \
       letter on the right ";
    start_game_logic flashcards;
    round_loop username;
    flashcards
  end

(* flashcard review frontend *)

(** [print_dash n] prints a horizontal line of [n] dashes. *)
let rec print_dash (n : int) =
  if n == 1 then print_string "-"
  else (
    print_string "-";
    print_dash (n - 1))

(** [print_flashcard text] displays [text] inside a formatted flashcard box with
    fixed width and centered text. *)
let print_flashcard (text : string) =
  let width = 50 in
  let wrapped = wrap_string text (width - 6) in
  print_dash width;
  print_endline "";
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  List.iter
    (fun line ->
      print_string "| ";
      print_string (center_string line (width - 4));
      print_endline " |")
    wrapped;
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  print_dash width;
  print_endline ""

(** [show_term card] clears the screen and displays the term part of [card]. *)
let show_term (card : string * string) =
  clear ();
  print_endline "Term:";
  print_flashcard (fst card)

(** [show_definition card] clears the screen and displays the definition part of
    [card]. *)
let show_definition (card : string * string) =
  clear ();
  print_endline "Definition:";
  print_flashcard (snd card)

(** [print_stats stats] prints a summary of a review session, including the
    percentage of cards marked as known, an encouraging message based on
    performance, and detailed per-card results such as term, definition, whether
    the card was flipped, if it was marked as known, and how confident the user
    is. *)
let print_stats (stats : review_stats list) =
  let percent =
    List.fold_left
      (fun acc (_, _, known, _) -> if known then acc + 1 else acc)
      0 stats
    * 100 / List.length stats
  in
  if percent == 100 then print_string "Congrats! Perfect session. "
  else if percent >= 90 then print_string "Excellent work! "
  else if percent >= 80 then print_string "Great job! "
  else if percent >= 70 then print_string "Nice work! "
  else if percent >= 50 then print_string "You're getting there! "
  else print_string "Keep practicing! ";
  print_endline (string_of_int percent ^ "% of cards known.");
  List.iter
    (fun ((term, def), flipped, known, conf) ->
      let known_status = if known then "Known" else "Unknown" in
      let flipped_status = if flipped then "Flipped" else "Unflipped" in
      let confidence =
        match conf with
        | High -> "High"
        | Medium -> "Medium"
        | Low -> "Low"
      in
      print_endline
        ("Term: " ^ term ^ ", Definition: " ^ def ^ " - " ^ known_status ^ ", "
       ^ flipped_status ^ ", Confidence: " ^ confidence))
    stats

(** [print_progress_history filename] reads stored review history from
    [filename] and prints per-session statistics, including the percent of cards
    known in that session and the distribution of confidence levels (low,
    medium, high). If the file does not exist since the user has never played
    this set in the flashcard review mode, prints a message indicating that. *)
let print_progress_history (filename : string) =
  if not (Sys.file_exists filename) then
    print_endline "No previous sessions found."
  else
    let history = load_all filename in
    let group_hist = group_sessions history in
    print_endline "\nProgress History:\n";
    List.iter
      (fun (session, stats) ->
        let acc = session_known stats in
        let low, med, high = session_confidence stats in
        Printf.printf
          "Session %d: %.2f%% known | Low Confidence: %.2f%% | Medium \
           Confidence: %.2f%% | High Confidence: %.2f%%\n"
          session acc low med high)
      group_hist;
    print_endline ""

(** [review_card card] runs the interactive review flow for a single [card]. The
    user is allowed to skip the card or flip the card, and if the user flips the
    card, they can mark it as known/unknown and assign a confidence level to it.
    Returns the resulting [review_stats] for that card. *)
let review_card (card : string * string) =
  show_definition card;
  print_string "Press s to skip, press any other key to flip: ";
  flush stdout;
  let skip = String.lowercase_ascii (read_line ()) in
  print_endline "";
  if skip = "s" then (card, false, false, Low)
  else (
    show_term card;
    print_string "Did you know this? (y/n): ";
    flush stdout;
    let right = String.lowercase_ascii (read_line ()) in
    let correct = right = "y" in
    print_string "Rate your confidence (l for low/m for medium/h for high): ";
    let conf = String.lowercase_ascii (read_line ()) in
    print_endline "";
    let confidence =
      match conf with
      | "h" -> High
      | "m" -> Medium
      | _ -> Low
    in
    (card, true, correct, confidence))

(** [review_session cards name] runs a full flashcard review session. If
    previous session data exists for the set [name], then it plays the game
    using the optimized order of cards from the most recent session. Otherwise,
    it plays the provided [cards] in order. The function saves results to a CSV
    file under [flashcard_review_stats/], prints session statistics at the end,
    and returns the original list of cards as is. *)
let review_session (cards : (string * string) list) (name : string) =
  let filename = "flashcard_review_stats/" ^ name ^ "_flashcard_stats.csv" in
  let file_exists = Sys.file_exists filename in
  let rec review_session_helper (acc : review_stats list) cards =
    match cards with
    | [] ->
        print_stats (List.rev acc);
        let oc =
          open_out_gen [ Open_creat; Open_text; Open_append ] 0o666 filename
        in
        if file_exists then
          output_string oc
            (stats_to_csv (List.rev acc) (read_last_row filename + 1) ^ "\n")
        else
          output_string oc
            ("session,term,definition,flipped,known,confidence" ^ "\n"
            ^ stats_to_csv (List.rev acc) 1
            ^ "\n");
        close_out oc
    | h :: t -> review_session_helper (review_card h :: acc) t
  in
  if file_exists then
    review_session_helper [] (optimal_order (load_last filename))
  else review_session_helper [] cards;
  cards

(* test generation frontend *)

(*In order to play the test activity the Mula library must be installed.*)
let rec valid_count (size : int) : int =
  let input = read_line () in
  if
    String.for_all
      (fun x ->
        List.exists
          (fun y -> x = y)
          [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ])
      input
    = false
  then (
    print_endline
      ("Please enter a valid integer from 1 to " ^ string_of_int size ^ "!");
    valid_count size)
  else if int_of_string input > 0 && int_of_string input <= size then
    int_of_string input
  else (
    print_endline
      ("Please enter a valid integer from 1 to " ^ string_of_int size ^ "!");
    valid_count size)

let test_question (td : string * string) (num : int) (mode : int) : string list
    =
  if mode = 1 then
    match td with
    | i, j ->
        let () =
          print_endline "";
          print_endline ("Definition " ^ string_of_int num ^ ": " ^ j);
          print_endline "";
          print_endline "Enter the term: "
        in
        let guess = read_line () in
        [ guess; i; correctness guess i; "DEFINITION"; j ]
  else
    match td with
    | i, j ->
        let () =
          print_endline "";
          print_endline ("Term " ^ string_of_int num ^ ": " ^ i);
          print_endline "";
          print_endline "Enter the definition: "
        in
        let guess = read_line () in
        [ guess; j; correctness guess j; "TERM"; i ]

let rec mcq_printer (choices : string list) (n : int) =
  if n >= 3 then print_endline ("4) " ^ List.nth choices 3)
  else
    let () =
      print_endline (string_of_int (n + 1) ^ ") " ^ List.nth choices n)
    in
    mcq_printer choices (n + 1)

let mcq_question (tdlist : (string * string) list) (question : int) (num : int)
    (mode : int) =
  if mode = 1 then
    let team = mcq_builder question tdlist mode in
    match team with
    | i, j ->
        let ans = List.nth j 0 in
        let ques = List.nth j 1 in
        let () =
          print_endline "";
          print_endline ("Definition " ^ string_of_int (num + 1) ^ ": " ^ ques);
          print_endline "";
          print_endline "Choose the correct term:"
        in
        let () = mcq_printer i 0 in
        let guess = List.nth i (valid_count 4 - 1) in
        [ guess; ans; correctness guess ans; "DEFINITION"; ques ]
  else
    let team = mcq_builder question tdlist mode in
    match team with
    | i, j ->
        let ans = List.nth j 0 in
        let ques = List.nth j 1 in
        let () =
          print_endline "";
          print_endline ("Term " ^ string_of_int (num + 1) ^ ": " ^ ques);
          print_endline "";
          print_endline "Choose the correct definition:"
        in
        let () = mcq_printer i 0 in
        let guess = List.nth i (valid_count 4 - 1) in
        [ guess; ans; correctness guess ans; "TERM"; ques ]

let rec test_activity_loop (tdlist : (string * string) list) (rlist : int list)
    (acc : 'a list) (num : int) (count : int) (mode : int) (gtype : int) :
    string list list =
  let eureka = [ mode; mode; ran_num count ] in
  if num = count then acc
  else if gtype = 2 then
    let question = List.nth rlist num in
    let gar = mcq_question tdlist question num (List.nth eureka (mode - 1)) in
    test_activity_loop tdlist rlist (gar :: acc) (num + 1) count mode gtype
  else
    let question = List.nth tdlist (List.nth rlist num) in
    let gar = test_question question (num + 1) (List.nth eureka (mode - 1)) in
    test_activity_loop tdlist rlist (gar :: acc) (num + 1) count mode gtype

let rec results_loop (num : int) (count : int) (scantron : string list list) =
  if num >= count then print_endline "Good Effort!"
  else (
    print_endline "";
    print_endline
      (List.nth (List.nth scantron 3) num
      ^ " "
      ^ string_of_int (count - num)
      ^ ": "
      ^ List.nth (List.nth scantron 4) num);
    print_endline
      ("YOUR ANSWER: "
      ^ List.nth (List.nth scantron 0) num
      ^ ", CORRECT ANSWER: "
      ^ List.nth (List.nth scantron 1) num
      ^ ", "
      ^ speak (List.nth (List.nth scantron 2) num));
    print_endline "";
    results_loop (num + 1) count scantron)

let mcq_validation (length : int) : int =
  if length < 4 then (
    print_endline "";
    print_endline
      "Since there are less than 4 term/definition pairs in your set, you \n\
       can only have Type in the Answer questions. In order to choose \n\
       between Multiple Choice and Type in the Answer questions, please \n\
       have 4 or more pairs in your set.";
    1)
  else
    let () =
      print_endline "";
      print_endline "Pick a question type:";
      print_endline "1) Type in the Answers";
      print_endline "2) Multiple Choice"
    in
    valid_count 2

let test_activity (tdlist : (string * string) list) =
  let () =
    print_endline "";
    print_endline
      "Welcome to the test activity! In this activity, you will be\n\
       given a test consisting of random terms/definitions from the set. To \
       get a\n\
       question right, you must input what correctly corresponds to the given\n\
       term/definition. Once all questions have been answered, you will be able\n\
       to see your grade, as well as what questions you got right and wrong."
  in
  let gtype = mcq_validation (List.length tdlist) in
  let () =
    print_endline "";
    print_endline "Pick a game mode:";
    print_endline "1) Given the definitions, have to respond with terms";
    print_endline "2) Given the terms, have to respond with definitions";
    print_endline
      "3) Given both terms and definitions, have to respond with both"
  in
  let mode = valid_count 3 in
  let () =
    print_endline "";
    print_endline
      ("Enter how many questions (from 1 to "
      ^ string_of_int (List.length tdlist)
      ^ ") the test should have to begin: ")
  in
  let count = valid_count (List.length tdlist) in
  let rlist = ran_list [] (List.length tdlist) count in
  let scantron =
    BatList.transpose (test_activity_loop tdlist rlist [] 0 count mode gtype)
  in
  let grade = grader (List.nth scantron 2) count in
  let () = results_loop 0 count scantron in
  let () =
    print_endline "";
    print_endline
      ("RESULTS: " ^ List.nth grade 0 ^ "/" ^ string_of_int count ^ ", "
     ^ List.nth grade 1 ^ "%")
  in
  print_endline "";
  tdlist

(* flashcards frontend *)

let add_card (curr : (string * string) list) : (string * string) list =
  print_string "Please enter the term for the card you want to add: ";
  let term = read_line () in
  print_string "Please enter the definition for the card you want to add: ";
  let def = read_line () in
  add_card_from_input curr term def

(* As of now, this removes all cards in the list with that term. Can be changed
   depending on how we want to handle duplicates.*)
let remove_card (curr : (string * string) list) : (string * string) list =
  print_string "Please enter the term of the card you want to remove ";
  let rem_term = String.trim (read_line ()) in
  remove_card_from_input curr rem_term

let upload_cards () : (string * string) list option =
  print_endline
    "Please upload a two column CSV file with the entries in the first column \
     representing the terms and the corresponding entries in the second column \
     representing that definitions. Enter the path to the file below: ";
  try
    let filename = read_line () in
    match read_cards filename with
    | Some c -> Some c
    | None ->
        print_endline
          "\n\
           The provided CSV file has one or more rows that don't contain \
           exactly 2 entries. Please fix the file!\n";
        None
  with
  | Sys_error e ->
      (* maybe redo question instead of error*)
      print_endline
        "\n\
         The file could not be found or was not accessible. Please check the \
         path of the file!\n";
      None
  | Csv.Failure (_, _, _) ->
      print_endline
        "\n\
         The data in the file doesn't correspond to the CSV format so please \
         double check the formatting of this file!\n";
      None

let rec starter () : (string * string) list =
  print_endline
    "Please choose one of the following options and type your choice below:";
  print_endline "(1) I have an existing CSV file I would like to upload";
  print_endline
    "(2) I don't have a starter set but would like to manually add cards";
  let choice = ref None in
  let take_input () =
    try
      print_string "Your choice: ";
      flush stdout;
      let input = read_int () in
      if input = 1 || input = 2 then choice := Some input
      else print_endline "\nThat is not a valid choice. Please try again\n"
    with Failure _ ->
      print_endline "\nThat is not a valid choice. Please try again\n"
  in
  while !choice = None do
    take_input ()
  done;

  if !choice = Some 1 then
    match upload_cards () with
    | None -> starter ()
    | Some x -> x
  else add_card []

let run () =
  print_endline "\nWelcome to QuizCaml!\n";
  let caml_cards = ref (starter ()) in
  while true do
    print_endline
      "\n\
       Please choose one of the following games/actions and enter your choice \
       below: ";
    print_endline "(1) Add a card";
    print_endline "(2) Remove a card";
    print_endline "(3) Matching";
    print_endline "(4) Testing";
    print_endline "(5) Flashcard Review";
    print_endline "(6) Quit";
    let choice = ref None in
    let take_input () =
      try
        print_string "Your choice: ";
        flush stdout;
        let input = read_int () in
        if input = 6 then exit 0
        else if input = 1 || input = 2 || input = 3 || input = 4 || input = 5
        then choice := Some input
        else print_endline "\nThat is not a valid choice. Please try again\n"
      with Failure _ ->
        print_endline "\nThat is not a valid choice. Please try again\n"
    in
    while !choice = None do
      take_input ()
    done;
    if !choice = Some 1 then caml_cards := add_card !caml_cards
    else if !choice = Some 2 then caml_cards := remove_card !caml_cards
    else if !choice = Some 3 then
      caml_cards :=
        if List.length !caml_cards < 10 then begin
          print_endline
            ("\n\n\
              Must have at least 10 flashcards to do matching game. \n\
              Add at least "
            ^ string_of_int (10 - List.length !caml_cards)
            ^ " additional cards to play");
          !caml_cards
        end
        else begin
          (*Check if wants to play or see score*)
          print_endline
            "\n\
             Do you want to see previous matching scores, or start a new game?\n";
          print_endline "(1) New Game";
          print_endline "(2) See Scores";
          print_endline "Your choice: ";
          let activity : string = read_line () in
          if activity = "1" then (*new game*)
          begin
            (*Get username*)
            print_endline "\nEnter your player name\n";
            let username : string = read_line () in
            start_matching !caml_cards username
          end
          else (*see scores*)
          begin
            print_endline "\nEnter player's username\n";
            let username : string = read_line () in
            let fname : string =
              "matching_scores/" ^ username ^ "matching_scores.csv"
            in
            give_matching_score fname;
            !caml_cards
          end
        end
    else if !choice = Some 4 then caml_cards := test_activity !caml_cards
    else if !choice = Some 5 then (
      print_endline "Name the set you would like to review: ";
      let input_name = read_line () in
      let filename =
        "flashcard_review_stats/" ^ input_name ^ "_flashcard_stats.csv"
      in
      print_endline "\n(1) Start review session\n(2) View progress history\n";
      print_string "Your choice: ";
      let subchoice = read_int () in
      if subchoice = 1 then caml_cards := review_session !caml_cards input_name
      else print_progress_history filename)
  done

(* main driver *)

let () = run ()
