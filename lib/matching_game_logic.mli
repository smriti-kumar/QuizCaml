(**[sample_terms] stores all the (word, defintion) flashcard values*)
val sample_terms : (string * string) list ref

(**[set_flashcards_value flashcards_list] sets [sample_terms] to hold the value
   of [flashcards_list]*)
val set_flashcards_value : (string * string) list -> unit

(**[game_arr] stores the 10 flashcards from [sample_terms] randomly chosen to be
   used in the game*)
val game_arr : (string * string) array

(**[word_arr] stores only the words (aka terms) used for the game*)
val word_arr : string array

(**[def_arr] stores only the definitons used for the game*)
val def_arr : string array

(**[word_assn] stores the number associated with each term for matching
   purposes. An example is (1, Term1). The array is shortned as correct pairs
   are guessed.*)
val word_assn : (int * string) array ref

(**[def_assn] stores the string associated with each definiton for matching
   purposes. An example is ("c", Definition1). The array is shortned as correct
   pairs are guessed.*)
val def_assn : (string * string) array ref

(**[num_inc] stores the total number of incorrect guesses throught each game.
   With each incorrect guess, [num_inc] incrememnts by 1.*)
val num_inc : int ref

(**[num_corr] stores the total number of correct guesses throught each game.
   With each correct guess, [num_corr] incrememnts by 1.*)
val num_corr : int ref

(** [reset_game] resets the game before player begins*)
val reset_game : unit -> unit

(**[assign_print] associated each element in [word_arr] with a number and each
   element in [def_arr] with a string. Side-effect: updates elements in
   [word_assn] and [def_assn] to reflect these associations.*)
val assign_print : unit -> unit

(**[update_arrs word_num def_str] removes the correct word and definiton from
   [word_assn] and [def_assn], respectively*)
val update_arrs : int -> string -> unit

(**[check_guess guess] returns whether or not [guess] is a valid pairing based
   on the flashcard set in use*)
val check_guess : string -> bool

(**[start_game_logic flashcards_list] initializes the matching game based on the
   elements in [flashcards_list]*)
val start_game_logic : (string * string) list -> unit
