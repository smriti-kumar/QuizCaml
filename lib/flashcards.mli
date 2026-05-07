(** [read_cards filename] reads the terms and definitions from the CSV file
    saved by [filename] and returns them as an association list. It reads these
    terms by ignoring starting and ending whitespaces. If the file is not a
    proper two column CSV, then None is returned. *)
val read_cards : string -> (string * string) list option

(** [add_card_from_input curr term def] adds the flashcard represented by
    ([term], [def]) to the existing list of flahscards, [curr]. The updated list
    of flashcards is returned. *)
val add_card_from_input :
  (string * string) list -> string -> string -> (string * string) list

(** [remove_card_from_input curr rem_term] removes all flashcards with term
    [rem_term] from the current list of flashcards [curr]. If no such terms
    exist, nothing is removed. The updated list of flashcards is returned. *)
val remove_card_from_input :
  (string * string) list -> string -> (string * string) list

(** [export_set_from_input sets_list set_name] exports the set with name
    [set_name] from the game as a CSV and provides the user the filepath of this
    exported file. Requires that there is a set in [sets_list] with the name
    [set_name]. Returns the filepath of the exported file. *)
val export_set_from_input :
  (string * (string * string) list ref) list -> string -> string
