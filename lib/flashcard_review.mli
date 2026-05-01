type confidence
type flashcard_record
type review_stats

val string_to_words : string -> string list
val wrap_string : string -> int -> string list
val center_string : string -> int -> string
val optimal_order : review_stats list -> (string * string) list
val stat_to_csv : int -> review_stats -> string
val csv_to_stat : string -> review_stats
val get_session : string -> int
val stats_to_csv : review_stats list -> int -> string
val load_last : string -> review_stats list
val load_all : string -> (int * review_stats) list
val group_sessions : (int * review_stats) list -> (int * review_stats list) list
val session_known : review_stats list -> float
val session_confidence : review_stats list -> float * float * float
