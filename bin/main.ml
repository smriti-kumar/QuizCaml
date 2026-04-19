open Quizcaml.Flashcard_review

(* example flashcard set for testing purposes only *)
let example = [ ("2", "1 + 1"); ("4", "2 + 2"); ("6", "3 + 3") ]
let result = review_session example "additions"