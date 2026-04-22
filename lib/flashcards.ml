type card = string * string
type card_list = card list

(* Cards are added by trimming starting/ending whitespaces*)
let read_cards (filename : string) : card_list option =
  let data = Csv.load filename in
  let rec make_cards rows =
    match rows with
    | [] -> Some []
    | h :: t -> (
        match h with
        | [ term; def ] -> (
            match make_cards t with
            | Some c -> Some ((String.trim term, String.trim def) :: c)
            | None -> None)
        | _ ->
            (* maybe instead of failwith, redo the question (switch return type
               to option)*)
            print_endline
              "\n\
               The provided CSV file has one or more rows that don't contain \
               exactly 2 entries. Please fix the file!\n";
            None)
  in
  make_cards data

let add_card_from_input curr term def : card_list =
  (String.trim term, String.trim def) :: curr

let remove_card_from_input curr rem_term : card_list =
  let rec remove_term (lst : (string * string) list) acc =
    match lst with
    | [] -> acc
    | (term, def) :: t ->
        if term = rem_term then remove_term t acc
        else remove_term t ((term, def) :: acc)
  in
  List.rev (remove_term curr [])
