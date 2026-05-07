let read_cards (filename : string) : (string * string) list option =
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
        | _ -> None)
  in
  make_cards data

let add_card_from_input curr term def : (string * string) list =
  (String.trim term, String.trim def) :: curr

let remove_card_from_input curr rem_term : (string * string) list =
  let rec remove_term (lst : (string * string) list) acc =
    match lst with
    | [] -> acc
    | (term, def) :: t ->
        if term = rem_term then remove_term t acc
        else remove_term t ((term, def) :: acc)
  in
  List.rev (remove_term curr [])

let export_set_from_input
    (cards_list : (string * (string * string) list ref) list) (name : string) :
    string =
  let cards = !(List.assoc name cards_list) in
  if not (Sys.file_exists "output") then Sys.mkdir "output" 0o755;
  let filepath = "output/" ^ name ^ ".csv" in
  let out_channel = open_out filepath in
  let rec write_file (terms_list : (string * string) list) =
    match terms_list with
    | [] -> ()
    | (term, def) :: t ->
        output_string out_channel (term ^ "," ^ def ^ "\n");
        write_file t
  in
  write_file cards;
  close_out out_channel;
  filepath
