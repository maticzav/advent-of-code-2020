open Base
open Stdio

(* Nastavitve *)

let dan_naloge = "7"

(* Naloge *)

(* Torba je lahko ali prazna ali pa ima podtorbe. *)
type bag = Empty of string | Contents of string * bag list

(* Število torb in torba, ki bo lahko v neki torbi. *)
type subbag = int * string

type rule = string * subbag list

let string_to_bag (str : string) : rule =
  (* Patterns *)
  let stri = Re.(rep any) |> Tyre.regex in
  let bag =
    (* 1 mirrored yellow bag, *)
    Tyre.(
      str " " *> (int <* str " " <&> stri)
      <* (str " bag" <* opt (char 's'))
      <* (char ',' <|> char '.'))
  in
  let regex =
    Tyre.(start *> stri <* str " bags contain" <&> list (non_greedy bag))
  in
  (* Calculation *)
  let tyre = Tyre.compile regex in
  match Tyre.exec tyre str with
  | Ok rule -> rule
  | Error _ -> failwith "invalid pattern"

(* Ime  *)

let rules_from_list (vsebina : string list) =
  let init = Map.empty (module String) in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> (
        let name, bags = string_to_bag x in
        match Map.add acc ~key:name ~data:bags with
        | `Duplicate -> failwith "duplicate"
        | `Ok data -> aux data xs )
  in
  aux init vsebina

let ( >> ) f b a = f a |> b

let set_add_list ~set list =
  let rec aux acc = function [] -> acc | x :: xs -> aux (Set.add acc x) xs in
  aux set list

let set_of_list list =
  let empty = Set.empty (module String) in
  let rec aux acc = function [] -> acc | x :: xs -> aux (Set.add acc x) xs in
  aux empty list

(* Vrne imena vseh torb, ki so lahko v dani torbi. *)
let rec may_contain ~bags (bag : string) : string list =
  (* Helper function *)
  let name (_, name) = name in
  (* Traversal *)
  match Map.find bags bag with
  | Some [] -> []
  | Some values ->
      (* Dodaj vse vrednosti, ki jih imamo in poglej katere vse torbe lahko vsebujejo te torbe. *)
      let subbags = List.concat_map ~f:(name >> may_contain ~bags) values in
      let names = List.map ~f:name values in
      names @ subbags
  | None -> []

(* Naloga 1 *)

let naloga1 (vsebina : string list) =
  (* Data *)
  let my_bag = "shiny gold" in
  (* Helper functions *)
  let contains ~el list = List.mem ~equal:String.equal list el in
  (* Calculations *)
  let rules = rules_from_list vsebina in
  let bags = Map.keys rules |> List.map ~f:(may_contain ~bags:rules) in
  List.filter ~f:(contains ~el:my_bag) bags |> List.length

(* Naloga 2 *)

let sum = List.fold ~f:( + ) ~init:0

let rec bags_inside ~bags (bag : string) : int =
  (* Helper function *)
  let count (n, _) = n in
  (* Traversal *)
  let bags_in_bag (n, name) = n * bags_inside ~bags name in
  match Map.find bags bag with
  | Some [] -> 0
  | Some values ->
      (* Dodaj vse vrednosti, ki jih imamo in poglej katere vse torbe lahko vsebujejo te torbe. *)
      let subbags = List.map ~f:bags_in_bag values in
      let bags = sum (List.map ~f:count values) in
      bags + sum subbags
  | None -> 0

let naloga2 (vsebina : string list) =
  (* Data *)
  let my_bag = "shiny gold" in
  (* Calculations *)
  let bags = rules_from_list vsebina in
  bags_inside ~bags my_bag

(* Pomožne funkcije *)

let preberi_datoteko (dan : string) : string list =
  let ime_datoteke = "day_" ^ dan ^ ".in" in
  let vsebina = In_channel.read_lines ime_datoteke in
  vsebina

(* Main *)

(* Za lažjo uporabo v utop-u. *)
let vsebina_datoteke = preberi_datoteko dan_naloge

let _ =
  (* Izračunaj rešitev *)
  naloga1 vsebina_datoteke;
  naloga2 vsebina_datoteke
