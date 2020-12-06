open Base
open Stdio

(* Nastavitve *)

let dan_naloge = "6"

(* Naloge *)

(* Prevede vsebino v seznam skupin. *)
let groups (vsebina : string) : string list =
  Re.(Str.split (Str.regexp "\n\n") vsebina)

let anyone (group : string) : char list =
  let chars = String.to_list group in
  let empty = Set.empty (module Char) in
  let rec aux acc = function
    | [] -> acc
    | '\n' :: xs -> aux acc xs (* Ignore new lines *)
    | x :: xs -> aux (Set.add acc x) xs
  in
  aux empty chars |> Set.to_list

let string_to_char_set str =
  let chars = String.to_list str in
  let empty = Set.empty (module Char) in
  let rec aux acc = function [] -> acc | x :: xs -> aux (Set.add acc x) xs in
  aux empty chars

let everyone (group : string) : char list =
  match String.split_lines group with
  | [] -> []
  | p :: ps ->
      let start = string_to_char_set p in
      let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (Set.inter acc (string_to_char_set x)) xs
      in
      aux start ps |> Set.to_list

let ( >> ) f b a = f a |> b

let sum (xs : int list) : int =
  let rec aux acc = function [] -> acc | x :: xs -> aux (acc + x) xs in
  aux 0 xs

(* Naloga 1 *)

let naloga1 (vsebina : string) =
  vsebina |> groups |> List.map ~f:(anyone >> List.length) |> sum

(* Naloga 2 *)

let naloga2 (vsebina : string) =
  vsebina |> groups |> List.map ~f:(everyone >> List.length) |> sum

(* Pomožne funkcije *)

let preberi_datoteko (dan : string) : string =
  let ime_datoteke = "day_" ^ dan ^ ".in" in
  let vsebina = In_channel.read_all ime_datoteke in
  vsebina

(* Main *)

(* Za lažjo uporabo v utop-u. *)
let vsebina_datoteke = preberi_datoteko dan_naloge

let _ =
  (* Izračunaj rešitev *)
  naloga1 vsebina_datoteke;
  naloga2 vsebina_datoteke
