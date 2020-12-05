open Base
open Stdio
open Tyre.Infix

(* Nastavitve *)

let dan_naloge = "5"

(* Naloge *)

let bin_to_int (one : char) bin_string =
  let bin_chars = String.to_list_rev bin_string in
  let rec aux acc e = function
    | [] -> acc
    | x :: xs when Char.equal x one -> aux (acc + (2 ** e)) (e + 1) xs
    | _ :: xs -> aux acc (e + 1) xs
  in
  aux 0 0 bin_chars

let split_at str index =
  let len = String.length str - index in
  let first = String.sub str ~pos:0 ~len:index in
  let second = String.sub str ~pos:index ~len in
  (first, second)

let seat code =
  let row_code, col_code = split_at code 7 in
  let row = bin_to_int 'B' row_code in
  let col = bin_to_int 'R' col_code in
  (row, col)

let seat_id (row, col) = (row * 8) + col

(* Naloga 1 *)

let naloga1 (vsebina : string list) =
  let bigger a b = if a < b then -1 else 1 in
  vsebina |> List.map ~f:seat |> List.map ~f:seat_id
  |> List.max_elt ~compare:bigger

(* Naloga 2 *)

let all_seats : (int * int) list =
  let rows = List.range 0 127 in
  let cols = List.range 0 7 in
  List.cartesian_product rows cols

let ( >> ) f b a = f a |> b

let includes xs ~el:x = List.mem ~equal:( = ) xs x

let naloga2 (vsebina : string list) =
  (* Data *)
  let occupied_seats = vsebina |> List.map ~f:(seat >> seat_id) in
  let all_seats = all_seats |> List.map ~f:seat_id in
  (* Helper functions *)
  let same_seat a b = seat_id a = seat_id b in
  (* Tells whether a nearby +- seat has been taken. *)
  let nearby_seat id =
    includes occupied_seats ~el:(id + 1) || includes occupied_seats ~el:(id - 1)
  in
  let is_occupied seat = includes occupied_seats ~el:seat in
  (* Calculations *)
  let unoccupied_seats = List.filter ~f:(is_occupied >> not) all_seats in
  let possible_seats = List.filter ~f:nearby_seat unoccupied_seats in
  possible_seats

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
