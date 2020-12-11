open Base
open Stdio

(* Nastavitve *)

let dan_naloge = "10"

(* Ime  *)

let ( >> ) f b a = f a |> b

let numbers (strings : string list) : int list =
  List.map ~f:Int.of_string strings

let count_key ~key map =
  let mark = function None -> 1 | Some v -> v + 1 in
  Map.update map key ~f:mark

(* Counts the differences between elements. *)
let jolt_diffs (ns : int list) =
  (* Data *)
  let init = Map.empty (module Int) in
  let sorted_ns = List.sort ~compare:Int.compare ns in
  (* Calculation *)
  let rec aux acc = function
    | Some prev, x :: xs ->
        (* Calculate the difference. *)
        let diff = x - prev in
        (* Mark the difference update. *)
        let new_acc = count_key ~key:diff acc in
        aux new_acc (Some x, xs)
    | None, x :: xs -> aux acc (Some x, xs)
    | _ -> acc
  in
  aux init (None, sorted_ns)

let jolt_rating (ns : int list) =
  match List.max_elt ~compare:Int.compare ns with
  | Some n -> n + 3
  | None -> failwith "Empty list!"

(* Naloga 1 *)

let naloga1 (vsebina : string list) =
  (* Data *)
  let adapters = numbers vsebina in
  let rating = jolt_rating adapters in
  (* Calculation *)
  let diffs = jolt_diffs (0 :: rating :: adapters) in
  Map.find_exn diffs 3 * Map.find_exn diffs 1

(* Naloga 2 *)

let naloga2 (vsebina : string list) =
  (* Data *)
  let input = numbers vsebina in
  let rating = jolt_rating input in
  let adapters = List.sort ~compare:Int.compare (0 :: rating :: input) in
  (* Helper function *)
  let has_adapt n = List.mem ~equal:Int.equal adapters n in
  (* Calculation using tribonacci. *)
  let paths = Array.create ~len:(rating + 1) 0 in
  (* We traverse through the values and count all possible continuations. *)
  (*
     One line surely starts from 0J, one starts from 1J if we have a 1J adapter.
     Other paths are sums of all the paths we could come to that number from previus
     three numbers.
  *)
  let tribonacci = function
    | 0 -> paths.(0) <- 1
    | 1 -> paths.(1) <- (if has_adapt 1 then 1 else 0)
    | 2 -> paths.(2) <- paths.(0) + paths.(1)
    | i -> paths.(i) <- paths.(i - 3) + paths.(i - 2) + paths.(i - 1)
  in
  List.iter ~f:tribonacci adapters;
  (* Return the last one. *)
  paths.(rating)

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
  (naloga1 vsebina_datoteke, naloga2 vsebina_datoteke)
