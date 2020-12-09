open Base
open Stdio

(* Nastavitve *)

let dan_naloge = "9"

(* Ime  *)

let ( >> ) f b a = f a |> b

let numbers (strings : string list) : int list =
  List.map ~f:Int.of_string strings

let sums (ns : int array) =
  Array.cartesian_product ns ns |> Array.map ~f:(fun (a, b) -> a + b)

let member ~sums n = Array.mem sums n ~equal:( = )

(* Naloga 1 *)

let naloga1 (vsebina : string list) =
  (* Data *)
  let ns = numbers vsebina |> List.to_array in
  let step = 25 in
  let rec aux (n : int) =
    let preabmle = Array.sub ~pos:(n - step) ~len:step ns in
    let possible_sums = sums preabmle in
    let item = ns.(n) in
    if member ~sums:possible_sums item then aux (n + 1) else item
  in
  aux step

(* Naloga 2 *)

let sum_below ~(list : int array) (n : int) =
  Array.fold_until ~init:(0, 0)
    ~f:(fun (acc, i) x ->
      let new_acc = acc + x in
      (* Different cases. *)
      if new_acc = n then
        let set = Array.sub ~pos:0 ~len:(i + 1) list in
        Stop (Some set)
      else if new_acc < n then Continue (new_acc, i + 1)
      else Stop None)
    ~finish:(fun _ -> None)
    list

let naloga2 (vsebina : string list) =
  (* Data *)
  let sum = naloga1 vsebina in
  let ns = numbers vsebina |> List.to_array in
  (* Helper functions *)
  let max = Array.max_elt ~compare:Int.compare in
  let min = Array.min_elt ~compare:Int.compare in
  (* Calc *)
  let rec aux n =
    print_endline (Int.to_string n);
    let remaining = Array.length ns - n in
    let sublist = Array.sub ~pos:n ~len:remaining ns in
    match sum_below ~list:sublist sum with
    (* Continue searching. *)
    | None -> aux (n + 1)
    (* Add up the beginning item and the last item. *)
    | Some x -> (
        match (max x, min x) with
        | Some a, Some b -> a + b
        | _ -> failwith "..." )
  in
  aux 0

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
