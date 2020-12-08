open Base
open Stdio

(* Nastavitve *)

let dan_naloge = "8"

(* Naloge *)

type task = NoOP of int | Acc of int | Jmp of int

let string_to_task (str : string) : task =
  (* Patterns *)
  let name = Re.(rep (compl [ blank ])) |> Tyre.regex in
  let pattern =
    (* 1 mirrored yellow bag, *)
    Tyre.(name <&> str " " *> opt (str "+") *> int)
  in
  (* Calculation *)
  let tyre = Tyre.compile pattern in
  match Tyre.exec tyre str with
  | Ok ("nop", n) -> NoOP n
  | Ok ("jmp", n) -> Jmp n
  | Ok ("acc", n) -> Acc n
  | _ -> failwith "invalid pattern"

let tasks_from_list (vsebina : string list) : task array =
  List.map ~f:string_to_task vsebina |> List.to_array

(* let exec_task acc loop n = function
  | NoOP -> acc new_loop (n + 1)
  | Acc inc -> (acc + inc) new_loop (n + 1)
  | Jmp diff -> acc new_loop (n + diff) *)

let ( >> ) f b a = f a |> b

(* Naloga 1 *)

let naloga1 (vsebina : string list) =
  (* Data *)
  let acc_init = 0 in
  let loop_init = Set.empty (module Int) in
  let tasks = tasks_from_list vsebina in
  (* Calculation *)
  let rec aux acc loop n =
    (* Add current step to the set of visited steps. *)
    let new_loop = Set.add loop n in
    (* Check weather we've been here before. *)
    match (tasks.(n), Set.mem loop n) with
    | _, true -> acc
    | NoOP _, _ -> aux acc new_loop (n + 1)
    | Acc inc, _ -> aux (acc + inc) new_loop (n + 1)
    | Jmp diff, _ -> aux acc new_loop (n + diff)
  in
  aux acc_init loop_init 0

(* Naloga 2 *)

let arr_safe_get arr n = if n < Array.length arr then Some arr.(n) else None

let switch_on ~tasks corrupt_n =
  let acc_init = 0 in
  let loop_init = Set.empty (module Int) in
  (* Calculation *)
  let rec aux acc loop n =
    (* Add current step to the set of visited steps. *)
    let new_loop = Set.add loop n in
    (* Check weather we've been here before. *)
    match (arr_safe_get tasks n, Set.mem loop n) with
    (* Loop *)
    (* Switch to see if it fixes the problem *)
    | Some (NoOP diff), false when n = corrupt_n -> aux acc new_loop (n + diff)
    | Some (Jmp _), false when n = corrupt_n -> aux acc new_loop (n + 1)
    | Some (NoOP _), false -> aux acc new_loop (n + 1)
    | Some (Jmp diff), false -> aux acc new_loop (n + diff)
    (* This one shouldn't be recursed. *)
    | Some (Acc inc), false -> aux (acc + inc) new_loop (n + 1)
    | Some _, true -> None
    (* We've reached the last one, return the solution. *)
    | None, _ -> Some acc
  in
  aux acc_init loop_init 0

let is_jmp_or_nop = function Jmp _ | NoOP _ -> true | Acc _ -> false

let naloga2 (vsebina : string list) =
  (* Data *)
  let tasks = tasks_from_list vsebina in
  (* Calculation *)
  let rec aux n =
    if n = Array.length tasks then None
    else
      match switch_on ~tasks n with Some acc -> Some acc | None -> aux (n + 1)
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
