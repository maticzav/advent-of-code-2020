open Base
open Stdio
open Tyre.Infix

(* Nastavitve *)

let dan_naloge = "2"

(* Naloge *)

type policy = { min : int; max : int; letter : char; pwd : string }

let string_to_policy (input : string) : policy =
  (* Regex Pattern *)
  let string = Re.(rep (compl [ blank ])) |> Tyre.regex in
  let regex =
    Tyre.(
      int
      <&> str "-" *> int
      <&> blanks *> string
      <&> str ":" *> blanks *> string)
    |> Tyre.compile
  in
  match Tyre.exec regex input with
  | Ok (((min, max), letter), pwd) -> { min; max; letter = letter.[0]; pwd }
  | Error _ -> failwith "wrong pattern"

(* XOR *)
let ( /| ) a b = (a || b) && not (a && b)

(* Naloga 1 *)

let is_pwd_valid_old_job { min; max; letter; pwd } =
  let ponovitve = String.count ~f:(fun el -> Char.equal el letter) pwd in
  min <= ponovitve && max >= ponovitve

let naloga1 (vsebina : string list) =
  (* Calculations *)
  let result =
    vsebina
    |> List.map ~f:string_to_policy
    |> List.filter ~f:is_pwd_valid_old_job
    |> List.length
  in
  Int.to_string result |> print_string

(* Naloga 2 *)

let is_pwd_valid_new_job { min; max; letter; pwd } =
  Char.equal pwd.[min - 1] letter /| Char.equal pwd.[max - 1] letter

let naloga2 (vsebina : string list) =
  (* Calculations *)
  let result =
    vsebina
    |> List.map ~f:string_to_policy
    |> List.filter ~f:is_pwd_valid_new_job
    |> List.length
  in
  Int.to_string result |> print_string

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
