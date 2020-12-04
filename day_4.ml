open Base
open Stdio
open Re

(* Nastavitve *)

let dan_naloge = "4"

(* Naloge *)

(* Prevede vsebino v seznam passportov. *)
let passports (vsebina : string) : string list =
  Re.(Str.split (Str.regexp "\n\n") vsebina)

let rec every ~f = function [] -> true | x :: xs -> f x && every f xs

let contains str ~content =
  let re = Str.regexp_string str in
  let size = String.length content in
  let rec acc i =
    if Str.string_match re content i then true
    else if i = size then false
    else acc (i + 1)
  in
  acc 0

(* Naloga 1 *)

let naloga1 (vsebina : string) =
  let is_valid_passport pass =
    [ "byr:"; "iyr:"; "eyr:"; "hgt:"; "hcl:"; "ecl:"; "pid:" ]
    |> every ~f:(contains ~content:pass)
  in
  passports vsebina |> List.filter ~f:is_valid_passport |> List.length

(* Naloga 2 *)

(* Patterns *)
let num key = Tyre.(str key *> int) |> Tyre.compile

let str key =
  let re = Re.(rep alnum) |> Tyre.regex in
  Tyre.(str key *> re) |> Tyre.compile

let length key =
  Tyre.(str key *> int <&> (str "cm" <|> str "in") <&> blanks) |> Tyre.compile

(* Helper functions *)
let matchp pattern value =
  match Tyre.exec pattern value with Ok r -> Some r | Error _ -> None

let is_valid_passport pass =
  (* Calculations *)
  let byr = matchp (num "byr:") pass in
  let iyr = matchp (num "iyr:") pass in
  let eyr = matchp (num "eyr:") pass in
  let hgt = matchp (length "hgt:") pass in
  let hcl = matchp (str "hcl:#") pass in
  let ecl = matchp (str "ecl:") pass in
  let pid = matchp (str "pid:") pass in
  match (byr, iyr, eyr, hgt, hcl, ecl, pid) with
  | Some byr, Some iyr, Some eyr, Some hgt, Some hcl, Some ecl, Some pid ->
      (* Checks *)
      let byr_valid = 1920 <= byr && byr <= 2002 in
      let iyr_valid = 2010 <= iyr && iyr <= 2020 in
      let eyr_valid = 2020 <= eyr && eyr <= 2030 in
      let hgt_valid =
        match hgt with
        | (hgt, `Left _), _ -> 150 <= hgt && hgt <= 193
        | (hgt, `Right _), _ -> 59 <= hgt && hgt <= 76
      in
      let hcl_valid = String.length hcl = 6 in
      let ecl_valid =
        List.mem ~equal:String.equal
          [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
          ecl
      in
      let pid_valid = String.length pid = 9 in
      (* All *)
      byr_valid && iyr_valid && eyr_valid && hgt_valid && hcl_valid && ecl_valid
      && pid_valid
  | _ -> false

let naloga2 (vsebina : string) =
  passports vsebina |> List.filter ~f:is_valid_passport |> List.length

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
