open Base
open Stdio
open Tyre.Infix

(* Nastavitve *)

let dan_naloge = "3"

(* Naloge *)

type korak = { drevesa : int; loc : int * int }

let drevesa_na_poti ((desno, dol) : int * int) ~(vsebina : string list) =
  let acc { drevesa; loc } vrstica =
    (* Izračunaj pozicijo na mapi, ki se ponavlja v desno. *)
    let vrstica = String.strip vrstica in
    let abs_x, abs_y = loc in
    let dolzina_vrstice = String.length vrstica in
    let x = abs_x % dolzina_vrstice in
    let y = abs_y % dol in
    (* Pogledamo če to vrstico upoštevamo glede na y-korak, drugače gremo samo eno dol. *)
    if y = 0 then
      (* Poglej kaj je na tem mestu in popravi število dreves ter lokacijo. *)
      {
        drevesa = (match vrstica.[x] with '#' -> drevesa + 1 | _ -> drevesa);
        loc = (abs_x + desno, abs_y + 1);
      }
    else (* Ne spremeni nič *)
      { drevesa; loc = (abs_x, abs_y + 1) }
  in
  (* Računanje *)
  let { drevesa } =
    List.fold ~init:{ drevesa = 0; loc = (0, 0) } ~f:acc vsebina
  in
  drevesa

(* Naloga 1 *)

let naloga1 (vsebina : string list) = drevesa_na_poti (3, 1) vsebina

(* Naloga 2 *)

let rec zmnozek = function [] -> 1 | x :: xs -> x * zmnozek xs

let naloga2 (vsebina : string list) =
  let poti =
    [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
    |> List.map ~f:(drevesa_na_poti ~vsebina)
  in
  zmnozek poti

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
