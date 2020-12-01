open Base

(* Nastavitve *)

let dan_naloge = "1"

(* Naloge *)

let naloga1 (vsebina_datoteke : string) : string =
  (* Data *)
  let lines = String.split vsebina_datoteke '\n' in
  let numbers : int list = List.map lines Int.of_string in
  (* Helper functions *)
  let sum_and_product ((n1, n2) : int * int) : int * int = (n1 + n2, n1 * n2) in
  let is_2020 (tuple : int * int) : bool =
    match tuple with 2020, _ -> true | _ -> false
  in
  (* Calculation *)
  let result =
    List.cartesian_product numbers numbers
    |> List.map ~f:sum_and_product
    |> List.find ~f:is_2020
  in
  match result with
  | Some (_, prod) -> Int.to_string prod
  | _ -> "Something unexpected."

let naloga2 (vsebina_datoteke : string) : string =
  (* Data *)
  let lines = String.split vsebina_datoteke '\n' in
  let numbers : int list = List.map lines Int.of_string in
  (* Helper functions *)
  let sum_and_product (n1, (n2, n3)) = (n1 + n2 + n3, n1 * n2 * n3) in
  let is_2020 tuple = match tuple with 2020, _ -> true | _ -> false in
  (* Calculation *)
  let result =
    List.cartesian_product numbers numbers
    |> List.cartesian_product numbers
    |> List.map ~f:sum_and_product
    |> List.find ~f:is_2020
  in
  match result with
  | Some (sum, prod) -> Int.to_string prod ^ "-" ^ Int.to_string sum
  | _ -> "Something unexpected."

(* Pomožne funkcije *)

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let ime_vhodne_datoteke dan = "day_" ^ dan ^ ".in"

let ime_izhodne_datoteke dan naloga = "day_" ^ dan ^ "_" ^ naloga ^ ".out"

(* Main *)

let _ =
  (* Preberi podatke *)
  let vsebina_datoteke = preberi_datoteko (ime_vhodne_datoteke dan_naloge) in
  (* Izračunaj rešitev *)
  let odgovor1 = naloga1 vsebina_datoteke in
  let odgovor2 = naloga2 vsebina_datoteke in
  (* Izpiši podatke *)
  izpisi_datoteko (ime_izhodne_datoteke dan_naloge "1") odgovor1;
  izpisi_datoteko (ime_izhodne_datoteke dan_naloge "2") odgovor2
