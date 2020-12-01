(* Nastavitve *)

let dan_naloge = "1"

(* Naloge *)

let naloga1 vsebina_datoteke = "10"

let naloga2 vsebina_datoteke = string_of_int (String.length vsebina_datoteke)

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
