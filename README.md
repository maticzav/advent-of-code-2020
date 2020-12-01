# advent-of-code-2020

Rešitve Advent of Code 2020 v OCaml-u

Pri reševanju uporabljam še knjižnico Base https://opensource.janestreet.com/base/.

- https://adambard.com/blog/getting-started-with-ocaml/

## Template

```ocaml
open Base
open Stdio

(* Nastavitve *)

let dan_naloge = "1"

(* Naloge *)

let naloga1 (vsebina : string list) : string =
  "42"

let naloga2 (vsebina : string list) : string =
  "92"

(* Pomožne funkcije *)

let preberi_datoteko (ime_datoteke : string) : string list =
  let vsebina = In_channel.read_lines ime_datoteke in
  vsebina

let izpisi_datoteko (ime_datoteke : string) (vsebina : string) =
  Out_channel.write_all ime_datoteke ~data:vsebina

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
```



## Nastavitev razvijalskega okolja

Repository uporabla VSCode Docker container za delovanje, zato mora Docker biti zagnan v ozadju vedno, ko urejamo kodo.

1. Namesti Docker na svoj računalnik. [link](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
2. Namesti VSCode extension [link](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
3. Za uporabo odpri projekt v VSCode in zaženite ukaz (ctrl + shift + P) Remote-Containers: Rebuild and Reopen in Container.

> Prvi zagon traja nekaj časa, saj mora naložiti celotno sliko, vsi naslednji zagoni pa so hitri.

## Pogosta vprašanja

Če kaj ni jasno naredi issue v tem repozitoriju.