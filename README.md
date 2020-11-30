# advent-of-code-2020
Rešitve Advent of Code 2020 v OCaml-u

## Nastavitev razvijalskega okolja

Repository uporabla VSCode Docker container za delovanje.

> Spodnja navodila so prekopirana iz učilnice.

Namesto da si Python in Ocaml namestite direktno na računalnik lahko uporabite docker kontainerje, ki poskrbijo, da so namestitve ločene. Slaba stran te namestitve je dejstvo, da (trenutno) porabi nekoliko več prostora na disku (prostor, ki ga potrebuje docker + 4.5 GB)

Pri namestitvi sledite navodilom iz https://code.visualstudio.com/docs/remote/containers, kamor se lahko obrnete, če pride do kakšnih napak.

Namestite si docker za vaš operacijski sistem: https://docs.docker.com/get-docker/ in ga nastavite. Za sistem Windows si morate dodatno namestiti tudi WSL (https://docs.microsoft.com/sl-si/windows/wsl/wsl2-kernel), na kar vas ob prvem zagonu opozori tudi docker.
V VSC si namestite razširitev Remote - Containers.
Uredite si git v remote kontainerju https://code.visualstudio.com/docs/remote/containers#_working-with-git.
Za uporabo klonirajte repozitorij predmeta in zaženite ukaz (ctrl + shift + P) Remote-Containers: Rebuild and Reopen in Container.

Prvi zagon traja nekaj časa, saj mora naložiti celotno sliko, vsi naslednji zagoni pa so hitri.

Ko se projekt odpre, lahko normalno uporabljate taske, ki so na voljo na repozitoriju predmeta.