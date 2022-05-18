ocamlopt -c vol.ml
ocamlopt -c vol.cmx analyse_vols.ml
ocamlopt -c gateGroup.ml
ocamlopt -c gateGroup.cmx typeAvion.ml
ocamlopt -c vol.cmx gateGroup.cmx typeAvion.cmx control.ml


ocamlopt -o RS.exe vol.cmx gateGroup.cmx analyse_vols.cmx typeAvion.cmx control.cmx main.ml
RS.exe
PAUSE
 