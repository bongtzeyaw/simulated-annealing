let gates_ = "./data/gates.txt";;

type typeAvion = {aircraft_type:string; compatible_gate_group: int array};;

let empty_typeAvion = {aircraft_type=""; compatible_gate_group= Array.init 1 (fun i -> 999999999999)};;
let tab_typeAvion = ref (Array.init 1 (fun i -> ref empty_typeAvion)) ;;

let ouverture file =
 	(*Assurer la robustesse de l'ouverture fichier*)	
	try 
		open_in file;
	with exc ->
		Printf.printf "Echec ouverture de fichier entree %s\n" file;
		raise exc;;

let fermeture channel =
	(*Assurer la robustesse de la fermeture fichier*)		
	try 
		close_in channel;
	with exc ->
		Printf.printf "Echec fermeture de canal fichier";
		raise exc;;
		
let split chaine =
	(*Renvoyer liste de string à partir d'un grand string du format "string,string,..."*)
	let tab = ref [] in
	let i = ref 0 in
	let element = ref "" in
	let rec parcourt = fun() ->  
		if chaine.[!i] <> ',' then 
			begin
			element := String.concat "" [!element;Char.escaped chaine.[!i]] ;
			i := !i + 1 ;
			parcourt ()
			end
		else 
			begin
			tab := (!tab)@[!element] ;
			element := "" ;
			i := !i + 1;
			parcourt ()
			end in
	try parcourt () with exc -> begin tab := (!tab)@[!element]; !tab end;;

let extract_idgategroup gate tab_gategroup = 
	let is_in_gatelist gate gatelist =
	(*Renvoie un booléen indiquant si un gate est dans une liste de gate (c'est le champs gates de gateGroup)*)
	(*Ici gate est déjà du type char*string ('Z',"05") et gatelist est du type (char*string) list [('A',"01"); ('Z',"05")] *)
		let rec aux sgatelist =
			match sgatelist with 
				[] -> false
		  		|gatetete::queue -> if gatetete=gate then true else aux queue in
		aux gatelist in
	let idgategroup = ref 999999999999 in
	(*Si à la sortie on a 999999999999, le gate n'existe pas dans le tab_gategroup. Un ref négatif est plus sécurisé mais ocaml le permet pas*)
	Array.iter (fun ti -> if is_in_gatelist gate ti.GateGroup.gates then idgategroup:= ti.idGateGroup ) tab_gategroup;
	(*ti = objet du type gategroup = {idGateGroup = 0; gates = [('A', "01")]}*)
	!idgategroup
	

let lecture file tab_typeAvion tab_gategroup = 
	let canal = ouverture file in
	let rec encore () = 
		let l = input_line canal in 
		if l <> "" then
			begin
				let (gate, types) = Scanf.sscanf l "%c%2s:%s" (fun terminal numero_gate types -> ((terminal, numero_gate), types)) in
				let lst_types = split types in
				let add_aviongate = fun ref_tab gate typ -> 
				(*gate est du type char*string e.g. ('Z',"05") *)
					let typ_in_tab = ref false in
					Array.iter (fun ti -> if (!ti).aircraft_type = typ then begin typ_in_tab := true; ti := {aircraft_type=(!ti).aircraft_type; compatible_gate_group= Array.append (!ti).compatible_gate_group (Array.init 1 (fun i -> extract_idgategroup gate tab_gategroup))}  end) !ref_tab;
					if not !typ_in_tab then
						ref_tab := Array.append !ref_tab (Array.init 1 (fun i -> ref {aircraft_type=typ; compatible_gate_group= Array.init 1 (fun i -> extract_idgategroup gate tab_gategroup)})) in
				List.iter (add_aviongate tab_typeAvion gate) lst_types;
				encore ();
			end in
	try encore () with End_of_file -> () ;
	fermeture canal;;

lecture gates_ tab_typeAvion (GateGroup.createTable);;
(*tab_typeAvion est bien le tableau résultat qu'on veut mais le premier élément Empty_typeAvion persiste dans ce tableau. On l'enlève en faisant ceci. Au passage, on enlève ref du tab*)
(* let tab_typeAvion = Array.sub !tab_typeAvion 1 ((Array.length !tab_typeAvion) -1);; *)
(*Ensuite, on enlève les refs du typeAvion. Bizarrement, let tab_typeAvion = Array.iter (fun ti -> !(ti)) tab_typeAvion ne marche pas. On le contourne en faisant *)
(* let tab_typeAvion = Array.init (Array.length tab_typeAvion) (fun i -> (!(tab_typeAvion.(i))) );; *)
(* tab_typeAvion;; *)

(* ce matin j'ai mange des pates *)
let createTable = 
	let tab_typeAvion = Array.sub !tab_typeAvion 1 ((Array.length !tab_typeAvion) -1)
	in
	let tab_typeAvion = Array.init (Array.length tab_typeAvion) (fun i -> (!(tab_typeAvion.(i))) )
	in
	tab_typeAvion;;
	
let print_int_array a =
	let limite = 10 in
	print_string "[|";
	let n = (Array.length a) in
	if (n<limite) then begin
		for i=0 to ((n)-1) do
			print_int (a.(i));
			print_string " ; ";
		done;
			print_string "|]\n";
	end
	else begin
		for i=0 to ((limite)-1) do
			print_int (a.(i));
			print_string " ; ";
		done;
			print_string "...\n";
	end;;
		
			
let display_indiv avion = 
	print_string ((avion.aircraft_type)^" || ");
	print_int_array (avion.compatible_gate_group);;
	
let display tab = 
	for i = 0 to ((10)-1) do 
		display_indiv (tab.(i));
	done;
	print_string "...\n\n";;
	
(* renvoie l'indice de l'avion correspondant au vol passé en argument *)
let trouve_indice str avions_array = 
	let res = ref 0 in
	let n = Array.length avions_array in
	while (((!res)<n) && (str <> (avions_array.(!res).aircraft_type))) do incr res done;
	if ((!res) = n) then (-1) else (!res);;
	













