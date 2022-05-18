
let treat_line l =
	let item = Scanf.bscanf (Scanf.Scanning.from_string l) "%s@-%s@: %d %d %d %d %s %s" (fun a b c d e f g h -> {
		Vol.id_Arr = a ; 
		id_Dep = b ;
		t_arr = c ;
		t_dep = d ; 	
		terminal = g.[0]; 
		idGateGroup = 0;
		compatibleGateGroups = [||];
		aircraft_type = h;
		aircraft_id = 0;
		conflit = 0;
		id = 0;} )
	in 
	item;;


let analyse filename = 
	let fic = open_in filename in
	let rec analyse_ligne buffer = 
		try 
			analyse_ligne ((treat_line  (input_line fic))::buffer)
		with End_of_file -> buffer in 
	analyse_ligne [] ;;


let shape filename = 
	let vol_init = treat_line "unk-FL1: 0 970 0 4 A01 A332" in
	let buffer = analyse filename in
	let n = List.length buffer in
	let tableau = Array.make n vol_init in 
	let rec make_tableau buffer tableau indice = match buffer with 
		|[]-> ()
		|h::q -> tableau.(indice)<- h;(make_tableau q tableau (indice-1))
	in
	make_tableau buffer tableau (n-1);
	tableau;;


