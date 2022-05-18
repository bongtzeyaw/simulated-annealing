type t = {
	mutable id : int;
	id_Arr : string;
	id_Dep : string;
	t_arr : int;	
	t_dep : int;
	terminal : char;
	mutable idGateGroup : int;
	mutable compatibleGateGroups : int array;
	aircraft_type : string;
	mutable aircraft_id : int;
	mutable conflit : int};;
	
let display_indiv vol_indiv = 
	print_string ((vol_indiv.id_Arr)^"-"^(vol_indiv.id_Dep));
	print_string (" || "^(Char.escaped (vol_indiv.terminal))^" ");
	print_int vol_indiv.idGateGroup;
	print_string "\n";;

(* prend en argument un tableau de vols et affiche à quelle porte chacun est assigné *)	
let display vols_array = 
	for i = 0 to 14 do
		display_indiv vols_array.(i);
	done;
	print_string "...\n\n";;