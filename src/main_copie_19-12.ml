


(*----------------------  Initialisation -----------------------------------------------------------------------------------------------*)


(* PARSING *)
let vols_array_init = ref Analyse_vols.shape;; 														
let gates_array = GateGroup.createTable;;																
let avions_array = TypeAvion.createTable;;

let term = 'B';;
let vols_array = ref Control.filtrageTerminal vols_array_init term;;


(* CORPS PRINCIPAL *)
(* Initialisation *)
Control.preProcessingId vols_array avions_array;;
Control.preProcessingGates vols_array avions_array gates_array;;
Control.initAleatDecision vols_array avions_array;;

let repartition_array = ref Control.repartition vols_array gates_array ;;								
let conflits_array = ref Control.tableau_conflits vols_array gates_array repartition_array;;		
	
let t0 = Control.temp_initiale vols_array gates_array conflits_array repartition_array;;
let kmax = 10000;;

(*-------------------------------------------------------------------------------------------------------------------------------------*)
	
	
	
	
(*------------ CORPS DU PROGRAMME -----------------------------------------------------------------------------------------------------*)	
	
let recuit  !vols_array gates_array !repartition_array !conflits_array t0 =
	let s =  ref [|!vols_array; !repartition_array; !conflits_array|] in
	let s_best = ref [|!vols_array; !repartition_array; !conflits_array|] in
	let e = ref (Control.find_min !conflits_array) in
	let e_best = ref (Control.find_min !conflits_array) in
	let k = ref 0 in
	let t = ref t0 in
	
	while (k<kmax) 
		do 
			
			if ( (Control.find_min !conflits_array) > !e ) || ((Random.float 1)< (exp ( Control.deltaE !vols_array !gates_array !conflits_array !repartition_array) /. t)) (*l'exponentielle est positive car on cherche à maximiser la fonction*)
				then
					begin
					s := [|!vols_array; !repartition_array; !conflits_array|];
					e := (Control.find_min !conflits_array);
					if (!e > !e_max) 
						then s_best := !s (* CHECKER SI ON A BIEN MIS LES BONNES VARIABLES AUX BONS ENDROITS*)
					end
			else 
				begin
				vols_array := !s.(0); 
				repartition := !s.(1);
				conflits_array := !s.(2);
				end
			k := k+1;
			t := 0.99*t;
				
		done
	e_best;;
			
recuit !vols_array gates_array !repartition_array !conflits_array t0 ;;
	
(*-------------------------------------------------------------------------------------------------------------------------------------*)	
	
	
(*------------------ Trucs de DEBUG ---------------------------------------------------------------------------------------------------*)


let print_array tab = 									(* TESTE LE CONFLIT_ARRAY *)
	for i=0 to ((Array.length tab)-1) do 
		print_int tab.(i);
		print_string " - "
		done;;
(*print_array conflits_array;;*)
	
	
let print_gates vols_array  = 							(* TESTE LE VOLS_ARRAY *)
	for i=0 to ((Array.length vols_array)-1) do 
		print_int vols_array.(i).Vol.idGateGroup;
		print_string " - "
		done;;
(*print_gates vols_array;;*)


let print_idgates gates_array  = 						(* TESTE LE GATES_ARRAY *)
	for i=0 to ((Array.length gates_array)-1) do 
		print_int gates_array.(i).GateGroup.idGateGroup;
		print_string " - "
		done;;
		
(*print_idgates gates_array;;*)


let print_gate_rep liste = 
	let rec parcourt l = match l with
		|[] -> print_string "]\n"
		|(a,b,c)::q -> print_string "(";print_int a;
					   print_string ", ";print_int b;
					   print_string ", ";print_int c;
					   print_string "); ";parcourt q;
	in 
	print_string "[";
	parcourt liste;;
	
	
let test_repartition repartition = 
	for i=0 to ((Array.length repartition)-1) do 
		print_gate_rep repartition.(i);
		print_string "\n"
		done;;
		
(*test_repartition repartition_array;;*)
(*-------------------------------------------------------------------------------------------------------------------------------------*)



