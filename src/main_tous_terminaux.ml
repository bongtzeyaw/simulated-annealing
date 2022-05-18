


	
	
let recuit term =  
	let debut = (Sys.time ()) in														
	let gates_array = GateGroup.createTable in														
	let avions_array = TypeAvion.createTable in
	let vols_array_init =  Analyse_vols.shape in
	(* let term = 'D' in *)
	let vols_array = ref (Control.filtrageTerminal vols_array_init term) in

	Control.preProcessingId !vols_array avions_array;
	Control.preProcessingGates !vols_array avions_array gates_array;
	Control.initAleatDecision !vols_array avions_array;

	let repartition_array = ref (Control.repartition !vols_array gates_array) in							
	let conflits_array =  ref (Control.tableau_conflits !vols_array gates_array !repartition_array) in	
	let t0 = Control.temp_initiale !vols_array gates_array !conflits_array !repartition_array in
	let kmax = 100000 in
		
	let s_best_vols = ref (Array.copy !vols_array) in
	let s_best_repartition = ref (Array.copy !repartition_array) in
	let s_best_conflits = ref (Array.copy !conflits_array) in
	
	let e =  ref (Control.find_min !conflits_array) in
	let e_best =  ref (Control.find_min !conflits_array) in
	let k =  ref 0 in
	let t =  ref t0 in
	let delta = ref 0 in
	
	while (!k<kmax) && (((Sys.time ()) -. debut) < 3.)
		do 
			(* print_float ((Sys.time ()) -. debut); *)
			(* print_string "\n"; *)
			let s_vols =  (Array.copy !vols_array) in
			let s_repartition =  (Array.copy !repartition_array) in
			let s_conflits =  (Array.copy !conflits_array) in			
			delta := (Control.deltaE s_vols gates_array s_conflits s_repartition );
			
			(* Printf.printf " k = %d : e = %d  delta = %d \n" !k !e !delta; *)
			if ( !delta > 0 ) || ((Random.float 1.) < (exp ((abs_float (float_of_int !delta)) /. !t)))  (*l'exponentielle est positive car on cherche à maximiser la fonction*)
				then
					begin
					vols_array :=  (Array.copy s_vols) ;
					repartition_array := (Array.copy s_repartition) ;
					conflits_array := (Array.copy s_conflits) ;
					e := (Control.find_min !conflits_array);

					if (!e > !e_best) 
						then 
							begin
							s_best_vols :=  (Array.copy !vols_array) ;
							s_best_repartition := (Array.copy !repartition_array) ;
							s_best_conflits := (Array.copy !conflits_array) ;
							e_best := (Control.find_min !conflits_array);
							end;
					end;
			k := (!k)+1;
			t := 0.99*.(!t);
				
		done;
	!e_best;;

			
(* print_int recuit ;; *)


let terminaux = [|'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z'|];;
for i=0 to (Array.length terminaux)-1 do 
	Printf.printf "Terminal %c : %d \n" terminaux.(i) (recuit terminaux.(i))
	done
	
(*-------------------------------------------------------------------------------------------------------------------------------------*)	
	



