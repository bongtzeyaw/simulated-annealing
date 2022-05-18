


	
	
let recuit  =  
															
	let gates_array = GateGroup.createTable in														
	let avions_array = TypeAvion.createTable in
	let vols_array_init =  Analyse_vols.shape in
	let term = 'B' in
	let vols_array = ref Control.filtrageTerminal vols_array_init term in

	Control.preProcessingId vols_array avions_array;
	Control.preProcessingGates vols_array avions_array gates_array;
	Control.initAleatDecision vols_array avions_array;

	let repartition_array = Control.repartition vols_array gates_array in							
	let conflits_array =  Control.tableau_conflits vols_array gates_array repartition_array in	
	let t0 = Control.temp_initiale vols_array gates_array conflits_array repartition_array in
	let kmax = 10000 in
	
	(* let s = ref [|(Array.copy vols_array) ; (Array.copy repartition_array); (Array.copy conflits_array)|] in *)
	let s_vols = ref (Array.copy vols_array) in
	let s_repartition = ref (Array.copy repartition_array) in
	let s_conflits = ref (Array.copy conflits_array) in
	
	(* let s_best =  ref [|(Array.copy vols_array); (Array.copy repartition_array); (Array.copy conflits_array)|] in *)
	let s_best_vols = ref (Array.copy vols_array) in
	let s_best_repartition = ref (Array.copy repartition_array) in
	let s_best_conflits = ref (Array.copy conflits_array) in
	
	let e =  ref (Control.find_min conflits_array) in
	let e_best =  ref (Control.find_min conflits_array) in
	let k =  ref 0 in
	let t =  ref t0 in
	let delta = ref 0 in
	
	while (!k<kmax) 
		do 
			delta := (Control.deltaE vols_array gates_array conflits_array repartition_array );
			if ( !delta > 0 ) || ((Random.float 1.) < (exp ((float_of_int !delta) /. !t)))  (*l'exponentielle est positive car on cherche à maximiser la fonction*)
				then
					begin
					(* s := [|(Array.copy vols_array); (Array.copy repartition_array); (Array.copy conflits_array)|] *)
					s_vols :=  (Array.copy vols_array) ;
					s_repartition := (Array.copy repartition_array) ;
					s_conflits := (Array.copy conflits_array) ;
					e := (Control.find_min conflits_array);
					if (!e > !e_best) 
						then 
							begin
							(* s_best := [|(Array.copy vols_array); (Array.copy repartition_array); (Array.copy conflits_array)|]  *)
							s_best_vols :=  (Array.copy vols_array) ;
							s_best_repartition := (Array.copy repartition_array) ;
							s_best_conflits := (Array.copy conflits_array) ;
							e_best := (Control.find_min conflits_array);
							end
					end
			
			else 
				begin
				vols_array := (Array.copy !s_vols); 
				repartition := (Array.copy !s_repartition);
				conflits_array := (Array.copy !s_conflits);
				end
			k := k+1;
			t := 0.99*t;
				
		done;
	e_best;;
	5;;
			
(recuit );;
	
(*-------------------------------------------------------------------------------------------------------------------------------------*)	
	



