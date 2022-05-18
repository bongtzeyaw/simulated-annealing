(* Fichier contenant les fonctions utiles à l'initialisation *)


(*------------------ Outils ------------------------------------------------------------------------------------------------------------*)

let filtrageTerminal vols_array term = 
	let n = Array.length vols_array
	in
	let res = Array.make n vols_array.(0)
	in
	let compteur = ref 0
	in
	for i=0 to (n-1) do
		if (vols_array.(i).Vol.terminal = term) then
			begin
			res.(!compteur) <- (vols_array.(i));
			incr compteur;
			end
	done;
	Array.sub res 0 (!compteur);;

		

(* trie une liste de couples par rapprt au temps d'arrivée *)
let rec trirap liste = 
	let rec partition l pivot gauche droite = match l with
		|[] -> gauche,droite
		|(id,arr,dep)::q when arr<pivot -> partition q pivot ((id,arr,dep)::gauche) droite
		|(id,arr,dep)::q -> partition q pivot gauche ((id,arr,dep)::droite)
	in
	match liste with
		|[] -> []
		|(id,arr,dep)::q -> let g,d = (partition q arr [] []) in (trirap g)@[(id,arr,dep)]@(trirap d);;
		

(*groupe les vols par gate *)
(* renvoie un tableau de listes de couples *)
let repartition vols_array gates_array = 
	let n = Array.length vols_array in
	let res = Array.make (Array.length gates_array) [] in
	for i = 0 to (n-1) do
		(* gate correspondant au vol i  .append  (id , t_arr, t_dep) ::  *) 
		res.(vols_array.(i).Vol.idGateGroup) <- ( vols_array.(i).Vol.id , vols_array.(i).Vol.t_arr, vols_array.(i).Vol.t_dep)::res.(vols_array.(i).Vol.idGateGroup);
	done;
	res;;	
		
	
(* trouve l'ecart minimum entre les vols consecutifs d'une gate *) (*PAS SUR QUE CETTE FONCTION FONCTIONNE JE L'AI PAS ENCORE TESTEE*)
let ecart_mini_gate vols_liste = 
	let rec parcourt liste minimum = match liste with
		|[] -> minimum
		|[(id,a,b)] -> minimum
		|(id1,a,b)::(id2,c,d)::q -> if (c-b)<minimum then (parcourt ((id2,c,d)::q) (c-b)) else (parcourt ((id2,c,d)::q) minimum)
	in
	parcourt vols_liste max_int;;
	
	
(* crée un tableau qui contient l'écart minimum entre les vols consécutifs de chaque gate *)
let tableau_conflits vols_array gates_array repar = 
	let n = Array.length gates_array in
	
	let gates_ecarts = Array.make n (-max_int) in
	let mini_courant = ref 0 in
	
	for i = 0 to (n-1) do (* ici i joue le rôle de id_gategroup dans l'autre fonction *)
		begin
		mini_courant := (ecart_mini_gate (trirap repar.(i)));
		gates_ecarts.(i) <- !mini_courant

		end;
	done;
	gates_ecarts;;
	
	
(*-------------------------------------------------------------------------------------------------------------------------------------*)


	






(*-------------------- Modifications --------------------------------------------------------------------------------------------------*)

let find_min tab =																		(* Fonction min *)
		let min = ref max_int in 
		for i = 0 to ((Array.length tab)-1) 
			do
				if tab.(i) < !min 
					then 
						min := tab.(i);
			done;
		!min ;;
		



(* fonction de suppression d'un couple dans une liste : utile dans randomShift*)
let suppr id i j liste = 
	 let rec suppr_rec id i j liste = match liste with
	 	|[] -> []
	 	|(k,l,m)::q ->  if ( (k,l,m)=(id,i,j) ) then q else ( (k,l,m) :: (suppr_rec id i j q));
	 in 
	 suppr_rec id i j liste ;;

(* fonction d'ajout d'un triple dans une liste triée selon la deuxième composante : utile dans randomShift*)
let rec add_sort id i j liste = match liste with 
	| [] -> [(id,i,j)]
	| (id1,k,l):: q -> if (i>k) then (id1,k,l)::(add_sort id i j q) else (id,i,j)::(id1,k,l)::q;;
	

(* modification elementaire du vol i et modif de repartition_array *)
let randomShift  vols_array  i idg_initial  idg_final  repartition_array  conflits_array= 
		
			(* on met à jour le tableau répartition_array *)
	let id_i = vols_array.(i).Vol.idGateGroup in
	repartition_array.(id_i) <- (suppr i vols_array.(i).Vol.t_arr vols_array.(i).Vol.t_dep repartition_array.(id_i));
			
			(* on shift un vol *)
	vols_array.(i).Vol.idGateGroup <- ((vols_array.(i).Vol.compatibleGateGroups).(idg_final)); (* ATTENTION VERIFIER QU'ON A BIEN IDG_FINAL ICI? Y'AVAIT NOTE "G"*)
			
	let id_f = vols_array.(i).Vol.idGateGroup in
	repartition_array.(id_f)   <- (add_sort i vols_array.(i).Vol.t_arr vols_array.(i).Vol.t_dep repartition_array.(id_f));
			(* on met à jour le tableau conflits_array *)
	conflits_array.(id_i) <- (ecart_mini_gate repartition_array.(id_i));
	conflits_array.(id_f) <- (ecart_mini_gate repartition_array.(id_f));;
	
(*-------------------------------------------------------------------------------------------------------------------------------------*)
	
	
	
(*---------------------- DeltaE -------------------------------------------------------------------------------------------------------*)

(* Fait une variation élémentaire en place et qui retourne la différence d'énergie qu'elle entraine *)
let deltaE vols_array gates_array conflits_array repartition_array  = 
	
	
	let soluce1 = (find_min conflits_array) in 												 (* Solution avant modification élémentaire *)
	(*print_int soluce1;
	print_string "<>";*)
	let i = Random.int(Array.length( vols_array ) ) in
	(* print_int i; *)
	(* print_string " - "; *)   															(*ça ça montre que on a bien des avions différents qui sont déplacés*)
	let idg_initial = vols_array.(i).Vol.idGateGroup in 
	let idg_final = Random.int (Array.length (vols_array.(i).Vol.compatibleGateGroups)) in
	(* print_int idg_final; *)
	(* print_string " - ";    *)
	randomShift vols_array i idg_initial idg_final repartition_array conflits_array; (* Modification élémentaire *)
	
	let soluce2 = (find_min conflits_array) in												 (* Solution après modification élémentaire *)
	(*print_int soluce2;
	print_string "=> ";
	print_int (soluce2 - soluce1);*)
	(soluce2 - soluce1) ;;																	 (* DeltaE *)
		
(*-------------------------------------------------------------------------------------------------------------------------------------*)
		
		
		

		
		
(*------------------ Initialisation ---------------------------------------------------------------------------------------------------------*)

(* attribue à chaque vol un aircraft id correct et une clé primaire *)
let preProcessingId vols avions_array = 
	for i = 0 to ((Array.length vols)-1) do
		vols.(i).Vol.aircraft_id <- (TypeAvion.trouve_indice (vols.(i).Vol.aircraft_type) avions_array);
		vols.(i).Vol.id <- i;
	done;;

(* contruit la liste des gates possibles pour chaque vol *)
let preProcessingGatesIndiv vols avions g i = 
	let n = Array.length (avions.(vols.(i).Vol.aircraft_id).TypeAvion.compatible_gate_group) in
	let res = Array.make n 0 in
	let compteur = ref 0 in
	for j=0 to (n-1) do
		if (GateGroup.compatible (vols.(i).Vol.terminal) g.((avions.(vols.(i).Vol.aircraft_id).TypeAvion.compatible_gate_group).(j)))
		then
		begin
		res.(!compteur) <- ((avions.(vols.(i).Vol.aircraft_id).TypeAvion.compatible_gate_group).(j));
		incr compteur;
		end
	done;
	Array.sub res 0 (!compteur);;
		
	
let preProcessingGates vols avions gates = 
	for i=0 to ((Array.length vols)-1) do
		vols.(i).Vol.compatibleGateGroups <- preProcessingGatesIndiv vols avions gates i;
	done;;
	
	
	

(* modification elementaire du vol i *)
let randomShift_init vols_array i = 
	let g = Random.int (Array.length (vols_array.(i).Vol.compatibleGateGroups))
	in
	vols_array.(i).idGateGroup <- ((vols_array.(i).Vol.compatibleGateGroups).(g));;
	
(* attribue une porte aleatoire à chaque vol *)
let initAleatDecision vols avions_array = 
	for i = 0 to ((Array.length vols)-1) do
		randomShift_init vols i;
	done;;
	
	
	

let temp_initiale  vols_array gates_array conflits_array repartition_array = 
	let sum = ref 0. in
	for i=1 to 500 do 
		sum := !sum +. (abs_float (float_of_int (deltaE  vols_array gates_array conflits_array repartition_array )));
	done;
	-. (((!sum)/.500.) /. (log 0.8));;
	
(*-------------------------------------------------------------------------------------------------------------------------------------*)
