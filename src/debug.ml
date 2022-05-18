

	
(*------------------ Trucs de DEBUG ---------------------------------------------------------------------------------------------------*)


let print_array tab = 									(* TESTE LE CONFLIT_ARRAY *)
	for i=0 to ((Array.length tab)-1) do 
		print_int tab.(i);
		print_string " - "
		done;;
(* print_array conflits_array;; *)
	
	
let print_gates vols_array  = 							(* TESTE LE VOLS_ARRAY *)
	for i=0 to ((Array.length vols_array)-1) do 
		print_int vols_array.(i).Vol.idGateGroup;
		print_string " - "
		done;;
(* print_gates vols_array;; *)


let print_idgates gates_array  = 						(* TESTE LE GATES_ARRAY *)
	for i=0 to ((Array.length gates_array)-1) do 
		print_int gates_array.(i).GateGroup.idGateGroup;
		print_string " - "
		done;;
		
(*print_idgates gates_array;;*)


(* let print_gate_rep liste =  *)
	(* let rec parcourt l = match l with *)
		(* |[] -> print_string "]\n" *)
		(* |(a,b,c)::q -> print_string "(";print_int a; *)
					   (* print_string ", ";print_int b; *)
					   (* print_string ", ";print_int c; *)
					   (* print_string "); ";parcourt q; *)
	(* in  *)
	(* print_string "["; *)
	(* parcourt liste;; *)
	
	
(* let test_repartition repartition =  *)
	(* for i=0 to ((Array.length repartition)-1) do  *)
		(* print_gate_rep repartition.(i); *)
		(* print_string "\n" *)
		(* done;; *)
		
(*test_repartition repartition_array;;*)
(*-------------------------------------------------------------------------------------------------------------------------------------*)

