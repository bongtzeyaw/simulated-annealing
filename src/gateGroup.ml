(* Projet OCAML/IA *)
(* Analyse des fichiers relatifs au type gateGroup *)
(* Virgile Receveur IENAC18_SITA *)

(* Définition des chemins vers les fichiers utilisés *)
let dep_ = "./data/dependentgates"					(* Dépendance des portes *)
let gates_ = "./data/gates.txt"						(* Liste des portes *)


(* Définition du type gateGroup *)
type gateGroup = {
	idGateGroup : int;								(* Identifiant du groupe de portes *)
	gates : (char*string) list 						(* Liste des numéro de portes dans le groupe *)
	};;


(* Fonction qui compte le nombre de ligne dans un fichier *)
let comptage file= 
	let fic = open_in file in
	let rec compt fic id = 
		try 
			input_line fic;
			id+1;
			(compt fic id+1);
		with End_of_file -> 0;
	in (compt fic 0);;	

(*Définition de constantes globales : *)
let nb_dep = 3;; 									(* Nombre de portes par groupe de portes dépendantes *)
let long_dep = comptage dep_;; 				 		(* Nombre de groupe de portes dépendantes *) 
let nb_group = comptage gates_;; 					(* Nombre total de groupe de portes *)

(* Fonction de formatage pour le fichier dependantgates *)
let read_dependline line = 
	Scanf.sscanf line "%3s %3s %3s;" (fun a b c -> [a; b; c]);;


(* Fonction qui crée un gateGroup pour les portes dépendantes *)
let make_gate_depend line = 
	Scanf.sscanf line "%3s %3s %3s;" (fun a b c -> [a; b; c]);;


(* Fonction de formatage pour le fichier gates.txt *)
let read_line line = 
	Scanf.sscanf line "%3s:%s" (fun a b -> a);;

	
(* Fonction qui crée les gateGroup *)
let analyse_gate porte n =
	Scanf.sscanf porte "%c%2s" (fun a b -> {idGateGroup = n; gates =[(a,b)]});;

	
(* Fonction qui vérifie si porte se trouve dans liste *)
let isin_liste porte liste = (porte = List.nth liste 0)||( (porte = List.nth liste 1)||(porte = List.nth liste 2) );;
	

(* Fonction qui vérifie si une porte est indépendantes *)
let is_indep porte dependances =
	let test = ref true
	in for i = 0 to long_dep-1 do
		if (isin_liste porte dependances.(i)) then (test:=false);
		done;
	!test;;


(* Fonction d'analyse du fichier dependantgates *)
let analyse_depend file =
	let depend = Array.make long_dep ["";"";""]
	in let fic = open_in file in
	let rec lect n =
		try 
			if n = 0 then close_in fic else
			let line = input_line fic
			in depend.(n-1) <- (read_dependline line);
			lect (n-1);
		with End_of_file -> close_in fic;
	in lect long_dep;
	depend;;


(* Fonction d'analyse du fichier gates.txt *)
let analyse file depend = 
	let fic = open_in file
	in let tab_gates = Array.make nb_group {idGateGroup = 0; gates =[('a',"a")]}
	in let i = ref 0
	in let rec lect n = 
		try 
			let line = input_line fic
			in let porte = read_line line
			in begin 
				tab_gates.(!i) <- (analyse_gate porte !i); 
				i:=(!i+1);
			end;
			lect (n+1);
		with End_of_file -> close_in fic;
	in lect 0;
	tab_gates;;


(* Création du tableau des gateGroup *)
let createTable =
	let dependances = analyse_depend dep_
	in
	analyse gates_ dependances;;
	
(* renvoie true si le groupe contient une gate associée au terminal passé en argument *)
let compatible terminal group = 
	let rec parcourt liste = match liste with
		|[] -> false
		|(c,s)::q -> if (c=terminal) then true else parcourt q
	in
	parcourt (group.gates);;


	
let display group = 
	let rec parcourt l = match l with
		|[] -> ()
		|(c,s)::q -> print_string ((Char.escaped c)^s);parcourt q;
	in
	parcourt group.gates;;
	
	
	
	
	
	
	
	
	

