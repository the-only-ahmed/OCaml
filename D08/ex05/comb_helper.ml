let print_tuple lst = 
	let rec loop l =
	match l with
	| [] -> print_endline "----ENDOFTUPLE--------"
	| (x, y)::tl -> print_endline (x#to_string ^ " x " ^ string_of_int y); loop tl
	in
	loop lst

class comb_helper (startlst:Molecule.molecule list) (endlst:Molecule.molecule list) =
	object (self)
	inherit Reaction.reaction (startlst) (endlst)
	method private get_tuple alst = 
		let rec loop_get x lt acc =
			match lt with
			| [] -> [(x, acc)]
			| hd::tl when (hd#equals x) -> (loop_get x tl (acc + 1))
			| hd::tl -> [(x, acc)] @ (loop_get hd tl 1)
		in
		match (List.sort (fun m1 m2 -> if m1#formula < m2#formula then -1 else if m1#formula > m2#formula then 1 else 0) alst) with
		| [] -> []
		| hd::tl -> loop_get hd tl 1

	method private extract_atom alst = 
		let rec get_all_atoms atomlist mul =
		if mul > 0 then
			atomlist @ (get_all_atoms atomlist (mul - 1))
		else
			[]
		in
		let rec loop lst =
		match lst with
		| [] -> []
		| (hda, hdb)::tl -> (get_all_atoms (hda#get_atom) hdb) @ loop tl
		in
		loop alst
    method get_incomplete_results2 = ([(new comb_helper [] [])] @ [(new comb_helper [] [])])
    method get_incomplete_results = [(1, [((new Carbon_dioxyde.carbon_dioxyde), 1)])]
(*     (int * (Carbon_dioxyde.carbon_dioxyde * int) list) list
    (int * (molecule * int) list) list *)
	method get_start = if self#is_balanced = false then begin raise (Failure "Its Not Balanced."); (self#get_tuple _start) end else (self#get_tuple _start)
	method lol_get_start = (self#get_tuple _start)
	method lol_raw_get_start = _start
	method lol_raw_get_end = _end

method is_balanced =
		let mod1 = (self#get_tuple _start) in

		let lst1 = (self#extract_atom mod1) in

		let mod2 = (self#get_tuple _end) in

		let lst2 = (self#extract_atom mod2) in
		let ret1 = List.sort (fun a1 b1 -> if (a1#symbol < b1#symbol) then -1 else if (a1#symbol > b1#symbol) then 1 else 0) lst1 in
		let ret2 = List.sort (fun a1 b1 -> if (a1#symbol < b1#symbol) then -1 else if (a1#symbol > b1#symbol) then 1 else 0) lst2 in

				let rec get_nb_c lst acc =
    			match lst with
    			| [] -> acc
    			| hd::tl when hd#name = "Carbon" -> get_nb_c tl (acc+1)
    			| _::tl -> get_nb_c tl (acc)
				and get_nb_h lst acc =
    			match lst with
    			| [] -> acc
    			| hd::tl when hd#name = "Hydrogen" -> get_nb_h tl (acc+1)
    			| _::tl -> get_nb_h tl (acc)
				and get_nb_o lst acc =
    			match lst with
    			| [] -> acc
    			| hd::tl when hd#name = "Oxygen" -> get_nb_o tl (acc+1)
    			| _::tl -> get_nb_o tl (acc)
				in
				let nb_c_ret1 = (get_nb_c ret1 0) in
				let nb_h_ret1 = (get_nb_h ret1 0) in
				let nb_o_ret1 = (get_nb_o ret1 0) in
				let nb_c_ret2 = (get_nb_c ret2 0) in
				let nb_h_ret2 = (get_nb_h ret2 0) in
				let nb_o_ret2 = (get_nb_o ret2 0) in
				nb_o_ret2 = nb_o_ret1 && nb_h_ret2 = nb_h_ret1 &&  nb_c_ret2 = nb_c_ret1
(* 		let rec looploop x lt acc =
			match lt with
			| [] -> [(x, acc)]
			| hd::tl when (hd#equals x) -> (looploop x tl (acc + 1))
			| hd::tl -> [(x, acc)] @ (looploop hd tl 1)
		in

		let finaleret1 =
		match ret1 with
		| [] -> []
		| hd::tl -> looploop hd tl 1
		in
		let finaleret2 =
		match ret2 with
		| [] -> []
		| hd::tl -> looploop hd tl 1
		in
		let rec autre_fonction (x, y) alst2 =
			match alst2 with
			| [] -> false
			| (a1, a2)::tl when a1 = x && a2 = y -> true
			| (a1, a2)::tl when a1 = x -> false
			| _::tl -> autre_fonction (x, y) tl
		in
		let rec loop lst =
			match lst with
			| [] -> true
			| hd::tl ->  if (autre_fonction hd finaleret2) = true then loop tl else false
		in
		loop finaleret1 *)

	method get_result = if self#is_balanced = false then begin raise (Failure "Its Not Balanced."); (self#get_tuple _end) end else (self#get_tuple _end)
	method lol_get_result = (self#get_tuple _end)
    method balance = new comb_helper [] []
	end