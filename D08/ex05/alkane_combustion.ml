	let get_alcane lst =

		let rec loopmin lst tete =
		match lst with
		| [] -> [tete]
		| hd::tl when List.length (hd#get_atom) < List.length (tete#get_atom) -> loopmin tl hd
		| _::tl -> loopmin tl tete
	in
		let rec loop l =
		match l with
		| [] -> []
		| hd::tl when hd#name = "Dioxygen" -> loop tl
		| hd::tl -> [hd] @ (loop tl)
	in
	let lstmol = loop lst in
	loopmin lstmol (List.hd lstmol)

class alkane_combustion (startlst:Alkane.alkane list) =
	object (self)
	inherit Reaction.reaction (startlst @ [(new Dioxygen.dioxygen)]) ([(new Carbon_dioxyde.carbon_dioxyde)] @ [(new Water.water)])
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

	method get_start = if self#is_balanced = false then begin raise (Failure "Its Not Balanced."); (self#get_tuple _start) end else (self#get_tuple _start)
	method lol_get_start = (self#get_tuple _start)
	method lol_raw_get_start = _start
	method lol_raw_get_end = _end
	method get_incomplete_results2 =

		let rec get_new_list lst div =
		match lst with
		| [] -> []
		| (x, y)::tl -> [(x, (y/div))] @ (get_new_list tl div)
		and count_diox lst acc =
			match lst with
			| [] -> acc
			| (x, y)::tl when x#name = "Dioxygen" -> y
			| _::tl -> count_diox tl acc
		and get_number_alc lst acc =
			match lst with
			| [] -> acc
			| (x, y)::tl when x#name <> "Dioxygen" && (y < acc || acc = 0) -> get_number_alc tl y
			| _::tl -> get_number_alc tl acc
		in
		let balanced = self#balance in
		let start = balanced#lol_get_start in

		let nb_diox = count_diox start 0 in
		let nb_alc = get_number_alc start 0 in

		let newlist = if nb_alc > 0 then get_new_list start nb_alc else start in

		let nb_pos = ((nb_diox / nb_alc) - 1) in

		let rec make_bf acc currentlist ret =
				let rec balancing_inc thereac =
											let rec get_nb_c lst acc =
							    			match lst with
							    			| [] -> acc
							    			| hd::tl when hd#name = "Carbon" -> get_nb_c tl (acc+1)
							    			| _::tl -> get_nb_c tl (acc)
											and get_nb_o lst acc =
							    			match lst with
							    			| [] -> acc
							    			| hd::tl when hd#name = "Oxygen" -> get_nb_o tl (acc+1)
							    			| _::tl -> get_nb_o tl (acc)
											in
											let mod1 = thereac#lol_get_start in
											let ret1 = (thereac#extract_atom mod1) in
											let mod2 = thereac#lol_get_result in
											let ret2 = (thereac#extract_atom mod2) in
												let nb_c1 = (get_nb_c ret1 0) in
												let nb_o1 = (get_nb_o ret1 0) in
												let nb_c2 = (get_nb_c ret2 0) in
												let nb_o2 = (get_nb_o ret2 0) in
						(* 						print_endline ("nb c1 " ^ (string_of_int nb_c1));
												print_endline ("nb c2 " ^ (string_of_int nb_c2));
												print_endline ("nb o1 " ^ (string_of_int nb_o1));
												print_endline ("nb o2 " ^ (string_of_int nb_o2)); *)
												if (thereac#is_balanced = true) then
													thereac
												else if (nb_o1 = nb_o2 && nb_c1 = nb_c2) then
													thereac
												else if (nb_o1 = nb_o2 + 1 && nb_c1 > nb_c2) then
													balancing_inc (new Comb_helper.comb_helper thereac#lol_raw_get_start (thereac#lol_raw_get_end @ [(new Carbon_monoxyde.carbon_monoxyde)]))
												else if (nb_o1 > nb_o2 && nb_c1 > nb_c2) then
													balancing_inc (new Comb_helper.comb_helper thereac#lol_raw_get_start (thereac#lol_raw_get_end @ [(new Carbon_dioxyde.carbon_dioxyde)]))
												else
													balancing_inc (new Comb_helper.comb_helper thereac#lol_raw_get_start (thereac#lol_raw_get_end @ [(new Carbon_atom.carbon_atom)]))
													(* thereac *)

				in
						if acc > 0 then begin
								let rec firstlist nb =
									if nb > 0 then
										([(new Water.water)]) @ (firstlist (nb-1))
									else
										[]
								in
								let rec getnbh lst acc =
									let rec counth lst acc =
									match lst with
									| [] -> acc
									| hd::tl when hd#name = "Hydrogen" -> counth tl (acc+1)
									| _::tl -> counth tl (acc)
								in
								match lst with
					    			| [] -> acc
					    			| (x, y)::tl -> let nbh = ((counth (x#get_atom) 0) * y) in getnbh tl (acc+nbh)
					    		in
					    		let rec dec_dyox lst =
									match lst with
									| [] -> []
									| (x, y)::tl when x#name = "Dioxygen" -> [(x, (y-1))] @ (dec_dyox tl)
									| (x, y)::tl -> [(x, (y))] @ dec_dyox tl
								in
					    		let rec get_list_wt lst =
					    			let rec addxwhilenb elem acc =
					    			if acc > 0 then
					    				[elem] @ addxwhilenb elem (acc-1)
					    			else
					    				[]
					    			in
									match lst with
									| [] -> []
									| (x, y)::tl -> (addxwhilenb x y) @ (get_list_wt tl)
								in
								let nbh = getnbh currentlist 0 in
								let fl = (firstlist (nbh/ 2)) in
								let lol = (get_list_wt currentlist) in
								let reac = balancing_inc (new Comb_helper.comb_helper lol fl) in
								make_bf (acc -1) (dec_dyox currentlist) (ret @ [reac])				
							end else
									ret

		in
		make_bf nb_pos newlist []


method get_incomplete_results =
								let rec nb_ox lst acc =
								match lst with
					    			| [] -> acc
					    			| (x, y)::tl when x#name = "Dioxygen" -> y
					    			| hd::tl -> nb_ox tl acc
					    		in
	let rec loop lst =
	match lst with
	| [] -> []
	| hd::tl -> [((nb_ox (hd#lol_get_start) 0), hd#lol_get_result)] @ loop tl
	in
	loop self#get_incomplete_results2

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


	method get_result = if self#is_balanced = false then begin raise (Failure "Its Not Balanced."); (self#get_tuple _end) end else (self#get_tuple _end)
	method lol_get_result = (self#get_tuple _end)
    method balance = 
    	let rec loop comb =	
    		if comb#is_balanced = true then
    			comb
    		else begin
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
    			let mod1 = comb#lol_get_start in
				let lst1 = (comb#extract_atom mod1) in
				let mod2 = comb#lol_get_result in
				let lst2 = (comb#extract_atom mod2) in
				let ret1 = List.sort (fun a1 b1 -> if (a1#symbol < b1#symbol) then -1 else if (a1#symbol > b1#symbol) then 1 else 0) lst1 in
				let ret2 = List.sort (fun a1 b1 -> if (a1#symbol < b1#symbol) then -1 else if (a1#symbol > b1#symbol) then 1 else 0) lst2 in
				let nb_c_ret1 = (get_nb_c ret1 0) in
				let nb_h_ret1 = (get_nb_h ret1 0) in
				let nb_o_ret1 = (get_nb_o ret1 0) in
				let nb_c_ret2 = (get_nb_c ret2 0) in
				let nb_h_ret2 = (get_nb_h ret2 0) in
				let nb_o_ret2 = (get_nb_o ret2 0) in
				
 				if (nb_h_ret1 > nb_h_ret2) then
					loop (new Comb_helper.comb_helper (comb#lol_raw_get_start) (comb#lol_raw_get_end @ [(new Water.water)]))
				else if (nb_o_ret1 < nb_o_ret2) then
					loop (new Comb_helper.comb_helper (comb#lol_raw_get_start @ [(new Dioxygen.dioxygen)]) (comb#lol_raw_get_end))
				else if (nb_c_ret1 < nb_c_ret2) then
					loop (new Comb_helper.comb_helper (comb#lol_raw_get_start @ (get_alcane comb#lol_raw_get_start)) (comb#lol_raw_get_end))
				else if (nb_c_ret1 > nb_c_ret2) then
					loop (new Comb_helper.comb_helper (comb#lol_raw_get_start) (comb#lol_raw_get_end @ [(new Carbon_dioxyde.carbon_dioxyde)]))
				else if (nb_h_ret1 < nb_h_ret2) then
					loop (new Comb_helper.comb_helper (comb#lol_raw_get_start @ (get_alcane comb#lol_raw_get_start)) (comb#lol_raw_get_end))
				else if (nb_o_ret1 > nb_o_ret2) then
					loop (new Comb_helper.comb_helper (comb#lol_raw_get_start) (comb#lol_raw_get_end @ [(new Carbon_dioxyde.carbon_dioxyde)]))
				else 
					comb
    		end
	    in
	    loop (new alkane_combustion (startlst))
	end