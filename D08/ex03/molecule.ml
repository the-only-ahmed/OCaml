class virtual molecule (name:string) (atoms:Atom.atom list) =
	object (self)
		val _name = name
		val _atoms = List.sort (fun a1 a2 -> if (a1#symbol < a2#symbol) then -1 else if (a1#symbol > a2#symbol) then 1 else 0) atoms
		method name = _name
		method formula =
			let rec count t lt acc =
			match lt with
			| [] -> (acc, [])
			| hd::tl when hd#equals t -> count t tl (acc+1)
			| _ -> (acc, lt)
			and loop lst = 
				match lst with
				| [] -> ""
				| hd::tl -> let (x, y) = (count hd tl 1) in (hd#symbol) ^ (if (x > 1 ) then (string_of_int x) else "") ^ (loop y)
			in
			loop _atoms
		method to_string = self#name ^ " " ^ self#formula
		method equals (that: molecule) =
			((that#name = self#name) && (that#formula = self#formula))
	end