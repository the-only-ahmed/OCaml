let atom_list _n = 
			let rec loopc acc =
				if acc > 0 then
					([(new Carbon.carbon)]) @ (loopc (acc-1))
				else
					[]
			in
			let rec looph acc =
				if acc > 0 then
					([(new Hydrogen.hydrogen)]) @ (looph (acc-1))
				else
					[]
			in
			(loopc _n) @ (looph (2 * (_n + 1)))
				

let name _n = match _n with
						| 1 -> "Methane"
						| 2 -> "Ethane"
						| 3 -> "Propane"
						| 4 -> "Butane"
						| 5 -> "Pentane"
						| 6 -> "Hexane"
						| 7 -> "Heptane"
						| 8 -> "Octane"
						| 9 -> "Nonane"
						| 10 -> "Decane"
						| 11 -> "Undecane"
						| 12 -> "Dodecane"
						| 13 -> "Tridecane"
						| 14 -> "Tetradecane"
						| 15 -> "Pentadecane"
						| 16 -> "Cetane"
						| 17 -> "Heptadecane"
						| 18 -> "Octadecane"
						| 19 -> "Nonadecane"
						| 20 -> "Eicosane"
						| 21 -> "Heneicosane"
						| 22 -> "Docosane"
						| 23 -> "Tricosane"
						| 24 -> "Tetracosane"
						| 25 -> "Pentacosane"
						| 26 -> "Hexacosane"
						| 27 -> "Heptacosane"
						| 28 -> "Octacosane"
						| 29 -> "Nonacosane"
						| 30 -> "Triacontane"
						| 31 -> "Untriacontane"
						| 32 -> "Dotriacontane"
						| 33 -> "Tritriacontane"
						| 34 -> "Tetratriacontane"
						| 35 -> "Pentatriacontane"
						| 36 -> "Hexatriacontane"
						| 37 -> "Heptatriacontane"
						| 38 -> "Octatriacontane"
						| 39 -> "Nonatriacontane"
						| 40 -> "Tetracontane"
						| _ -> "invalid"

class virtual alkane (n:int) =
	object (self)
	inherit Molecule.molecule (name n) (atom_list n)
		val _n = n
		
		(* method formula = if _n = 1 then "CH4" else ("C" ^ (string_of_int _n) ^ "H" ^ (string_of_int (2 * (_n + 1))) ) *)
		(* method to_string = if self#name = "invalid" then "" else self#name ^ " " ^ self#formula *)
		(* method equals (that: alkane) = *)
			(* ((that#name = self#name) && (that#formula = self#formula)) *)
	end