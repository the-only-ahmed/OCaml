class virtual reaction (startlst:Molecule.molecule list) (endlst:Molecule.molecule list) =
	object (self)
	val _start = startlst
	val _end = endlst
		method virtual get_start : (Molecule.molecule * int) list
		method virtual get_result : (Molecule.molecule * int) list
		method virtual balance : reaction
		method virtual is_balanced : bool
	end
