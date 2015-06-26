class virtual atom =
	object (self)
		method virtual name : string
		method virtual symbol : string
		method virtual atomic_number : int
		method to_string = "______________\n|   " ^  self#name ^ "\n|     " ^ self#symbol ^ "\n|     " ^ (string_of_int self#atomic_number) ^ "\n|______________\n"
		method equals (that: atom) =
			((that#name = self#name) && (that#symbol = self#symbol) && (that#atomic_number = self#atomic_number))
	end
	