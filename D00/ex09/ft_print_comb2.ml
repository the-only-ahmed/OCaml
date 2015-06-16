let ft_print_comb2 () =
	let print_nbr n1 n2 = 
		if n1 < 10 then print_int 0;
		print_int n1;
		print_char ' ';
		if n2 < 10 then print_int 0;
		print_int n2;
	in
	let rec func1 nb =
		if nb = 98 then
			print_nbr nb 99
		else
			begin
				let rec func2 nb2 =
					if nb2 < 100 then
						begin
							print_nbr nb nb2;
							print_char ',';
							print_char ' ';
							func2 (nb2 + 1)
						end
				in
				func2 (nb + 1);
				func1 (nb + 1)
			end
	in
	func1 0;
	print_char '\n'

(* let () = ft_print_comb2 ()
 *)