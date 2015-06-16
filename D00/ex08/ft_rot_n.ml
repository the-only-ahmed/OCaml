let ft_rot_n nb s =
	let ascii_of_a = int_of_char 'a'
	and ascii_of_A = int_of_char 'A'
	in
	let func1 c =
		if c >= 'a' && c <= 'z' then
			char_of_int ((((int_of_char c) - ascii_of_a + nb) mod 26) + ascii_of_a)
		else if c >= 'A' && c <= 'Z' then
			char_of_int ((((int_of_char c) - ascii_of_A + nb) mod 26) + ascii_of_A)
		else c
	in
	String.map func1 s

(* let () = print_endline "Test with [1 'abcdefghijklmnopqrstuvwxyz']:";
	print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz")

let () = print_endline "Test with [13 'abcdefghijklmnopqrstuvwxyz']:";
	print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz")

let () = print_endline "Test with [42 '0123456789']:";
	print_endline (ft_rot_n 42 "0123456789")

let () = print_endline "Test with [2 'OI2EAS67B9']:";
	print_endline (ft_rot_n 2 "OI2EAS67B9")

let () = print_endline "Test with [0 'Damned !']:";
	print_endline (ft_rot_n 0 "Damned !")

let () = print_endline "Test with [42 '']:";
	print_endline (ft_rot_n 42 "")

let () = print_endline "Test with [1 'NBzlk qnbjr !']:";
	print_endline (ft_rot_n 1 "NBzlk qnbjr !")
 *)