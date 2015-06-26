let main () =
	let h = new Hydrogen.hydrogen in
	let o = new Oxygen.oxygen in
	let c = new Carbon.carbon in
	let a = new Argon.argon in
	let ba = new Barium.barium in
	let bo = new Boron.boron in
	let ca = new Calcium.calcium in
	let cl = new Chlorine.chlorine in
	print_endline h#to_string;
	print_endline o#to_string;
	print_endline c#to_string;
	print_endline (string_of_bool (h#equals h));
	print_endline (string_of_bool (h#equals o));
	print_endline (string_of_bool (h#equals c));


print_string a#to_string;
print_string ba#to_string;
print_string bo#to_string;
print_string ca#to_string;
print_endline cl#to_string

let () = main()