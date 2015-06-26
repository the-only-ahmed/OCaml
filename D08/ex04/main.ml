let print_tuple lst = 
	let rec loop l =
	match l with
	| [] -> print_endline "------------"
	| (x, y)::tl -> print_endline (x#to_string ^" x " ^  (string_of_int y)); loop tl
	in
	loop lst

let main () =
	let h = new Hydrogen.hydrogen in
	let o = new Oxygen.oxygen in
	let c = new Carbon.carbon in
	print_endline h#to_string;
	print_endline o#to_string;
	print_endline c#to_string;
	print_endline (string_of_bool (h#equals h));
	print_endline (string_of_bool (h#equals o));
	print_endline (string_of_bool (h#equals c));
let w = new Water.water in
	print_endline w#to_string;
let cd = new Carbon_dioxyde.carbon_dioxyde in
	print_endline cd#to_string;

let tnt = new Trinitrotoluene.trinitrotoluene in
	print_endline tnt#to_string;
let ca = new Chlorophylle_a.chlorophylle_a in
	print_endline ca#to_string;
let cb = new Chlorophylle_b.chlorophylle_b in
	print_endline cb#to_string;
let cc1 = new Chlorophylle_c1.chlorophylle_c1 in
	print_endline cc1#to_string;
let cc2 = new Chlorophylle_c2.chlorophylle_c2 in
	print_endline cc2#to_string;
let cd = new Chlorophylle_d.chlorophylle_d in
	print_endline cd#to_string;
let cf = new Chlorophylle_f.chlorophylle_f in
	print_endline cf#to_string;


let meth = new Methane.methane in
	print_endline meth#to_string;

let eth = new Ethane.ethane in
	print_endline eth#to_string;

let prop = new Propane.propane in
	print_endline prop#to_string;

let but = new Butane.butane in
	print_endline but#to_string;

let pent = new Pentane.pentane in
	print_endline pent#to_string;

let hex = new Hexane.hexane in
	print_endline hex#to_string;

let hept = new Heptane.heptane in
	print_endline hept#to_string;

let oct = new Octane.octane in
	print_endline oct#to_string;


let non = new Nonane.nonane in
	print_endline non#to_string;
let dec = new Decane.decane in
	print_endline dec#to_string;
	print_endline dec#formula;
	print_endline dec#name;
	print_endline (string_of_bool (dec#equals non));



	let ret1 = new Alkane_combustion.alkane_combustion ([eth]) in
	let etuplestart1 = (ret1#lol_get_start) in
	let etupleresult1 = (ret1#lol_get_result) in
	print_endline "START";
	print_tuple etuplestart1;
	print_endline "RESULT";
	print_tuple etupleresult1;
	print_endline "BOOL";
	print_endline (string_of_bool (ret1#is_balanced));

	let alc1 = (ret1#balance) in
	let tuplestart1 = (alc1#get_start) in
	let tupleresult1 = (alc1#get_result) in
	print_endline "START";

	print_tuple tuplestart1;
	print_endline "RESULT";
	print_tuple tupleresult1;
	print_endline "BOOL";

	print_endline (string_of_bool (alc1#is_balanced));


	let ret = new Alkane_combustion.alkane_combustion ([meth]) in
	let etuplestart = (ret#lol_get_start) in
	let etupleresult = (ret#lol_get_result) in
	print_endline "START";
	print_tuple etuplestart;
	print_endline "RESULT";
	print_tuple etupleresult;
	print_endline "BOOL";
	print_endline (string_of_bool (ret#is_balanced));

	let alc = (ret#balance) in
	let tuplestart = (alc#get_start) in
	let tupleresult = (alc#get_result) in
	print_endline "START";

	print_tuple tuplestart;
	print_endline "RESULT";
	print_tuple tupleresult;
	print_endline "BOOL";

	print_endline (string_of_bool (alc#is_balanced));

print_endline "testing exeception throwing";
	let ex = new Alkane_combustion.alkane_combustion ([eth]) in
	try 
	let a = ex#get_result in 
	print_endline "balanced.";
	with Failure s -> print_endline s

let () = main()