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
	print_endline dec#to_string

let () = main()