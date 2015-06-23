(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   k_nn.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 21:57:27 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/23 21:57:31 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* radar list -> int -> radar -> string *)

type radar = float array * string

let eu_dist aArr bArr =
	let myMin f1 f2 = if f1 < f2 then f1 else f2 in
	let aLen = Array.length aArr in
	let bLen = Array.length bArr in
	let maxLen = myMin aLen bLen in
	let rec loop idx res =
		if idx = maxLen then (sqrt res)
		else loop (idx + 1) (res +. ((aArr.(idx) -. bArr.(idx)) ** 2.))
	in
	loop 0 0.


let rec print_ll lst =
	match lst with
	| [] -> print_char '\n'
	| (str, nb)::tl -> print_endline (str ^ " -> " ^ (string_of_int nb)) ; print_ll tl


let k_nn (lst:radar list) (hp:int) ((radArr, radStr):radar) =
	let addToList ol (ar, st, dt) =
		let sfn (_, _, d1) (_, _, d2) =
			if d1 > d2 then 1
			else if d1 = d2 then 0
			else -1
		in
		let rec retList ll acc retL =
			match ll with
			| [] -> retL
			| hd::tl when acc > hp -> retL
			| hd::tl -> retList tl (acc + 1) (retL @ [hd])
		in
		let newL = ol @ [(ar, st, dt)] in
		retList (List.sort sfn newL) 0 []
	in
	let treatRet (ls:(float array * string * float) list) =
		let rec notIn nl el =
			match nl with
			| [] -> true
			| (str, _)::tl when str = el -> false
			| hd::tl -> notIn tl el
		in
		let rec countIn cl ec acc =
			match cl with
			| [] -> acc
			| (_, str, _)::tl when str = ec -> countIn tl ec (acc + 1)
			| hd::tl -> countIn tl ec acc
		in
		let finalRet (ss, _) = ss in
		let sortRet (_, n1) (_, n2) =
			(* if n1 > n2 then 1 *)
			if n1 < n2 then 1
			else if n1 = n2 then 0
			else -1
		in
		let rec loopRet lll retList =
			match lll with
			(* | [] -> print_ll (List.sort sortRet retList); finalRet (List.hd (List.sort sortRet retList)) *)
			| [] -> finalRet (List.hd (List.sort sortRet retList))
			| (_, stri, _)::tl when (notIn retList stri) -> loopRet tl (retList @ [(stri, (countIn ls stri 0))])
			| hd::tl -> loopRet tl retList
		in
		loopRet ls []
	in
	let rec loop l ret =
		match l with
		| [] -> (treatRet ret)
		| (arr, s)::tl -> loop tl (addToList ret (arr, s, (eu_dist arr radArr)))
	in
	loop lst []

(* (rad * str * dist)   -> (str * nb) *)

let examples_of_files file =
	try
		let file = open_in file in
		let l = ref [] in
		try
			while true do
				let line = input_line file in
				let lst = Str.split_delim (Str.regexp ",") line in
				let len = List.length lst in
				let arr = Array.make (len - 1) 0. in
				for i = 0 to (len - 2)  do
					arr.(i) <- (float_of_string (List.nth lst i))
				done;
				l := (!l @ [(arr, (List.nth lst (len - 1)))])
			done;
			!l
		with
		| End_of_file -> !l
	with e -> failwith "yolo"

let main argc argv =
	if argc = 2 then
		begin
			try
				begin
					let radList = examples_of_files argv.(1) in
					let rad = ([|1.;0.;1.;-0.06182;1.;0.02942;1.;-0.05131;1.;-0.01707;1.;-0.11726;0.84493;-0.05202;0.93392;-0.06598;0.69170;-0.07379;0.65731;-0.20367;0.94910;-0.31558;0.80852;-0.31654;0.84932;-0.34838;0.72529;-0.29174;0.73094;-0.38576;0.54356;-0.26284;0.64207;-0.39487|], "") in
					let rad2 = ([|1.;0.;-0.54180;0.14861;-0.33746;0.73375;0.52012;-0.13932;0.31889;-0.06811;0.20743;-0.15170;0.47368;0.08978;0.56347;-0.15480;0.16409;0.45201;0.33746;0.03406;0.50464;0.07121;-0.63777;-0.61610;1.;0.65635;0.41348;-0.40116;-0.15170;0.11146;0.02399;0.55820;0.52632;-0.08978|], "") in
					print_endline ("RESULT[1] 1 -> " ^ (k_nn radList 1 rad));
					print_endline ("RESULT[1] 2 -> " ^ (k_nn radList 2 rad));
					print_endline ("RESULT[1] 3 -> " ^ (k_nn radList 3 rad));
					print_endline ("RESULT[1] 10 -> " ^ (k_nn radList 10 rad));
					print_endline ("RESULT[2] 1 -> " ^ (k_nn radList 1 rad2));
					print_endline ("RESULT[2] 5 -> " ^ (k_nn radList 5 rad2));
					print_endline ("RESULT[2] 10 -> " ^ (k_nn radList 10 rad2));
					print_endline ("RESULT[2] 20 -> " ^ (k_nn radList 20 rad2))
					(* let rad = ([|2.5|], "") in *)
					(* let rad2 = ([|9.0|], "") in *)
					(* print_endline ("RESULT[1] 1 -> " ^ (k_nn radList 1 rad)); *)
					(* print_endline ("RESULT[1] 2 -> " ^ (k_nn radList 2 rad)); *)
					(* print_endline ("RESULT[1] 3 -> " ^ (k_nn radList 3 rad)); *)
					(* print_endline ("RESULT[1] 10 -> " ^ (k_nn radList 10 rad)); *)
					(* print_endline ("RESULT[2] 1 -> " ^ (k_nn radList 1 rad2)); *)
					(* print_endline ("RESULT[2] 5 -> " ^ (k_nn radList 5 rad2)); *)
					(* print_endline ("RESULT[2] 10 -> " ^ (k_nn radList 10 rad2)); *)
					(* print_endline ("RESULT[2] 20 -> " ^ (k_nn radList 20 rad2)) *)
				end
			with e -> print_endline "FAIL"
		end
	else
		print_endline("Wrong number of argument, usage : " ^ (Array.get argv 0) ^ " <filename>")

let () = main (Array.length Sys.argv) Sys.argv


(* ligne de compilation : ocamlopt str.cmxa k_nn.ml *)
