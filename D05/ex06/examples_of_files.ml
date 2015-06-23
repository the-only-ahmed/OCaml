(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_files.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 21:57:15 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/23 21:57:18 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

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

let print_mytab tab =
	for i = 0 to ((Array.length tab) - 1) do
		print_endline (string_of_float tab.(i))
	done

let print_mylist lst =
	let rec loop l =
		match l with
		| [] -> print_char '\n'
		| (x, y)::tl -> print_mytab x; print_endline ("LETTRE FIN -> " ^ y); loop tl
	in
	loop lst

let main argc argv =
	if argc = 2 then
		begin
			try
				let ret = examples_of_files argv.(1) in
				print_mylist ret
			with e -> print_endline "FAIL"
		end
	else
		print_endline("Wrong number of argument, usage : " ^ (Array.get argv 0) ^ " <filename>")

let () = main (Array.length Sys.argv) Sys.argv
