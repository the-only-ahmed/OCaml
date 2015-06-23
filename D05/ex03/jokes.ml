(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 21:56:59 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/23 21:57:01 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_joke joks =
	let nbJook = Array.length joks in
	let rd = (Random.int nbJook) in
	print_endline (joks.(rd))

let readFile file =
	let tmp = Array.make 0 "" in
	let array = ref (tmp) in
	try
		while true do
			let line = input_line file in
			array := Array.append !array [|line|]
		done
	with
	| End_of_file -> print_joke !array

let main argc argv =
	Random.self_init ();
	if argc = 2 then
		begin
			try
				let file = open_in argv.(1) in
				readFile file
			with e -> print_endline "ERROR"
		end
	else
		print_endline("Wrong number of argument, usage : " ^ (Array.get argv 0) ^ " <filename>")

let () = main (Array.length Sys.argv) Sys.argv
