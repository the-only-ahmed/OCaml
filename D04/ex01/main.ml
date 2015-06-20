(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:17:03 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:17:06 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_card card =
	print_string "[" ;print_string (Values.toString card);print_string "] ";
	print_string (Values.toStringVerbose card);
	print_string " -- Int value : "; print_int (Values.toInt card); print_char '\n'


let rec print_val_cards lst =
	match lst with
	| [] -> ()
	| hd::tl -> print_card hd; print_val_cards tl

let rec previous_n_next lst =
	match lst with
	| [] -> ()
	| hd::tl -> print_endline "previous :"; print_card (Values.previous hd);
				print_endline "next :"; print_card (Values.next hd);
				previous_n_next tl


let main () =
	let lst2 =
		match Values.all with
		| [] -> []
		| _::tl -> tl
	in
	print_val_cards Values.all;
	previous_n_next lst2

let () = main ()
