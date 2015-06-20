(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:16:57 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:16:58 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_all lst =
	match lst with
	| [] -> ()
	| hd::tl -> print_string (Color.toStringVerbose hd); print_string " -> "; print_endline (Color.toString hd); print_all tl

let main () =
	print_all Color.all

let () = main ()
