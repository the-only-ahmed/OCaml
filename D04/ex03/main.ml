(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:17:42 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:17:44 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_lst lst =
	match lst with
	| [] -> print_char '\n'
	| hd::tl -> print_endline hd; print_lst tl

let rec draw_all lst =
	let (x, y) = (Deck.drawCard lst) in
	print_endline ((Deck.Card.toString x) ^ " - " ^ (Deck.Card.Value.toStringVerbose (Deck.Card.getValue x)) ^ " - " ^ (Deck.Card.Color.toStringVerbose (Deck.Card.getColor x)));
	draw_all y
	(* fini par lancer une exception *)

let main () =
	Random.self_init ();
	let decko = Deck.newDeck () in
	print_endline "Premier deck print normal :";
	print_lst (Deck.toStringList decko);
	print_endline "Premier deck print verbose :";
	print_lst (Deck.toStringListVerbose decko);
	let deck2 = Deck.newDeck () in
	print_endline "Second deck print verbose :";
	print_lst (Deck.toStringListVerbose deck2);
	print_endline "On pioche toutes les carte du premier deck et on fini par une exception :";
	try (draw_all decko) with Failure ex -> print_endline ex

let () = main ()
