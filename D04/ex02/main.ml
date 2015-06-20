(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:17:25 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:17:27 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_all f lst =
	match lst with
	| [] -> print_char '\n'
	| hd::tl -> print_endline (f hd); print_all f tl

let main () =
	let asPic = Card.newCard Card.Value.As Card.Color.Spade in
	let kingClub = Card.newCard Card.Value.King Card.Color.Club in
	let jackHeart = Card.newCard Card.Value.Jack Card.Color.Heart in
	let queenDiamond = Card.newCard Card.Value.Queen Card.Color.Diamond in
	print_endline "Get values :";
	print_endline (Card.Value.toString (Card.getValue asPic));
	print_endline (Card.Value.toString (Card.getValue kingClub));
	print_endline (Card.Value.toString (Card.getValue jackHeart));
	print_endline (Card.Value.toString (Card.getValue queenDiamond));
	print_endline "Get colors :";
	print_endline (Card.Color.toString (Card.getColor asPic));
	print_endline (Card.Color.toString (Card.getColor kingClub));
	print_endline (Card.Color.toString (Card.getColor jackHeart));
	print_endline (Card.Color.toString (Card.getColor queenDiamond));
 	print_endline "Print toString:";
	print_all Card.toString Card.all;
 	print_endline "Print toStringVerbose:";
	print_all Card.toStringVerbose Card.all;
	print_endline "Max -- Min:";
	print_string "\tmax -> As Spade - Queen Diamond: ";
	print_endline (Card.toStringVerbose (Card.max asPic queenDiamond));
	print_string "\tmin -> jack Heart - King Club: ";
	print_endline (Card.toStringVerbose (Card.min jackHeart kingClub));
	print_endline "Compare :";
	print_endline "\t As Spade - King Club:";
	print_endline (string_of_int (Card.compare asPic kingClub));
	print_endline "\t Queen Diamond - Queen Diamond:";
	print_endline (string_of_int (Card.compare queenDiamond queenDiamond));
	print_endline "Best :";
	print_endline (Card.toStringVerbose (Card.best Card.allSpades));
	print_endline "Is Of:";
	print_endline ("As Spade -> ? heart : " ^ (string_of_bool (Card.isOf asPic Card.Color.Heart)));
	print_endline ("As Spade -> ? space : " ^ (string_of_bool (Card.isOf asPic Card.Color.Spade)));
	print_endline "Is Diamond:";
	print_endline ("Queen Diamond -> ? diamond : " ^ (string_of_bool (Card.isDiamond queenDiamond)))

let () = main ()
