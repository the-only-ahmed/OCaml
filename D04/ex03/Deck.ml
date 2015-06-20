(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Deck.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:17:30 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:17:34 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Card =
struct


module Color =
struct
	type t = Spade | Heart | Diamond | Club

	let all = Spade::Heart::Diamond::Club::[]

	let toString color =
		match color with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

	let toStringVerbose color =
		match color with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"
end


module Value =
struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = T2::T3::T4::T5::T6::T7::T8::T9::T10::Jack::Queen::King::As::[]


	let toInt value =
		match value with
		| T2 -> 1
		| T3 -> 2
		| T4 -> 3
		| T5 -> 4
		| T6 -> 5
		| T7 -> 6
		| T8 -> 7
		| T9 -> 8
		| T10 -> 9
		| Jack -> 10
		| Queen -> 11
		| King -> 12
		| As -> 13

	let toString value =
		match value with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "J"
		| Queen -> "Q"
		| King -> "K"
		| As -> "A"

	let toStringVerbose value =
		match value with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "Jack"
		| Queen -> "Queen"
		| King -> "King"
		| As -> "As"

	let next value =
		match value with
		| T2 -> T3
		| T3 -> T4
		| T4 -> T5
		| T5 -> T6
		| T6 -> T7
		| T7 -> T8
		| T8 -> T9
		| T9 -> T10
		| T10 -> Jack
		| Jack -> Queen
		| Queen -> King
		| King -> As
		| As -> invalid_arg "there is no next t"

	let previous value =
		match value with
		| T2 -> invalid_arg  "there is no previous t"
		| T3 -> T2
		| T4 -> T3
		| T5 -> T4
		| T6 -> T5
		| T7 -> T6
		| T8 -> T7
		| T9 -> T8
		| T10 -> T9
		| Jack -> T10
		| Queen -> Jack
		| King -> Queen
		| As -> King

end

type t = { value:Value.t ; color:Color.t }

let newCard value color = { value = value; color = color}

let allWithColor (colo:Color.t) =
	let rec loop allV ret =
		match allV with
		| [] -> ret
		| hd::tl -> loop tl (ret @ [newCard hd colo])
	in
	loop Value.all []

let allSpades = allWithColor Color.Spade
let allHearts = allWithColor Color.Heart
let allDiamonds = allWithColor Color.Diamond
let allClubs = allWithColor Color.Club

let all = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue card = card.value

let getColor card = card.color

let toString card = ((Value.toString card.value) ^ (Color.toString card.color))

let toStringVerbose card = (Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose card.value) (Color.toStringVerbose card.color))

let max c1 c2 =
	if (Value.toInt c2.value) > (Value.toInt c1.value) then c2 else c1

let min c1 c2 =
	if (Value.toInt c2.value) < (Value.toInt c1.value) then c2 else c1

let compare card1 card2 =
	if card1.value = card2.value then 0
	else if (max card1 card2) = card1 then 1
	else -1

let best lst =
	match lst with
	| [] -> invalid_arg "empty list"
	| hd::_ -> List.fold_left max hd lst

let isOf card color =
	if color = card.color then true else false

let isSpade card = isOf card Color.Spade
let isHeart card = isOf card Color.Heart
let isDiamond card = isOf card Color.Diamond
let isClub card = isOf card Color.Club

end


type t = Card.t list

let size_list lst =
	let rec loop l acc =
		match l with
		| [] -> acc
		| _::tl -> loop tl (acc + 1)
	in
	loop lst 0

let newDeck () =
	let allcard = Card.all in
	let rec pop src elem =
		match src with
		| [] -> src
		| td::tl when td = elem -> tl
		| hd::tl -> [hd] @ (pop tl elem)
	in
	let rec get src nb acc =
		match src with
		| [] -> invalid_arg "no more list"
		| td::_ when acc = nb -> td
		| _::tl -> get tl nb (acc + 1)
	in
	let rec fill acc src dst =
		match src with
		| [] -> dst
		(* | _ when acc = 52 -> dst *)
		| _ -> 	let ran = Random.int(size_list src) in
				let elem = (get src ran 0) in
				fill (acc + 1) (pop src elem) (dst @ [elem])
	in
	fill 0 allcard []


let tsl dck f =
	let rec loop deck ret =
		match deck with
		| [] -> ret
		| hd::tl -> loop tl (ret @ [f hd])
	in
	loop dck []

let toStringList dck = tsl dck Card.toString

let toStringListVerbose dck = tsl dck Card.toStringVerbose

let drawCard deck =
	match deck with
	| [] -> failwith "deck is empty"
	| hd::tl -> (hd, tl)
