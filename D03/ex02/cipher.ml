(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 23:21:38 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/19 23:21:40 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

 let caesar s n =
	let u = if n < 0 then -1 else 0 in
	let a = int_of_char 'a'
	and a_A = int_of_char 'A'
	in
	let rot c =
		match c with
		| 'A' .. 'Z' -> char_of_int ((((int_of_char c) - a_A + n + (26 * ((n / 26) - 1) * u)) mod 26) + a_A)
		| 'a' .. 'z' -> char_of_int ((((int_of_char c) - a + n + (26 * ((n / 26) - 1) * u)) mod 26) + a)
		| _ -> c
	in
	String.map rot s

let rot42 s =
   caesar s 42

let xor s n =
	let xor_bis c = char_of_int ((int_of_char c) lxor n)
	in
	String.map xor_bis s

let rec ft_crypt s l = match l with
   | [] -> s
   | hd::ln -> ft_crypt (hd s) ln
