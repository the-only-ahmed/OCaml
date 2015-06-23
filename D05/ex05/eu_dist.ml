(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 21:57:10 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/23 21:57:12 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

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

let main () =
	print_endline (string_of_float (eu_dist [|3.;4.|] [|0.;0.|]))

let () = main ()
