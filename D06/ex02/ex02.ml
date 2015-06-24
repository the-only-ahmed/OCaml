(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 22:39:21 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/24 22:39:24 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end


(* FIX ME !!! *)

module type MAKEPROJECTION =
	functor (Pair : PAIR) ->
		VAL

module MakeFst : MAKEPROJECTION =
	functor (Pair : PAIR) ->
		struct
			let retFm (fm, _) = fm
			let x = retFm Pair.pair
		end

module MakeSnd : MAKEPROJECTION =
	functor (Pair : PAIR) ->
		struct
			let retSm (_, sm) = sm
			let x = retSm Pair.pair
		end


module Pair : PAIR = struct let pair = ( 21, 42 ) end
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x

(* 			$> ocamlopt ex02.ml && ./a.out 			*)
(* 			Fst.x = 21, Snd.x = 42 					*)
(* 			$> 										*)
