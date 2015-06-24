(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 22:39:26 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/24 22:39:28 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* Write in the file "ex03.ml" a functor Make implementing the functor signature MAKE, *)
(* that takes as input modules implementing the signature FRACTIONNAL_BITS and outputs *)
(* modules that implement the signature FIXED. The signature FIXED is defined as follows: *)

module type FIXED =
sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig val bits : int end

module type MAKE =
	functor (FracBit : FRACTIONNAL_BITS) ->
		FIXED

module Make : MAKE =
	functor (FracBit : FRACTIONNAL_BITS) ->
		struct
			type t = int
			let of_float fl =
				let integer = (int_of_float fl) in
				let spare = fl -. (float_of_int integer) in
				let res = int_of_float (ceil (spare *. (2. ** (float_of_int FracBit.bits)))) in
				(integer lsl FracBit.bits) + res
			let of_int it =
				it lsl FracBit.bits
			let to_float fx =
				let x = fx lsr FracBit.bits in
				let y = fx land (int_of_float (2. ** (float_of_int FracBit.bits)) - 1) in
				(float_of_int x) +. ((float_of_int y) /. (2. ** (float_of_int FracBit.bits)))
			let to_int fx = fx lsr FracBit.bits
			let to_string fx = string_of_float (to_float fx)
			let zero = 0
			let one =  1 lsl FracBit.bits
			let succ fx = fx + 1
			let pred fx = fx - 1
			let min fx1 fx2 =
				if fx1 < fx2 then fx1 else fx2
			let max fx1 fx2 =
				if fx1 > fx2 then fx1 else fx2
			let gth fx1 fx2 = fx1 > fx2
			let lth fx1 fx2 = fx1 < fx2
			let gte fx1 fx2 = fx1 >= fx2
			let lte fx1 fx2 = fx1 <= fx2
			let eqp fx1 fx2 = fx1 == fx2 (** physical equality *)
			let eqs fx1 fx2 = fx1 = fx2 (** structural equality *)
			let add fx1 fx2 = fx1 + fx2
			let sub fx1 fx2 = fx1 - fx2
			let mul fx1 fx2 = of_float ((to_float fx1) *. (to_float fx2))
			let div fx1 fx2 =
				if fx2 = 0 then
					Ivalid_argument "Can't divide by zero"
				else
					of_float ((to_float fx1) /. (to_float fx2))
			let foreach fx1 fx2 fn =
				let rec loopUp stUp =
					if stUp < fx2 then
						begin fn stUp; loopUp (succ stUp) end
				in
				let rec loopDown stDow =
					if stDow > fx2 then
						begin fn stDow; loopDown (pred stDow) end
				in
				if (gth fx1 fx2) then
					loopDown fx1
				else
					loopUp fx1
		end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let y8_2 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	let r9 = Fixed8.sub x8 y8 in
	let r10 = Fixed8.mul x8 y8 in
	let r11 = Fixed8.div x8 y8 in
	print_endline (string_of_bool (Fixed8.eqp y8 y8_2));
	print_endline (string_of_bool (Fixed8.eqs y8 y8_2));
	print_endline ("gte -> " ^ (string_of_bool (Fixed8.gte y8 y8_2)));
	print_endline ("gte -> " ^ (string_of_bool (Fixed8.gte y8 x8)));
	print_endline ("lte -> " ^ (string_of_bool (Fixed8.gte y8 y8_2)));
	print_endline ("lte -> " ^ (string_of_bool (Fixed8.gte y8 x8)));
	print_endline ("x8-> " ^ (Fixed8.to_string x8));
	print_endline ("y8-> " ^ (Fixed8.to_string y8));
	print_endline ("r8-> " ^ (Fixed8.to_string r8));
	print_endline ("r9-> " ^ (Fixed8.to_string r9));
	print_endline ("r10-> " ^ (Fixed8.to_string r10));
	print_endline ("r11-> " ^ (Fixed8.to_string r11));
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))


(* 			$> ocamlopt ex03.ml && ./a.out 		*)
(* 			42.421875 							*)
(* 			0. 									*)
(* 			0.0625								*)
(* 			0.125 								*)
(* 			0.1875 								*)
(* 			0.25 								*)
(* 			0.3125 								*)
(* 			0.375 								*)
(* 			0.4375 								*)
(* 			0.5 								*)
(* 			0.5625 								*)
(* 			0.625 								*)
(* 			0.6875 								*)
(* 			0.75 								*)
(* 			0.8125 								*)
(* 			0.875 								*)
(* 			0.9375 								*)
(* 			1. 									*)
(* 			$> 									*)
