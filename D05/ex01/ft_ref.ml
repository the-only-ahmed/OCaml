(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 21:56:43 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/23 21:56:45 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = { mutable data : 'a }

let return data = { data = data }

let get myRef = myRef.data

let set myRef data = myRef.data <- data

let bind (myRef:'a ft_ref) (fn:('a -> 'b ft_ref)) = fn (get myRef)

let main () =
	let x = return 42 in
	print_endline (string_of_int (get x));
	set x 54;
	print_endline (string_of_int (get x));
	let y = bind x (fun x -> return (x * 2)) in
	print_endline (string_of_int (get y))


let () = main ()


(* return: 'a -> 'a ft_ref: creates a new reference. *)
(* get: 'a ft_ref -> 'a: Dereferences a reference. *)
(* set: 'a ft_ref -> 'a -> unit: Assign a reference's value. *)
(* bind: 'a ft_ref -> ('a -> 'b ft_ref) -> 'b ft_ref *)
