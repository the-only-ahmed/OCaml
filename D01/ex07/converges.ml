(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 12:22:26 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/17 12:30:52 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =
    if (n < 0) then
        false
    else if (f x = x) then
        true
    else
        converges f (f x) (n - 1)

let () =
    print_endline (string_of_bool (converges (( * ) 2) 2 5));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 3));
    print_endline (string_of_bool (converges (fun x -> x /  2) 2 2))
