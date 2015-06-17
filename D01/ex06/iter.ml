(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 12:13:09 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/17 12:21:51 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec iter f x n =
    if (n < 0) then
        -1
    else if (n = 0) then
        x
    else if (n = 1) then
        f x
    else
        iter f (f x) (n - 1)

let () =
    print_int (iter (fun x -> x * x) 2 4);
    print_char '\n';
    print_int (iter (fun x -> x * 2) 2 4);
    print_char '\n'
