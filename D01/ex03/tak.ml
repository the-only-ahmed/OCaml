(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tak.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 12:59:30 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/16 17:03:08 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let tak x y z =
    let rec loop x y z =
        if (y < x) then
            loop (loop (x - 1) y z) (loop (y - 1) z x) (loop (z - 1) x y)
        else
            z
    in loop x y z

let () =
    print_int (tak 1 2 3);
    print_char '\n';
    print_int (tak 5 23 7);
    print_char '\n';
    print_int (tak 9 1 0);
    print_char '\n';
    print_int (tak 1 1 1);
    print_char '\n';
    print_int (tak 0 42 0);
    print_char '\n';
    print_int (tak 23498 98734 98776);
    print_char '\n'
