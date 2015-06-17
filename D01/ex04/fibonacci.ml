(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 11:59:26 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/17 12:02:03 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
    let rec fib_help a b nb =
        if nb < 0 then
            -1
        else if nb = 0 then
            a
        else fib_help b (a + b) (nb - 1)
    in
    fib_help 0 1 n

let () =
    print_int (fibonacci (-42));
    print_char '\n';
    print_int (fibonacci 1);
    print_char '\n';
    print_int (fibonacci 3);
    print_char '\n';
    print_int (fibonacci 6);
    print_char '\n';
    print_int (fibonacci 50);
    print_char '\n'
