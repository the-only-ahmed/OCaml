(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 12:15:52 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/16 12:57:53 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ackermann x y =
    let rec loop m n =
        if (m > 0 && n = 0) then
            loop (m - 1) 1
        else if (m > 0 && n > 0) then
            loop (m - 1) (loop m (n - 1))
        else if (m = 0) then
             n + 1
        else
            n
    in
    if (x < 0 || y < 0) then
        -1
    else
        loop x y

let main() =
    print_int (ackermann (-1) 7);
    print_char '\n';
    print_int (ackermann 0 0);
    print_char '\n';
    print_int (ackermann 2 3);
    print_char '\n';
    print_int (ackermann 4 1);
    print_char '\n'

let () = main ()
