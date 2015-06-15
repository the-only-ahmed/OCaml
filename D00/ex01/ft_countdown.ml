(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 18:46:08 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 18:52:30 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x =
    print_int x;
    print_char '\n';
    if x > 0
    then ft_countdown(x - 1)

let main() =
    ft_countdown 5;
    print_char('\n');
    ft_countdown 0;
    print_char('\n');
    ft_countdown (-1)

let () = main ()
