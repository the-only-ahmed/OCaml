(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 18:53:11 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 19:19:56 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power x y =
    if y > 0
        then x * ft_power x (y - 1)
     else
         1

let main() =
    print_int (ft_power 5 2);
    print_char '\n';
    print_int (ft_power 2 4);
    print_char '\n';
    print_int (ft_power 3 0);
    print_char '\n';
    print_int (ft_power 0 5)

let () = main()
