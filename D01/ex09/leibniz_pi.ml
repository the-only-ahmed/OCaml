(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 12:50:37 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/17 12:56:11 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi delta =
    let ft_power x n =
        if (n mod 2) = 0 then
            1
        else
            -1
    in
    let rec ft_sum f acc nb =
        if (delta < 0.) then
            -1
        else if (4. *. acc) < ((4. *. (atan 1.)) +. delta)
            && (4. *. acc) > ((4. *. (atan 1.)) -. delta) then
                nb
        else
            ft_sum f (acc +. (f (nb))) (nb + 1)
    in
    ft_sum (fun i -> (float_of_int (ft_power (-1) i))
        /. (float_of_int (2 * i) +. 1.)) 0.0 0

let () =
    print_int (leibniz_pi 0.0000001);
    print_char '\n'
