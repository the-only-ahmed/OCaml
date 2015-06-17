(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 12:31:23 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/17 12:49:42 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f i n =
    if (n < i) then
        nan
    else
        begin
            let rec sum f i n s =
                if (n < i) then
                    s
                else
                    sum f (i + 1) n (s +. (f i))
            in
            sum f i n 0.0
        end

let () =
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10)
