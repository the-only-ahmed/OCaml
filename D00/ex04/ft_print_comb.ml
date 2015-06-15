(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 19:28:42 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 19:46:45 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
    let rec comb a b c =
        print_int a; print_int b; print_int c;
        if a < 7 then
            print_string ", "
        else
            print_string "\n";
        if c < 9 then
            comb a b (c + 1)
        else
            if b < 8 then
                comb (a + 1) (b + 1) (b + 2)
    in
    comb 0 1 2

let main () =
    ft_print_comb ()

let () = main ()
