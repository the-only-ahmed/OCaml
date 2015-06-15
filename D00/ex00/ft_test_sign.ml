(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 16:57:03 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 18:46:28 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign x =
    if x < 0
    then print_endline "negative"
    else print_endline "positive"

let main () =
    ft_test_sign 42;
    ft_test_sign 0;
    ft_test_sign (-42)

let () = main()
