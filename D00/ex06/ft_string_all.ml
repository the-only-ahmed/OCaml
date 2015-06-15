(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 20:52:05 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 21:19:08 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_digit c =
    c >= '0' && c <= '9'

let ft_string_all func param =
    let rec loop ln =
        if ln > 0 then
            if func (String.get param ln) = false then
                false
            else
                loop (ln - 1)
        else
            true

    in
    loop (String.length param - 1)

let main() =
    print_endline (string_of_bool (ft_string_all is_digit "943289774523u8"));
    print_endline (string_of_bool (ft_string_all is_digit "9432897745238"))

let () = main ()
