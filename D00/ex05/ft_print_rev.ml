(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 19:47:24 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 21:16:24 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
    let rec loop ln =
        if ln > 0 then
            begin
                print_char (String.get str (ln - 1));
                loop (ln - 1)
            end
        else
            print_char '\n'

    in
    loop (String.length str)

let main () =
    ft_print_rev ("ahmed");
    ft_print_rev ("Hello World !");
    ft_print_rev ("")

let () = main ()
