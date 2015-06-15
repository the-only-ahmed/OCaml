(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 19:20:39 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 19:28:02 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () =
    let a = int_of_char 'a' in
        let z = int_of_char 'z' in
            let rec loop alpha_int =
                if alpha_int <= z then
                    let alpha = char_of_int alpha_int in
                        print_char alpha;
                        loop (alpha_int + 1)
    in
    loop a;
    print_char '\n'

let main() =
    ft_print_alphabet()

let () = main ()
