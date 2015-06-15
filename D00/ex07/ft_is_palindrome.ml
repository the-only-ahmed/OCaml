(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 21:19:45 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/15 22:28:15 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome str =
    let rec loop ln i =
        if (i < ln) then
            begin
                if (String.get str i) <> (String.get str ln) then
                    false
                else
                    loop (ln - 1) (i + 1)
            end
        else
            true

    in
    if (String.length str mod 2) <> 1 then
        false
    else
        loop ((String.length str) - 1) 0

let main() =
    print_endline (string_of_bool (ft_is_palindrome("radar")));
    print_endline (string_of_bool (ft_is_palindrome("radari")));
    print_endline (string_of_bool (ft_is_palindrome("radbr")));
    print_endline (string_of_bool (ft_is_palindrome("abcde")))

let () = main ()
