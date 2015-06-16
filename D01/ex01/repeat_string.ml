(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 11:59:20 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/16 17:03:04 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_string ?(str="x") n =
    let rec loop n str old =
        if (n < 0) then
            "Error"
        else
            begin
                if (n > 0) then
                    loop (n - 1) (str ^ old) old
                else
                    str
            end
    in
    loop n "" str


let () =
    print_endline (repeat_string (-1));
    print_endline (repeat_string 0);
    print_endline (repeat_string 2);
    print_endline (repeat_string ~str:"toto" 1);
    print_endline (repeat_string ~str:"a" 5);
    print_endline (repeat_string ~str:"ahmed" 3)
