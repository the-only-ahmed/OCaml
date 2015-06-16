(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 11:48:03 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/16 11:58:14 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x n =
    let rec loop n str =
        if (n < 0) then
            "Error"
        else
            begin
                if (n > 0) then
                    loop (n - 1) (str ^ "x")
                else
                    str
            end
    in
    loop n ""

let main() =
    print_endline (repeat_x (-1));
    print_endline (repeat_x 0);
    print_endline (repeat_x 1);
    print_endline (repeat_x 5)

let () = main()
