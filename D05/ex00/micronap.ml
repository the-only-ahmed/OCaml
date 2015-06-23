(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 13:26:51 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/22 14:51:15 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let my_sleep () = Unix.sleep 1

let read () =
    if (Array.length Sys.argv <> 2) then
        failwith "Not enough arguments"
    else
        begin
            try
                let a = (int_of_string Sys.argv.(1)) in
                    for i = 1 to a do
                       my_sleep ()
                    done;
            with
                | Not_found -> ignore ("Not_found")
                | Failure err -> ignore (err)
                | _ -> ignore ("WTF !!!")
        end

let () =
    try read() with
        | _ -> ()

(*ocamlopt unix.cmxa micronap.ml*)
