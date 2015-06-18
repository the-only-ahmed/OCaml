(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 22:35:58 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/18 22:35:59 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec list_to_string liste = match liste with
    | [] -> ""
    | hd::tl -> (string_of_int hd) ^ (list_to_string tl)

let sequence n =
    let rec test a acc res =
      let loop liste =
         let rec do_work x tail acc =  match tail with
            | [] -> [acc; x]
            | hd::tl -> if hd = x then do_work x tl (acc + 1) else [acc; x] @ (do_work hd tail 0)
         in match liste with
            | [] -> []
            | hd::_ -> do_work hd liste 0
      in
      if (a = 0) then
        list_to_string acc
      else
        test (a - 1) (loop acc) (list_to_string acc)
   in
   if (n < 0) then
       ""
   else
       test n [1] ""


let() =
       print_endline (sequence (-1));
       print_endline (sequence 0);
       print_endline (sequence 1);
       print_endline (sequence 15)
