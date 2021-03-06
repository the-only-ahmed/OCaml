(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 22:35:43 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/18 22:35:45 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec crossover l1 l2 =
   let rec check_existance x liste = match liste with
      | [] -> []
      | hd::tl -> if (x = hd) then [x] else check_existance x tl
   in match l1 with
   | [] -> []
   | hd::tl -> (check_existance hd l2) @ (crossover tl l2)

let () =

   let rec toto liste = match liste with
      | [] -> ()
      | hd::tl -> print_endline(hd) ; toto tl

      in toto (crossover ["coucou"; "salut"; "tata"] ["coucou"; "tata"] );
      print_char('\n');
      toto (crossover ["titi"; "toto"; "tutu"] ["tutu"; "titi"] );
      print_char('\n');
      toto (crossover ["hey"] [] )
