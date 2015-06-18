(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 22:35:52 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/18 22:35:53 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power a b =
   if (b <= 0)
      then 1
   else a * (ft_power a (b - 1))

let inverse x =
   if (x = 0) then
      1
   else
      0

let rec append l rep = match l with
    | [] -> rep
    | a::b -> a::(append b rep)

let create_int len =
   let str = [] in
   let rec conc str n =
      if (n > 0) then
         conc (append str [0]) (n-1)
      else
         str
   in conc str len



let rec print_liste2 liste = match liste with
      | [] -> ()
      | hd::tl -> print_int (hd); print_liste2 tl

let rec print_liste liste = match liste with
   | [] -> ()
   | hd::[] -> print_liste2 (hd); print_char '\n'
   | hd::tl -> print_liste2 (hd); print_char ' '; print_liste tl

let rec list_equal v1 v2 = match v1, v2 with
   | [], []        -> true
   | [], _ | _, [] -> false
   | x::xs, y::ys  -> x = y && list_equal xs ys

let generate_liste liste index =
   let rec creating liste index acc acc2 = match liste with
   | [] -> []
   | hd::tail -> if (index = acc2) then  (append [inverse hd] (creating tail index acc (acc2 + 1)))
      else (append [hd] (creating tail index acc (acc2 + 1)))
   in creating liste index [] 0

   let rec ft_recur2 ll1 cur = match ll1 with
      | [] -> true
      | hd::tl -> if ((list_equal hd cur) = true) then false else ft_recur2 tl cur

let rec ft_recur l1 last n =
   let current = generate_liste last (n-1)
      in

      if (ft_recur2 l1 current) = false then
         begin
            if ((n-1) >= 0) then
               ft_recur l1 last (n-1)
            else
               []
         end
      else
         current

let rec last_l liste = match liste with
   | [] -> []
   | hd::[] -> hd
   | _::tl -> last_l tl


  let gray n =
   (* let x = ft_power 2 n in *)
   let liste = [create_int n] in
   let rec ft_gray l1 n2 =
      let queue = (last_l l1) in
      let adding = (ft_recur l1 queue n2) in
      if (list_equal adding []) = true then
         l1
      else
         begin
         let toto = (append l1 [adding]) in
         ft_gray toto n2
         end
   in ft_gray liste n

let () =
   print_liste (gray 0);
   print_liste (gray 1);
   print_liste (gray 2);
   print_liste (gray 3);
   print_liste (gray 4);
