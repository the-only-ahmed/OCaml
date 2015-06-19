(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 23:21:11 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/19 23:21:18 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let size t =
   let rec calculate t2 = match t2 with
   | Nil -> 0
   | Node(v, l, r) -> (calculate l ) + (calculate r ) + 1
      in calculate t

let height t =
   let max i i2 =
      if i < i2 then i2
      else i
   in
   let rec calculate t2 = match t2 with
   | Nil -> 0
   | Node(v, l, r) -> (max (calculate l ) (calculate r)) + 1
      in calculate t

let draw_square x y size =
   Graphics.moveto (x - (size / 2)) (y - (size / 2));
   Graphics.lineto (x + (size / 2)) (y - (size / 2));
   Graphics.lineto (x + (size / 2)) (y + (size / 2));
   Graphics.lineto (x - (size / 2)) (y + (size / 2));
   Graphics.lineto (x - (size / 2)) (y - (size / 2))

let draw_tree_node t =
   let rec draw_node t2 x y = match t2 with
   | Nil -> draw_square x y 50; Graphics.moveto (x - 20) y; Graphics.draw_string "Nil"
   | Node(v, l, r) -> draw_square x y 50; Graphics.moveto (x - 20) y; Graphics.draw_string v; draw_node l (x+ 100) (y + 50); draw_node r (x + 100) (y - 50); Graphics.moveto (x+25) (y); Graphics.lineto (x+75) (y+50); Graphics.moveto (x+25) (y); Graphics.lineto (x+75) (y-50)
      in draw_node t 100 250
let main () =
   Graphics.open_graph " 800x600";
   let t = Node ("toto", Node("titi", Node("tata", Nil, Nil), Nil), Node("titi2",Nil, Node("tutu", Nil, Nil)))
   in
   print_string ("SIZE T1= ");
   print_int (size t);
   print_char '\n';
   print_string ("HEIGHT T1= ");
   print_int (height t);
   print_char '\n';
   draw_tree_node(t);
   ignore(Graphics.read_key());
   Graphics.clear_graph();
   let t2 = Node ("toto", Nil, Node("titi",Nil, Node("tutu", Node("ahah", Nil, Nil), Nil)))
   in
   print_string ("SIZE T2= ");
   print_int (size t2);
   print_char '\n';
   print_string ("HEIGHT T2= ");
   print_int (height t2);
   print_char '\n';
   draw_tree_node(t2);
   ignore(Graphics.read_key())

let () = main ()
