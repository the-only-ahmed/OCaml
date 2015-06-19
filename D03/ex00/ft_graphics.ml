(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 23:20:19 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/19 23:20:47 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
   Graphics.moveto (x - (size / 2)) (y - (size / 2));
   Graphics.lineto (x + (size / 2)) (y - (size / 2));
   Graphics.lineto (x + (size / 2)) (y + (size / 2));
   Graphics.lineto (x - (size / 2)) (y + (size / 2));
   Graphics.lineto (x - (size / 2)) (y - (size / 2))

let draw_tree_node t =
   let rec draw_node t2 x y = match t2 with
   | Nil -> draw_square x y 50; Graphics.moveto x y; Graphics.draw_string "Nil"
   | Node(v, l, r) -> draw_square x y 50; Graphics.moveto x y; Graphics.draw_string v; draw_node l (x+ 100) (y + 50); draw_node r (x + 100) (y - 50); Graphics.moveto (x+25) (y); Graphics.lineto (x+75) (y+50); Graphics.moveto (x+25) (y); Graphics.lineto (x+75) (y-50)
      in draw_node t 400 300
let main () =
   Graphics.open_graph " 800x600";
   draw_tree_node(Node("toto", Nil, Nil));
   ignore(Graphics.read_key());
   Graphics.clear_graph();
   draw_tree_node(Nil);
   ignore(Graphics.read_key())
let () = main ()
