(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 22:39:17 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/24 22:39:18 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* Hash function -> djb2 *)

module StringHash =
        struct
          type t = String.t
          let equal i j = i = j
          let hash i =
			let maxx = (String.length i) - 1 in
			let rec loop idx hh =
				if idx = maxx then hh
				else loop (idx + 1) (((hh lsl 5) + hh) + (int_of_char i.[idx]))
			in
			loop 0 5381
        end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
