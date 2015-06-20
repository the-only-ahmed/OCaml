(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Values.mli                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:17:09 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:17:10 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

(** The list of all values of type t *)
val all : t list

(** Interger representation of a card value, from 1 for T2 to 13 for As *)
val toInt : t -> int

(** returns "2", ..., "10", "J", "Q", "K" or "A" *)
val toString : t -> string
(** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
val toStringVerbose : t -> string

(** Returns the next value, or calls invalid_arg if argument is As *)
val next : t -> t
(** Returns the previous value, or calls invalid_arg if argument is T2 *)
val previous : t -> t
