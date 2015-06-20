(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.mli                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 22:16:55 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/20 22:16:56 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = Spade | Heart | Diamond | Club

val all : t list (** The list of all values of type t *)

val toString : t -> string (** "S", "H", "D" or "C" *)
val toStringVerbose : t -> string (** "Spade", "Heart", etc *)
