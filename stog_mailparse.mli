(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(*i==m=[Mail.Parse]=1.0=t==*)
(** Parsing of e-mail messages, including attachments.
@author Xavier Leroy
@version 1.0
@cgname Mail.Parse*)
(***********************************************************************)
(*                                                                     *)
(*                 SpamOracle -- a Bayesian spam filter                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  This file is distributed under the terms of the   *)
(*  GNU Public License version 2, http://www.gnu.org/licenses/gpl.txt  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: smailparse.mli 41 2005-10-13 14:42:53Z guesdon $ *)

(** Parsing of e-mail messages, including attachments.

@cgname Mail.Parse
@version 1.0
@author Xavier Leroy
*)

type message =
  { headers: (string * string) list;
    body: string;
    parts: message list }
  (** The type of parsed e-mail messages.
    - [headers] is an A-list of pairs [(header-name, header-content)].
      [header-name] is lowercased and includes [:], e.g. [subject:].
    - [body] is the body of the message.  Base64 and quoted-printable
      encodings are already decoded.  For multipart messages, [body]
      is the initial blurb before the first part.
    - [parts] is empty except for multipart messages, in which case
      it lists all parts, recursively represented as messages. *)

val parse_message: string -> message
  (** Parse the given textual message and return its structure. *)

val header: string -> message -> string
  (** [header h msg] returns the contents of header named [h]
      in message [msg], or the empty string if this header is missing.
      Remember that header names are lowercased and include the final [:],
      e.g. [subject:]. *)

(*/i==m=[Mail.Parse]=1.0=t==*)

val messages_from_dir : string -> message list

val build_message_tree : message list -> Stog_types.message Stog_types.tree list  