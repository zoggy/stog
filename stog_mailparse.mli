
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