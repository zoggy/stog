(** *)

type contents_kind = Text | Html
type date = int * int * int (** day 1..31 * month 1..12 * year *)
type time = Unix.tm ;;

type message = {
    mes_time : time ;
    mes_subject : string ;
    mes_from : string ;
    mes_to : string list ;
    mes_body : string ;
  }

type 'a tree = 'a * 'a list ;;

type article =
  { art_id : string;
    art_kind : contents_kind ;
    art_body : string ;
    art_date : date ;
    art_keywords : string list ;
    art_topics : string list ;
    art_title : string ;
    art_location : string ;
    art_comments : message tree ;
  }

