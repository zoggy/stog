(** Handling HTML5. *)

(** Prevent tags to self-close (i.e. <tag/>), except
  for {{:http://www.w3.org/TR/html5/syntax.html#void-elements}specifc HTML5 void tags}. *)
val hack_self_closed : Xtmpl.tree -> Xtmpl.tree