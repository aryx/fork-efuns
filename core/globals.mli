
val location : unit -> Efuns.editor
(* should be set once, in main *)
val global_location : Efuns.editor option ref

val with_lock : (unit -> 'a) -> 'a
val error : ('a -> 'b, out_channel, unit) format -> 'a -> unit

val check : bool ref
val debug : bool ref
val debug_graphics : bool ref
val debug_display : bool ref
val debug_init : bool ref

val load_path : string list Options.option_record
val path : string list ref
val efuns_path : string list

val font : string Options.option_record

val displayname: string ref
val xdefaults : string
