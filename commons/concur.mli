(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*                                                                     *)
(*                 mail: fabrice.le_fessant@inria.fr                   *)
(*                 web:  http://www-sor.inria.fr/~lefessan             *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(*         File written in the Objective-CAML language                 *)

val iterator : int ref -> unit
val poll : unit -> bool
module Mutex :
  sig
    type t
    val create : unit -> t
    val lock : t -> unit
    val try_lock : t -> bool
    val unlock : t -> unit
  end
module Condition :
  sig
    type t
    val create : unit -> t
    val wait : t -> Mutex.t -> unit
    val signal : t -> unit
    val broadcast : t -> unit
  end
module Thread : sig 
    val add_reader : Unix.file_descr -> (unit -> unit) -> unit 
    val remove_reader : Unix.file_descr -> unit
    val add_timer : float -> (unit -> unit) -> unit
    val fork : unit -> int
  end
module ThreadUnix :
  sig
    val execv : string -> string array -> unit
    val execve : string -> string array -> string array -> unit
    val execvp : string -> string array -> unit
    val wait : unit -> int * Unix.process_status
    val waitpid : Unix.wait_flag list -> int -> int * Unix.process_status
    val system : string -> Unix.process_status
    val read : Unix.file_descr -> string -> int -> int -> int
    val write : Unix.file_descr -> string -> int -> int -> int
    val pipe : unit -> Unix.file_descr * Unix.file_descr
    val open_process_out : string -> out_channel
    val open_process : string -> in_channel * out_channel
    val sleep : int -> unit
    val socket :
      Unix.socket_domain -> Unix.socket_type -> int -> Unix.file_descr
(*
    val socketpair :
      Unix.socket_domain ->
      Unix.socket_type -> int -> Unix.file_descr * Unix.file_descr
*)
    val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
    val connect : Unix.file_descr -> Unix.sockaddr -> unit
    val recv :
      Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int
    val recvfrom :
      Unix.file_descr ->
    string -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr
    val send :
      Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int
    val sendto :
      Unix.file_descr ->
    string -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int
    val open_connection : Unix.sockaddr -> in_channel * out_channel
    val select : Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list -> float ->  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list 
(*
    val establish_server :
      (in_channel -> out_channel -> 'a) -> Unix.sockaddr -> unit
*)
  end
  
(* external ioctl: Unix.file_descr -> int -> string -> 'a -> unit = "ml2c_ioctl" *)
