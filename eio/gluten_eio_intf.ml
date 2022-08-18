module type IO = sig
  type socket

  val read
    :  socket
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> [ `Eof | `Ok of int ]
  (** The region [(off, off + len)] is where read bytes can be written to *)

  val writev
    :  socket
    -> Bigstringaf.t Faraday.iovec list
    -> [ `Closed | `Ok of int ]

  val shutdown_receive : socket -> unit
  val close : socket -> unit
end

module type Server = sig
  type socket

  val create_connection_handler
    :  read_buffer_size:int
    -> domain_mgr:Eio.Domain_manager.t
    -> protocol:'t Gluten.runtime
    -> 't
    -> Eio.Net.Sockaddr.stream
    -> socket
    -> unit

  val create_upgradable_connection_handler
    :  read_buffer_size:int
    -> protocol:'t Gluten.runtime
    -> create_protocol:(('reqd -> unit) -> 't)
    -> request_handler:
         (Eio.Net.Sockaddr.stream -> 'reqd Gluten.Server.request_handler)
    -> domain_mgr:Eio.Domain_manager.t
    -> Eio.Net.Sockaddr.stream
    -> socket
    -> unit
end

module type Client = sig
  type socket
  type t

  val create
    :  Eio.Stdenv.t
    -> sw:Eio.Switch.t
    -> read_buffer_size:int
    -> protocol:'t Gluten.runtime
    -> 't
    -> socket
    -> t

  val upgrade : t -> Gluten.impl -> unit
  val shutdown : t -> unit
  val is_closed : t -> bool
  val socket : t -> socket
end
