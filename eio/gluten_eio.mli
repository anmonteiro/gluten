module Server : sig
  type socket = Eio.Flow.two_way
  type addr = Eio.Net.Sockaddr.stream

  val create_connection_handler
    :  read_buffer_size:int
    -> domain_mgr:Eio.Domain_manager.t
    -> protocol:'t Gluten.runtime
    -> 't
    -> addr
    -> Eio.Flow.two_way
    -> unit

  val create_upgradable_connection_handler
    :  read_buffer_size:int
    -> protocol:'t Gluten.runtime
    -> create_protocol:(('reqd -> unit) -> 't)
    -> request_handler:(addr -> 'reqd Gluten.Server.request_handler)
    -> domain_mgr:Eio.Domain_manager.t
    -> addr
    -> Eio.Flow.two_way
    -> unit
end

module Client : sig
  type socket = Eio.Flow.two_way

  type t =
    { connection : Gluten.Client.t
    ; socket : socket
    }

  val create
    :  domain_mgr:Eio.Domain_manager.t
    -> read_buffer_size:int
    -> protocol:'t Gluten.runtime
    -> 't
    -> Server.socket
    -> t

  val upgrade : t -> Gluten.impl -> unit
  val shutdown : t -> unit
  val is_closed : t -> bool
  val socket : t -> socket
end
