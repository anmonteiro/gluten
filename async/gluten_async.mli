(*----------------------------------------------------------------------------
 *  Copyright (c) 2018 Inhabited Type LLC.
 *  Copyright (c) 2019-2020 Antonio N. Monteiro.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the author nor the names of his contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
 *  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Async

module Server : sig
  include
    Gluten_async_intf.Server
      with type 'a socket =
        ([ `Active ], ([< Socket.Address.t ] as 'a)) Socket.t

  module SSL : sig
    include Gluten_async_intf.Server with type 'a socket = 'a Ssl_io.descriptor

    val create_default :
       ?alpn_protocols:string list
      -> certfile:string
      -> keyfile:string
      -> ([< Socket.Address.t ] as 'a)
      -> ([ `Active ], 'a) Socket.t
      -> 'a socket Deferred.t
  end

  (* module TLS : sig include Gluten_async_intf.Server with type socket =
     Tls_io.descriptor and type addr := Socket.Address.t

     val create_default : ?alpn_protocols:string list -> certfile:string ->
     keyfile:string -> 'b -> ([ `Active ], 'a) Socket.t -> socket Deferred.t
     end *)
end

module Client : sig
  include
    Gluten_async_intf.Client
      with type 'a socket =
        ([ `Active ], ([< Socket.Address.t ] as 'a)) Socket.t

  module SSL : sig
    include Gluten_async_intf.Client with type 'a socket = 'a Ssl_io.descriptor

    val create_default :
       ?alpn_protocols:string list
      -> ([ `Active ], [< Socket.Address.t ]) Socket.t
      -> [< Socket.Address.t ] socket Deferred.t
  end

  module TLS : sig
    include Gluten_async_intf.Client with type 'a socket = 'a Tls_io.descriptor

    val create_default :
       ?alpn_protocols:string list
      -> ([ `Unconnected ], ([< Socket.Address.t ] as 'a)) Socket.t
      -> 'a Tcp.Where_to_connect.t
      -> 'a socket Deferred.t
  end
end
