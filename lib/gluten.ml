(*----------------------------------------------------------------------------
 *  Copyright (c) 2019-2020 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module type RUNTIME = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int

  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation
    :  t
    -> [ `Write of Bigstringaf.t Httpaf.IOVec.t list | `Yield | `Close of int ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit

  val yield_writer : t -> (unit -> unit) -> unit

  val report_exn : t -> exn -> unit

  val is_closed : t -> bool

  val shutdown : t -> unit
end

type 't runtime = (module RUNTIME with type t = 't)

type impl = Runtime : 't runtime * 't -> impl

let make runtime t = Runtime (runtime, t)

module Reqd = struct
  type 'reqd t =
    { reqd : 'reqd
    ; upgrade : impl -> unit
    }

  let create reqd upgrade = { reqd; upgrade }
end

module Upgradable_connection = struct
  type t = { mutable io_handler : impl }

  let upgrade_protocol t protocol' =
    let { io_handler = Runtime ((module P), t') } = t in
    t.io_handler <- protocol';
    P.shutdown t'

  let next_read_operation { io_handler = Runtime ((module P), t) } =
    P.next_read_operation t

  let read { io_handler = Runtime ((module P), t) } = P.read t

  let read_eof { io_handler = Runtime ((module P), t) } = P.read_eof t

  let yield_reader { io_handler = Runtime ((module P), t) } = P.yield_reader t

  let next_write_operation { io_handler = Runtime ((module P), t) } =
    P.next_write_operation t

  let report_write_result { io_handler = Runtime ((module P), t) } =
    P.report_write_result t

  let yield_writer { io_handler = Runtime ((module P), t) } = P.yield_writer t

  let report_exn { io_handler = Runtime ((module P), t) } = P.report_exn t

  let shutdown { io_handler = Runtime ((module P), t) } = P.shutdown t

  let is_closed { io_handler = Runtime ((module P), t) } = P.is_closed t
end

module Server = struct
  include Upgradable_connection

  type 'reqd request_handler = 'reqd Reqd.t -> unit

  let create
      : type t' reqd.
        protocol:t' runtime
        -> create:((reqd -> unit) -> t')
        -> reqd request_handler
        -> t
    =
   fun ~protocol ~create request_handler ->
    let rec t =
      lazy { io_handler = Runtime (protocol, create request_handler') }
    and request_handler' reqd =
      let reqd' = Reqd.create reqd (upgrade_protocol (Lazy.force t)) in
      request_handler reqd'
    in
    Lazy.force t
end

module Client = struct
  include Upgradable_connection

  let create ~protocol t' = { io_handler = Runtime (protocol, t') }
end
