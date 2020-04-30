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
    -> [ `Write of Bigstringaf.t Faraday.iovec list | `Yield | `Close of int ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit

  val yield_writer : t -> (unit -> unit) -> unit

  val report_exn : t -> exn -> unit

  val is_closed : t -> bool

  val shutdown : t -> unit
end

type 't runtime = (module RUNTIME with type t = 't)

type impl

val make : 't runtime -> 't -> impl

module Reqd : sig
  type 'reqd t = private
    { reqd : 'reqd
    ; upgrade : impl -> unit
    }
end

module Server : sig
  include RUNTIME

  val create : protocol:'t runtime -> 't -> t

  val upgrade_protocol : t -> impl -> unit

  type 'reqd request_handler = 'reqd Reqd.t -> unit

  val create_upgradable
    :  protocol:'t runtime
    -> create:(('reqd -> unit) -> 't)
    -> 'reqd request_handler
    -> t
end

module Client : sig
  include RUNTIME

  val create : protocol:'t runtime -> 't -> t

  val upgrade_protocol : t -> impl -> unit
end

(* Export upgradable Reqd for convenience *)
type 'reqd reqd = 'reqd Reqd.t = private
  { reqd : 'reqd
  ; upgrade : impl -> unit
  }
