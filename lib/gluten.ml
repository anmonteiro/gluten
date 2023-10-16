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

  val next_write_operation :
     t
    -> [ `Write of Bigstringaf.t Faraday.iovec list | `Yield | `Close of int ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
  val is_closed : t -> bool
  val shutdown : t -> unit
end

type 't runtime = (module RUNTIME with type t = 't)
type impl = Runtime : 't runtime * 't -> impl

let make runtime t = Runtime (runtime, t)

module Runtime = struct
  type t = { mutable connection : impl }

  let create ~protocol t' = { connection = Runtime (protocol, t') }

  let upgrade_protocol t protocol' =
    let { connection = Runtime ((module P), t') } = t in
    t.connection <- protocol';
    P.shutdown t'

  let next_read_operation { connection = Runtime ((module P), t) } =
    P.next_read_operation t

  let read { connection = Runtime ((module P), t) } = P.read t
  let read_eof { connection = Runtime ((module P), t) } = P.read_eof t
  let yield_reader { connection = Runtime ((module P), t) } = P.yield_reader t

  let next_write_operation { connection = Runtime ((module P), t) } =
    P.next_write_operation t

  let report_write_result { connection = Runtime ((module P), t) } =
    P.report_write_result t

  let yield_writer { connection = Runtime ((module P), t) } = P.yield_writer t
  let report_exn { connection = Runtime ((module P), t) } = P.report_exn t
  let shutdown { connection = Runtime ((module P), t) } = P.shutdown t
  let is_closed { connection = Runtime ((module P), t) } = P.is_closed t
end

module Reqd = struct
  type 'reqd t =
    { reqd : 'reqd
    ; upgrade : impl -> unit
    }

  let create reqd upgrade = { reqd; upgrade }
end

module Client = Runtime

module Server = struct
  include Runtime

  type 'reqd request_handler = 'reqd Reqd.t -> unit

  let create_upgradable :
      type t' reqd.
      protocol:t' runtime
      -> create:((reqd -> unit) -> t')
      -> reqd request_handler
      -> t
    =
   fun ~protocol ~create request_handler ->
    let rec t =
      lazy { connection = Runtime (protocol, create request_handler') }
    and request_handler' reqd =
      let reqd' = Reqd.create reqd (upgrade_protocol (Lazy.force t)) in
      request_handler reqd'
    in
    Lazy.force t
end

type 'reqd reqd = 'reqd Reqd.t = private
  { reqd : 'reqd
  ; upgrade : impl -> unit
  }

module Buffer = struct
  type t =
    { buffer :
        (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    ; mutable off : int
    ; mutable len : int
    ; cap : int
    }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0; cap = size }

  let compress t =
    if t.len = 0
    then (
      t.off <- 0;
      t.len <- 0)
    else if t.off > 0
    then (
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0)

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    n

  let put t ~f k =
    compress t;
    let off = t.off + t.len in
    let len = t.cap - t.len - t.off in
    f t.buffer ~off ~len (fun n ->
      t.len <- t.len + n;
      k n)
end
