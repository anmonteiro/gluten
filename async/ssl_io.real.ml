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

open Core
open Async
open Async_ssl
module Unix = Core.Unix

type descriptor =
  { reader : Reader.t
  ; writer : Writer.t
  ; closed : unit Ivar.t
  }

module Io :
  Gluten_async_intf.IO
    with type socket = descriptor
     and type addr = Socket.Address.Inet.t = struct
  type socket = descriptor =
    { reader : Reader.t
    ; writer : Writer.t
    ; closed : unit Ivar.t
    }

  type addr = Socket.Address.Inet.t

  let read { reader; _ } bigstring ~off ~len =
    let bigsubstr = Bigsubstring.create ~pos:off ~len bigstring in
    Reader.read_bigsubstring reader bigsubstr

  let writev { writer; _ } iovecs =
    let iovecs_q = Queue.create ~capacity:(List.length iovecs) () in
    let len =
      List.fold
        ~init:0
        ~f:(fun acc { Faraday.buffer; off = pos; len } ->
          Queue.enqueue iovecs_q (Unix.IOVec.of_bigstring ~pos ~len buffer);
          acc + len)
        iovecs
    in
    if Writer.is_closed writer then
      Deferred.return `Closed
    else (
      Writer.schedule_iovecs writer iovecs_q;
      let pipe = Writer.pipe writer in
      Pipe.downstream_flushed pipe >>| fun _ -> `Ok len)

  (* From RFC8446§6.1:
   *   The client and the server must share knowledge that the connection is
   *   ending in order to avoid a truncation attack.
   *
   * Note: In the SSL / TLS runtimes we can't just shutdown one part of the
   * full-duplex connection, as both sides must know that the underlying TLS
   * conection is closing. *)
  let shutdown_receive _ = ()

  let close { reader; writer; closed } =
    Writer.flushed writer >>= fun () ->
    Deferred.all_unit [ Writer.close writer; Reader.close reader ] >>= fun () ->
    Ivar.read closed
end

(* taken from https://github.com/janestreet/async_extra/blob/master/src/tcp.ml *)
let reader_writer_of_sock
    ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size s
  =
  let fd = Socket.fd s in
  ( Reader.create ?buf_len:reader_buffer_size fd
  , Writer.create ?buffer_age_limit ?buf_len:writer_buffer_size fd )

let connect r w =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ()
  |> Deferred.Or_error.ok_exn
  >>= fun _connection ->
  Reader.of_pipe (Info.of_string "httpaf_async_ssl_reader") app_rd
  >>= fun app_reader ->
  Writer.of_pipe (Info.of_string "httpaf_async_ssl_writer") app_wr
  >>| fun (app_writer, `Closed_and_flushed_downstream closed_and_flushed) ->
  let ivar = Ivar.create () in
  don't_wait_for
    ( closed_and_flushed >>= fun () ->
      Reader.close_finished app_reader >>| fun () ->
      Writer.close w >>> Ivar.fill ivar );
  { reader = app_reader; writer = app_writer; closed = Ivar.create () }

(* XXX(anmonteiro): Unfortunately Async_ssl doesn't seem to support configuring
 * the ALPN protocols *)
let make_default_client ?alpn_protocols:_ socket =
  let reader, writer = reader_writer_of_sock socket in
  connect reader writer

let listen ~crt_file ~key_file r w =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.server
    ~crt_file
    ~key_file
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
    ()
  |> Deferred.Or_error.ok_exn
  >>= fun _connection ->
  Reader.of_pipe (Info.of_string "httpaf_async_ssl_reader") app_rd
  >>= fun app_reader ->
  Writer.of_pipe (Info.of_string "httpaf_async_ssl_writer") app_wr
  >>| fun (app_writer, `Closed_and_flushed_downstream closed_and_flushed) ->
  let ivar = Ivar.create () in
  don't_wait_for
    ( closed_and_flushed >>= fun () ->
      Reader.close_finished app_reader >>| fun () ->
      Writer.close w >>> Ivar.fill ivar );
  { reader = app_reader; writer = app_writer; closed = ivar }

(* XXX(anmonteiro): Unfortunately Async_ssl doesn't seem to support configuring
 * the ALPN protocols *)
let make_server ?alpn_protocols:_ ~certfile ~keyfile socket =
  let reader, writer = reader_writer_of_sock socket in
  listen ~crt_file:certfile ~key_file:keyfile reader writer
