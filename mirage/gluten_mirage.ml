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

open Lwt.Infix

module Make_IO (Flow : Mirage_flow.S) : Gluten_lwt.IO with type addr = unit =
struct
  module Buffered_flow = struct
    module Mirage = Flow

    (* type data = (Cstruct.t Mirage_flow.or_eof, Mirage.error) result *)

    type t = {
      flow : Flow.flow;
      mutable buf : Cstruct.t;
    }

    (* let create flow = { flow; buf = Cstruct.empty } *)

    let writev v bufs =
      let len = Cstruct.lenv bufs in
      let data = Cstruct.create_unsafe len in
      let _, _ = Cstruct.fillv ~src:bufs ~dst:data in
      Mirage.write v.flow data

    (* let close v = Mirage.close v.flow *)

    let read v len =
      let trunc buf =
        match Cstruct.length buf > len with
        | false ->
          buf
        | true ->
          let head, rest = Cstruct.split buf len in
          v.buf <- rest;
          head
      in
      let buffered_data =
        match Cstruct.is_empty v.buf with
        | true ->
          None
        | false ->
          let buf = v.buf in
          v.buf <- Cstruct.empty;
          Some (Ok (`Data (trunc buf)))
      in
      match buffered_data with
      | Some data ->
        Lwt.return data
      | None -> (
        Flow.read v.flow >|= fun data ->
        assert (Cstruct.is_empty v.buf);
        match data with Ok (`Data buf) -> Ok (`Data (trunc buf)) | x -> x)
  end

  type socket = Buffered_flow.t = {
    flow : Flow.flow;
    mutable buf : Cstruct.t;
  }

  type addr = unit

  let shutdown t = Flow.close t.flow
  let shutdown_receive t = Lwt.async (fun () -> shutdown t)
  let close = shutdown

  let read t bigstring ~off ~len =
    Lwt.catch
      (fun () ->
        Buffered_flow.read t len >|= function
        | Ok (`Data buf) ->
          Bigstringaf.blit buf.buffer ~src_off:buf.off bigstring ~dst_off:off
            ~len:buf.len;
          `Ok buf.len
        | Ok `Eof ->
          `Eof
        | Error error ->
          failwith (Format.asprintf "%a" Buffered_flow.Mirage.pp_error error))
      (fun exn -> shutdown t >>= fun () -> Lwt.fail exn)

  let writev t iovecs =
    let cstruct_iovecs =
      List.map
        (fun { Faraday.buffer; off; len } ->
          Cstruct.of_bigarray ~off ~len buffer)
        iovecs
    in
    Lwt.catch
      (fun () ->
        Buffered_flow.writev t cstruct_iovecs >|= fun x ->
        match x with
        | Ok () ->
          `Ok (Cstruct.lenv cstruct_iovecs)
        | Error `Closed ->
          `Closed
        | Error other_error ->
          raise
            (Failure
               (Format.asprintf "%a" Buffered_flow.Mirage.pp_write_error
                  other_error)))
      (fun exn -> shutdown t >>= fun () -> Lwt.fail exn)
end

module Server (F : Mirage_flow.S) = Gluten_lwt.Server (Make_IO (F))
module Client (F : Mirage_flow.S) = Gluten_lwt.Client (Make_IO (F))
