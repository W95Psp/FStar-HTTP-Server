module HTTP.Server.Core

open FStar.Tactics.Tcp
open FStar.Tactics
open FStar.String

module Str = FStar.String
module L = FStar.List.Tot

open HTTP.Server.Helpers
open HTTP.Server.Types
open HTTP.Server.Types.Parse
open HTTP.Server.Types.Print
open HTTP.Server.Types.Helpers

let rec loop
  #exit #s
  (handler: s -> httpReq -> s * either exit httpRes)
  (on_error: s -> (error: string) -> s * either exit httpRes)
  (state: s)
  socket: Tac exit =
  let socket' = accept socket in
  let continue v: Tac _ =
    let state', res = v in
    match res with
    | Inl exit -> close socket'; close socket; exit
    | Inr res -> let _ = send socket' (printHttpRes res) in 
                loop handler on_error state' socket
  in
  match recv socket' 2048 with
  | (Some err, None) -> continue (on_error state err)
  | (None, Some body) -> (
    ( match parseHttpReq body with
    | Some req -> continue (handler state req)
    | None     -> 
        let res = mkHttpRes200Html "Error" in
        let _ = send socket' (printHttpRes res) in 
        loop handler on_error  state socket
    )
  )

let startHTTPServer #s #exit
  (addr: string) (port: nat)
  (handler: s -> httpReq -> s * either exit httpRes)
  (handlerErr: s -> (err: string) -> s * either exit httpRes)
  (initialState: s)
  : Tac exit
  = let socket = listen addr port in
    loop handler handlerErr initialState socket

let startHTTPServer' #st
  (handler: st -> httpReq -> st * httpRes)
  (initialState: st)
  : Tac (option string)
  = startHTTPServer "0.0.0.0" 8080
    (fun s req ->
      let s, res = handler s req in
      s, Inr res
    )
    (fun s err -> s, (Inl (Some err)))
    initialState

let example () =
  startHTTPServer'
  (fun s req ->
    let line = req.httpReqLine in
    let method = line.httpRequestLineMethod in
    let uri = line.httpRequestLineRequestURI in
    let headers = getHeader req.httpReqHeaders in
    let body = req.httpReqBody in
    let contentType = headers "Content-Type" in
    let contentType = fromOption contentType "" in
    let contentType = parseContentType contentType in
    let main s inner =
      s,
      mkHttpRes200Html
      ("<h1>Hi there!</h1>
        <form method=POST action=\"/\">
          <input name=username>
          <input name=password>
          <input type=submit>
        </form><br/><ul>"^
      String.concat "\n" (L.map (fun (user, pass) -> 
        "<li><b>" ^ user ^ "</b> " ^ pass ^ "</li>"
      ) s)
      ^"</ul><br/><br/>" ^ inner)
    in
    match method with
    | POST ->
      begin
        match contentType with
        | Application_XWwwFormUrlencoded ->
          // TODO: rename getHeader, it have broader use
          let f = getHeader (parseUrlEncodedString body) in
          begin match f "username", f "password" with
          | Some u, Some p -> main ((u,p)::s) "User registrated!"
          | _, _ -> main s "Error form"
          end
        | _ -> main s ""
      end
    | GET -> main s ""
    | _ -> main s "Unsupported method"
  )
  ([] <: list (string * string))

let _ = assert (true) by (
    let x = example () in
    ()
  )




