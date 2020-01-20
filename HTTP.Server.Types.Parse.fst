module HTTP.Server.Types.Parse

open HTTP.Server.Types
open HTTP.Server.Helpers
module L = FStar.List.Tot

let parseHttpMethod (s: string): option httpMethod =
  match trim s with
  | "GET" -> Some GET         | "HEAD" -> Some HEAD
  | "POST" -> Some POST       | "PUT" -> Some PUT
  | "DELETE" -> Some DELETE   | "CONNECT" -> Some CONNECT
  | "OPTIONS" -> Some OPTIONS | "TRACE" -> Some TRACE
  | _ -> None

let parseHttpVersion s
  = match trim s with
  | "HTTP/1.1" -> Http1_1
  | _ -> HttpUnknown

let parseHttpRequestLine l =
  let bind = option_bind in
  match String.split [' '] (trim l) with
  | [method;uri;version] ->
      method <-- parseHttpMethod method;
      Some
      ({ httpRequestLineMethod = method
       ; httpRequestLineRequestURI = uri
       ; httpRequestLineHttpVersion = parseHttpVersion version
       })
  | x -> None



let parseHttpHeader h =
  match String.split [':'] h with
  | key::tl -> Some (key, String.concat ":" tl)
  | _ -> None

let rec splitWhen keep (split: 'a -> bool) (l: list 'a)
  : list 'a * list 'a = 
  match l with
  | [] -> [], []
  | hd::tl -> if split hd
            then (if keep then [hd] else []), tl
            else (
              let l, r = splitWhen keep split tl in
              hd::l, r
            )

let parseHttpReq (s: string): option httpReq
  = let bind #t #u = option_bind #t #u in
    match String.split ['\n'] s with
  | reqLine::rest -> 
        reqLine <-- parseHttpRequestLine reqLine;
        let headers, body = splitWhen false (fun x -> trim x = "") rest in
        let body = String.concat "\n" body in
        headers <-- L.fold_left (fun (l: option (list (string * string))) h -> 
          l <-- l;
          h <-- parseHttpHeader h;
          let k, v = h in Some (set k v l)
        ) (Some []) headers;
        Some ( { httpReqLine = reqLine
               ; httpReqHeaders = headers
               ; httpReqBody = body
               }
             )
  | _ -> None

let noBlank = L.filter (fun x -> x <> "")

let parseContentType (s: string): contentType // TODO
  = match noBlank (String.split [';'] (trim s)) with
  | [] -> Text_Plain Utf8
  | hd::tl -> (
          match L.map trim (String.split ['/'] (trim hd)) with
          | ["text";"plain"] -> Text_Plain Utf8
          | ["text";"html"] -> Text_Html Utf8
          | ["application";"json"] -> Application_Json
          | ["application";"x-www-form-urlencoded"] -> Application_XWwwFormUrlencoded
          | _ -> Text_Plain Utf8
          )
