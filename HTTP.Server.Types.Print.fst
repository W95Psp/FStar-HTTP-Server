module HTTP.Server.Types.Print

open HTTP.Server.Types
open HTTP.Server.Helpers
module L = FStar.List.Tot

let printHttpMethod = function
  | GET -> "GET"       | HEAD -> "HEAD"       | POST -> "POST"       | PUT -> "PUT"
  | DELETE -> "DELETE" | CONNECT -> "CONNECT" | OPTIONS -> "OPTIONS" | TRACE -> "TRACE"

let printHttpVersion
  = function
  | Http1_1 -> "HTTP/1.1"
  | _ -> "HTTP/1.1"


let printHttpResponseStatusLine s = 
    printHttpVersion s.httpResponseVersion
  ^ " "
  ^ string_of_int s.httpResponseStatusCode
  ^ " "
  ^ s.httpResponseReasonPhrase ^ "\n"

let printHttpRes re =
    printHttpResponseStatusLine re.httpResStatus
  ^ String.concat "" (L.map (fun (k,v) -> k ^ ": " ^ v ^ "\n") re.httpResHeaders)
  ^ "\n"
  ^ re.httpResBody

