module HTTP.Server.Types.Helpers

open HTTP.Server.Types
open HTTP.Server.Helpers
module L = FStar.List.Tot

let addResHeader key value (h: httpRes) = {h with httpResHeaders = set key value h.httpResHeaders}

let addContentTypeHtml = addResHeader "Content-Type" "text/html"

let mkHttpRes (status: int) (reason: string) (body: string) =
    { httpResStatus
      = { httpResponseVersion = Http1_1
        ; httpResponseStatusCode = status
        ; httpResponseReasonPhrase = reason
        }
    ; httpResHeaders = ["Content-Length", string_of_int (String.length body)]
    ; httpResBody = body
    }

let mkHttpRes200Html body =
  addContentTypeHtml (mkHttpRes 200 "OK" body)



let getHeader (headers: list (string * string)) (key: string)
  : option string
  = match L.find (fun (k, _) -> String.lowercase k = String.lowercase key) headers with
  | Some (_, v) -> Some v
  | None        -> None

