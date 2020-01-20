module HTTP.Server.Types

type httpVersion =
  | Http1_1
  | HttpUnknown
type httpMethod =
  | GET | HEAD | POST | PUT | DELETE | CONNECT | OPTIONS | TRACE

type httpResponseStatusLine
  = { httpResponseVersion: httpVersion
    ; httpResponseStatusCode: int
    ; httpResponseReasonPhrase: string
    }
    
type httpRequestLine
  = { httpRequestLineMethod: httpMethod
    ; httpRequestLineRequestURI: string
    ; httpRequestLineHttpVersion: httpVersion
    }
    
type httpReq
  = { httpReqLine: httpRequestLine
    ; httpReqHeaders: list (string * string)
    ; httpReqBody: string
    }
    
type httpRes
  = { httpResStatus: httpResponseStatusLine
    ; httpResHeaders: list (string * string)
    ; httpResBody: string
    }

type charset =
  | Utf8
  | UnknownCharset

type contentType =
  | Text_Plain: charset -> contentType
  | Text_Html: charset -> contentType
  | Application_Json
  | Application_XWwwFormUrlencoded
  | Application_OctetStream: (boundary: string) -> contentType

