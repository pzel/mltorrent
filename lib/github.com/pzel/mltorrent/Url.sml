structure URL = struct
datatype protocol = UDP | HTTP | HTTPS
type t = { protocol: protocol
            , hostname: string
            , path: string
            , port: int}

local
fun protocolToString UDP = "udp://"
  | protocolToString HTTP = "http://"
  | protocolToString HTTPS = "https://"
in

fun toString {protocol,hostname,path,port} : string =
  protocolToString protocol
  ^ hostname ^ ":"
  ^ Int.toString port
  ^ "/" ^ path

fun fromString (unparsed: string) : t option =
  let val (prefixLen, proto) =
        if String.isPrefix "udp://" unparsed then (6, UDP)
        else if String.isPrefix "http://" unparsed then (7, HTTP)
        else if String.isPrefix "https://" unparsed then (8, HTTPS)
        else raise Domain
      val len = String.size unparsed - prefixLen
      val rest = String.substring(unparsed, prefixLen, len)
      val sep = fn c => c = #":" orelse c = #"/"
      val fields = String.fields sep rest
      val ints = map Int.fromString fields
  in case (fields, ints)
      of ((host::_::path), (NONE :: SOME port :: _)) => SOME {
                                                         protocol=proto,
                                                         hostname=host,
                                                         port=port,
                                                         path=String.concat path
                                                       }
       | ((host::path), _) => SOME {
                               protocol=proto,
                               hostname=host,
                               port=if proto = HTTP then 80 else 443,
                               path=String.concat path}


       | _ => NONE
  end handle Domain => NONE

end

end (* url *)



