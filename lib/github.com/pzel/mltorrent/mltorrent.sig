signature BENCODE = sig
datatype key = Key of string
datatype t = String of string
           | Integer of int
           | List of t list
           | Dict of (key * t) list

val encode : t -> (string, string) either
val decode : string -> (string, t) either
val keys : t -> string list
val atKey : string -> t -> t option
end

signature TORRENT = sig
  type bencode
  type protocol
  type host = { protocol: protocol
              , hostname: string
              , port: int
              }

  type t = { metaInfo : bencode
           , announceHost : host
           , peers : NetHostDB.in_addr list
           , infoHash: Bytestring.string
           }
  val openTorrent : string -> (string, t) either
  val connect : t -> t
end
