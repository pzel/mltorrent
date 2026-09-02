signature BENCODE = sig
datatype key = Key of string
datatype t = String of string
           | Integer of int
           | List of t list
           | Dict of (key * t) list

val decode : string -> (string, t) either
val keys : t -> string list
val atKey : string -> t -> t option
end

signature TORRENT = sig
  type bencode
  type t = { metaInfo : bencode,
             announceHost : NetHostDB.in_addr list,
             peers : NetHostDB.in_addr list
         }
  val openTorrent : string -> (string, t) either
  val getPeers : t -> t
end
