signature BENCODE = sig
datatype key = Key of string
datatype t = String of string
           | Integer of int
           | List of t list
           | Dict of (key * t) list

val decode : string -> (string, t) either
val keys : t -> string list
end

signature TORRENT = sig
  type bencode
  val openTorrent : string -> (string, bencode) either;
end
