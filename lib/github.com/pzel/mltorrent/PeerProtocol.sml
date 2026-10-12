structure PeerProtocol = struct

type t = Word8Vector.vector
type bytestring = Bytestring.string

val b = Bytestring.fromString
val bin = Bytestring.fromWord8Vector o Word8Vector.fromList

fun handshake (infoHash: bytestring) (peerId: bytestring) : bytestring =
  Bytestring.concat[
    bin[0w19]
  , b"BitTorrent protocol"
  , bin[0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w0]
  , infoHash
  , peerId
  ]

val keepalive : bytestring = bin[0w0,0w0,0w0,0w0]

val choke : bytestring = bin[0w0,0w0,0w0,0w1,0w0]
val unchoke : bytestring = bin[0w0,0w0,0w0,0w1,0w1]
val interested : bytestring = bin[0w0,0w0,0w0,0w1,0w2]
val notInterested : bytestring = bin[0w0,0w0,0w0,0w1,0w3]


end : sig
        type t = Word8Vector.vector
        val handshake : (Bytestring.string) -> (Bytestring.string) -> Bytestring.string
        val keepalive :  Bytestring.string
        val choke : Bytestring.string
        val unchoke : Bytestring.string
        val interested : Bytestring.string
        val notInterested : Bytestring.string
      end
