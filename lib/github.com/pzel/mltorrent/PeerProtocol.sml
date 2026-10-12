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

fun keepalive () : bytestring =
  bin[0w0,0w0,0w0,0w0]


end : sig
        type t = Word8Vector.vector
        val handshake : (Bytestring.string) -> (Bytestring.string) -> Bytestring.string
        val keepalive : unit -> Bytestring.string
      end
