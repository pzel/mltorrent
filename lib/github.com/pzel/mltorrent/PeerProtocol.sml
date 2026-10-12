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
fun bitfield idxs =
  let open Array
      val op<< =Word8.<< ; infix <<
      val maxIdx = List.foldl Int.max 0 idxs
      fun outIdx inIdx = (inIdx div 8)
      fun bit idx = 0w1 << (Word.fromInt (8-((idx mod 8)+1)))
      val out : Word8.word array = array(outIdx maxIdx + 1, 0w0)
      val _ = List.app (fn i => let val acc = sub(out, outIdx i)
                                    val new = Word8.orb(acc, bit i)
                                in update(out, outIdx i, new) end) idxs
      val outVector = Word8Vector.tabulate (length out, fn i => sub(out,i))
      val lengthPrefix = ConvertWord.word32ToBytesB (Word32.fromInt (length out + 1))
  in
    Word8Vector.concat[lengthPrefix
                      ,bin[0w5] (* bitfield tag *)
                      ,outVector]
  end

end :
sig
  type t = Word8Vector.vector
  val handshake : (Bytestring.string) -> (Bytestring.string) -> Bytestring.string
  val keepalive :  Bytestring.string
  val choke : Bytestring.string
  val unchoke : Bytestring.string
  val interested : Bytestring.string
  val notInterested : Bytestring.string
  val bitfield : int list -> Bytestring.string
end
