structure PeerProtocol = struct
type bytestring = Bytestring.string
type index = int
datatype t = Handshake of (bytestring * bytestring)
           | Keepalive
           | Choke
           | Unchoke
           | Interested
           | NotInterested
           | Have of index
           | Bitfield of index list
           | Request of (index * int * int)
           | Piece of (index * int * bytestring)
           | Cancel of (index * int * int)

val b = Bytestring.fromString
val bin = Bytestring.fromWord8Vector o Word8Vector.fromList
val bin32 = ConvertWord.word32ToBytesB o Word32.fromInt

fun serialize (Handshake(infoHash, peerId)) =
  Bytestring.concat[
    bin[0w19]
  , b"BitTorrent protocol"
  , bin[0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w0]
  , infoHash
  , peerId]

  | serialize Keepalive = bin[0w0,0w0,0w0,0w0]
  | serialize Choke = bin[0w0,0w0,0w0,0w1,0w0]
  | serialize Unchoke = bin[0w0,0w0,0w0,0w1,0w1]
  | serialize Interested = bin[0w0,0w0,0w0,0w1,0w2]
  | serialize NotInterested = bin[0w0,0w0,0w0,0w1,0w3]

  | serialize (Have idx) = Word8Vector.concat[
      bin[0w0,0w0,0w0,0w5,0w4]
     ,ConvertWord.word32ToBytesB (Word32.fromInt idx)]

  | serialize (Bitfield idxs) = bitfield idxs
  | serialize (Request(idx,begin,length)) =
    Word8Vector.concat
      [bin32 13,bin[0w6],bin32 idx,bin32 begin,bin32 length]
  | serialize (Cancel(idx,begin,length)) =
    Word8Vector.concat
      [bin32 13,bin[0w8],bin32 idx,bin32 begin,bin32 length]
  | serialize (Piece(idx,begin,d)) =
    Word8Vector.concat
      [bin32(Word8Vector.length d+9),bin[0w7],bin32 idx,bin32 begin,d]

and bitfield idxs =
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
                      ,bin[0w5]
                      ,outVector]
  end

end :
sig
  datatype t = Handshake of (Bytestring.string * Bytestring.string)
             | Keepalive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have of int
             | Bitfield of int list
             | Request of (int * int * int)
             | Piece of (int * int * Bytestring.string)
             | Cancel of (int * int * int)
  val serialize : t -> Bytestring.string
(*
  val handshake : (Bytestring.string) -> (Bytestring.string) -> Bytestring.string
  val keepalive :  Bytestring.string
  val choke : Bytestring.string
  val unchoke : Bytestring.string
  val interested : Bytestring.string
  val notInterested : Bytestring.string
  val have : int -> Bytestring.string
  val bitfield : int list -> Bytestring.string
  val request : int -> int -> int -> Bytestring.string
  val cancel : int -> int -> int -> Bytestring.string
  val piece : int -> int ->  Bytestring.string -> Bytestring.string
*)
end
