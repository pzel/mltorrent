structure PeerProtocol = struct
type bytestring = Bytestring.string
type index = int
datatype state = Fresh
               | HandshakeSent
               | HandshakeReceived  (* more: interested, choked, etc. *)

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
val hex = Bytestring.toStringHex
val i2s = Int.toString
fun bit (i: int) : Word8.word = Word8.<<(0w1,Word.fromInt (8-((i mod 8)+1)))

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
      val maxIdx = List.foldl Int.max 0 idxs
      fun outIdx inIdx = (inIdx div 8)
      val out : Word8.word array = array(outIdx maxIdx + 1, 0w0)
      val _ = List.app (fn i => let val acc = sub(out, outIdx i)
                                    val new = Word8.orb(acc, bit i)
                                in update(out, outIdx i, new) end) idxs
      val outVector = Word8Vector.tabulate (length out, fn i => sub(out,i))
      val lengthPrefix = ConvertWord.word32ToBytesB (Word32.fromInt (length out + 1))
  in
    Word8Vector.concat[lengthPrefix, bin[0w5], outVector]
  end

(* Assuming framing is handled on the tcp receiving end*)
local open Bytestring in
fun parse b =
  if size b < 4
  then INL \> String.^("Message too short: ", PolyML.makestring b)
  else if size b = 4
  then INR Keepalive
  else case (sub(b,4), extract(b, 5, NONE)) of
           (0wx54, _) => parseHandshake b
         | (0wx0, _) => INR Choke
         | (0wx1, _) => INR Unchoke
         | (0wx2, _) => INR Interested
         | (0wx3, _) => INR NotInterested
         | (0wx4, body) => parseHave body
         | (0wx5, body) => parseBitField body
         | (0wx6, body) => parseReqC Request body
         | (0wx7, body) => parsePiece body
         | (0wx8, body) => parseReqC Cancel body
         | (w,_) => INL \> String.^("Unknown message type: 0x",Word8.toString w)

and parseHandshake b =
  if size b = 68
  then INR \> Handshake (substring(b,28,20) ,substring(b,48,20))
  else INL \> String.concat["bad handshake?", PolyML.makestring (Bytestring.size b, Byte.bytesToString b)]

and parseHave b =
  case ConvertWord.bytesToWord32B' b
   of (SOME word) => INR (Have (Word32.toInt word))
    | _ => INL "Failed to parse `have` message payload"

and parseBitField b =
  let val masks = List.tabulate(8, fn i => (i, bit i))
      fun extract (offset,b,acc) =
        (List.mapPartial (fn (i,m) =>
                             if Word8.andb(b, m) > 0w0
                             then SOME(offset*8+i)
                             else NONE) masks) @ acc
  in INR (Bitfield \> Word8Vector.foldri extract [] b)
  end

and parseReqC cons b =
  let val i = LargeWord.toInt
      val idx = PackWord32Big.subVec(b,0)
      val begin = PackWord32Big.subVec(b,1)
      val length = PackWord32Big.subVec(b,2)
  in INR \> cons(i idx, i begin, i length)
  end handle Subscript => INL "Failed to parse `request/cancel` message payload"

and parsePiece b =
  let val i = LargeWord.toInt
      val idx = PackWord32Big.subVec(b,0)
      val begin = PackWord32Big.subVec(b,1)
      val data = extract(b, 8, NONE)
  in INR \> Piece(i idx, i begin, data)
  end handle Subscript => INL "Failed to parse `piece` message payload"
end (*Local *)

fun compactString (Handshake(p,i)) = "Handshake ("^hex p^","^hex i^")"
  | compactString (Keepalive) = "Keepalive"
  | compactString (Choke) = "Choke"
  | compactString (Unchoke) = "Unchoke"
  | compactString (Interested) = "Interested"
  | compactString (NotInterested) = "NotInterested"
  | compactString (Have n) = "Have ("^Int.toString n^")"
  | compactString (Bitfield l) = "Bitfield [" ^ String.concatWith "," (map i2s l) ^ "]"
  | compactString (Request(i,b,l)) = "Request ("^i2s i^","^i2s b^","^i2s l^")"
  | compactString (Piece(i,b,d)) = "Piece ("^i2s i^","^i2s b^",#"^Int.toString(Bytestring.size d)^")"
  | compactString (Cancel(i,b,l)) ="Cancel ("^i2s i^","^i2s b^","^i2s l^")"

end :
sig
  datatype state = Fresh
               | HandshakeSent
               | HandshakeReceived  (* more: interested, choked, etc. *)

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
  val parse: Bytestring.string -> (string, t) either
  val compactString : t -> string
end
