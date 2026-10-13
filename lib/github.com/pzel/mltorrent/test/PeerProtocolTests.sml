local

val b = Bytestring.fromString
val bin = Bytestring.fromWord8Vector o Word8Vector.fromList
open PeerProtocol

val gt = [
  It "Can serialize a handshake" (
    fn()=> let val op == = Assert.eq PolyML.makestring
               val infoHash = b"01234567890123456789"
               val peerId =   b"000000000000000simon"
               val handshake = Handshake(infoHash, peerId)
           in serialize handshake == Word8Vector.fromList[
                (* 19 + 'BitTorrent protocol' *)
                0wx13, 0wx42, 0wx69, 0wx74, 0wx54, 0wx6F, 0wx72, 0wx72, 0wx65, 0wx6E, 0wx74, 0wx20,
                0wx70, 0wx72, 0wx6F, 0wx74, 0wx6F, 0wx63, 0wx6F, 0wx6C,

                (* 8 reserved bytes *)
                0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx30,

                (* the infohash *)
                0wx31, 0wx32, 0wx33, 0wx34, 0wx35, 0wx36, 0wx37, 0wx38, 0wx39,
                0wx30, 0wx31, 0wx32, 0wx33, 0wx34, 0wx35, 0wx36, 0wx37, 0wx38,

                (* the peerid *)
                0wx39, 0wx30, 0wx30, 0wx30, 0wx30, 0wx30, 0wx30, 0wx30, 0wx30,
                0wx30, 0wx30, 0wx30, 0wx30, 0wx30, 0wx30, 0wx30, 0wx73, 0wx69, 0wx6D, 0wx6F, 0wx6E]
           end)


 ,It "can create a keepalive message" (
    fn()=> (serialize Keepalive === bin[0w0,0w0,0w0,0w0])PolyML.makestring)

 ,It "can serialize choke/unchoke/interested/notInterested messages" (
    fn()=>( (serialize Choke =??= bin[0w0,0w0,0w0,0w1,0w0]) PolyML.makestring
          ; (serialize Unchoke =??= bin[0w0,0w0,0w0,0w1,0w1]) PolyML.makestring
          ; (serialize Interested =??= bin[0w0,0w0,0w0,0w1,0w2]) PolyML.makestring
          ; (serialize NotInterested =??= bin[0w0,0w0,0w0,0w1,0w3]) PolyML.makestring
          ; Assert.succeed "Messages 0-4 trivially valid"))

 ,It "can serialize a `have` message" (
    fn()=> (serialize (Have 4) === bin[0w0, 0w0, 0w0, 0w5, 0w4,
                                       0w0, 0w0, 0w0, 0w4])
             PolyML.makestring)
 ,It "can serialize a `have` message for a large index" (
    fn()=> (serialize (Have 4294967295) === bin[0w0, 0w0, 0w0, 0w5, 0w4,
                                                0wxff, 0wxff, 0wxff, 0wxff])
             PolyML.makestring)

 ,It "can serialize a bitfield with only piece 0" (
    fn()=> (serialize \> Bitfield[0] === bin[0w0, 0w0, 0w0, 0wx2,  (*length*)
                                             0wx5, (*bitfield tag*)
                                             0w128] (*the bitfield binary *)
           ) PolyML.makestring)

 ,It "can serialize a bitfield with pieces 0, 8, and 9" (
    fn()=> (serialize \> Bitfield[0,8,9] === bin[0w0, 0w0, 0w0, 0wx3, 0wx5,
                                                 0w128, 0w192]) PolyML.makestring)

 ,It "can serialize a bitfield with pieces 0, 8, and 9 even if unsorted" (
    fn()=> (serialize \> Bitfield[9,8,0] === bin[0w0, 0w0, 0w0, 0wx3, 0wx5,
                                                 0w128, 0w192]) PolyML.makestring)

 ,It "can serialize a bitfield with pieces 7, 15 and 127 (low bits)" (
    fn()=> (serialize \> Bitfield[7,15,127] === bin[0w0, 0w0, 0w0, 0wx11, 0wx5,
                                                    (* the bitfield *)
                                                    0w1, 0w1, 0w0, 0w0,
                                                    0w0, 0w0, 0w0, 0w0,
                                                    0w0, 0w0, 0w0, 0w0,
                                                    0w0, 0w0, 0w0, 0w1])
             PolyML.makestring)

 ,It"can create a request message"(
    fn()=> (serialize \> Request(1,2,100) === bin[0w0,0w0,0w0,0w13,0w6,
                                    0w0,0w0,0w0,0w1,
                                    0w0,0w0,0w0,0w2,
                                    0w0,0w0,0w0,0w100])
             PolyML.makestring)

 ,It"can create a cancel message"(
    fn()=> (serialize \> Cancel(1,2,100) === bin[0w0,0w0,0w0,0w13,0w8,
                                                 0w0,0w0,0w0,0w1,
                                                 0w0,0w0,0w0,0w2,
                                                 0w0,0w0,0w0,0w100])
             PolyML.makestring)

 ,It"can create a piece message"(
    fn()=> (serialize \> Piece(4,8,bin[0w11,0w22,0w33,0w44,0w55,0w66])
                  === bin[0w0,0w0,0w0,0w15,0w7,
                          0w0,0w0,0w0,0w4,
                          0w0,0w0,0w0,0w8,
                          0w11,0w22,0w33,0w44,0w55,0w66])
             PolyML.makestring)

]

val pt = [
  It "Can parse a handshake" (
    fn()=> let val op == = Assert.eq PolyML.makestring
               val infoHash = b"01234567890123456789"
               val peerId =   b"000000000000000simon"
               val payload = serialize \> Handshake(infoHash, peerId)
           in parse payload == INR \> Handshake(infoHash, peerId) end)
 ,It "can parse a keepalive message" (
    fn()=> (parse \> serialize Keepalive === INR Keepalive) PolyML.makestring)

 ,It "can parse the four trivial messages" (
    fn()=>( (parse \> serialize Choke =??= INR Choke) PolyML.makestring
          ; (parse \> serialize Unchoke =??= INR Unchoke) PolyML.makestring
          ; (parse \> serialize Interested =??= INR Interested) PolyML.makestring
          ; (parse \> serialize NotInterested =??= INR NotInterested) PolyML.makestring
          ; Assert.succeed "Messages 0-4 trivially valid"))

 ,It "can parse a `have` message" (
    fn()=> (parse \> serialize (Have 4) === INR \> Have 4)
             PolyML.makestring)

 ,It "can parse a `have` message for a large index" (
    fn()=> (parse \> serialize (Have 4294967295) === INR \> Have 4294967295)
             PolyML.makestring)

 ,It "can parse a bitfield with only piece 0" (
    fn()=> (parse (serialize \> Bitfield[0]) === INR \> Bitfield[0])
             PolyML.makestring)

 ,It "can parse a bitfield with pieces 7, 15 and 127 (low bits)" (
    fn()=> (parse (serialize (Bitfield[7,15,127])) === INR \> Bitfield[7,15,127])
             PolyML.makestring)


 ,It"can parse a request message"(
    fn()=> (parse (serialize \> Request(1,2,100)) === INR \> Request(1,2,100))
             PolyML.makestring)

 ,It"can create a cancel message"(
    fn()=> (parse (serialize \> Cancel(3,17,231)) === INR \> Cancel(3,17,231))
             PolyML.makestring)

]


in
val _ = addTests "peer protocol generator tests" gt
val _ = addTests "peer protocol parsing tests" pt
end
