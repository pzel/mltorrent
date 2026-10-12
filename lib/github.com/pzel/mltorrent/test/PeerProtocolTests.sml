local

val b = Bytestring.fromString
val bin = Bytestring.fromWord8Vector o Word8Vector.fromList
open PeerProtocol


val tt = [
  It "Can create a handshake" (
    fn()=> let val op == = Assert.eq PolyML.makestring
               val infoHash = b"01234567890123456789"
               val peerId =   b"000000000000000simon"
           in handshake infoHash peerId == Word8Vector.fromList[
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
    fn()=> (keepalive === bin[0w0,0w0,0w0,0w0])PolyML.makestring)

 ,It "can create choke/unchoke/interested/notInterested messages" (
    fn()=>( (choke =??= bin[0w0,0w0,0w0,0w1,0w0]) "choke"
          ; (unchoke =??= bin[0w0,0w0,0w0,0w1,0w1]) "unchoke"
          ; (interested =??= bin[0w0,0w0,0w0,0w1,0w2]) "interested"
          ; (notInterested =??= bin[0w0,0w0,0w0,0w1,0w3]) "notInterested"
          ; Assert.succeed "Messages 0-4 trivially valid"))

 ,It "can create a bitfield with only piece 0" (
    fn()=> (bitfield[0] === bin[0wx0, 0wx0, 0wx0, 0wx2,  (*length*)
                                0wx5, (*bitfield tag*)
                                0w128] (*the bitfield binary *)
           ) PolyML.makestring)

 ,It "can create a bitfield with pieces 0, 8, and 9" (
    fn()=> (bitfield[0,8,9] === bin[0wx0, 0wx0, 0wx0, 0wx3,
                                    0wx5,
                                    0w128, 0w192]) PolyML.makestring)

 ,It "can create a bitfield with pieces 0, 8, and 9 even if unsorted" (
    fn()=> (bitfield[9,8,0] === bin[0wx0, 0wx0, 0wx0, 0wx3,
                                    0wx5,
                                    0w128, 0w192]) PolyML.makestring)

 ,It "can create a bitfield with pieces 7, 15 and 127 (low bits)" (
    fn()=> (bitfield[7,15,127] === bin[0wx0, 0wx0, 0wx0, 0wx11, 0wx5,
                                       (* the bitfield *)
                                       0w1, 0w1, 0wx0, 0wx0,
                                       0wx0, 0wx0, 0wx0, 0wx0,
                                       0wx0, 0wx0, 0wx0, 0wx0,
                                       0wx0, 0wx0, 0wx0, 0w1]) PolyML.makestring)
]

in
val _ = addTests "peer protocol tests" tt
end
