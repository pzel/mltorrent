structure Bencode : BENCODE = struct
datatype key = Key of string
datatype t = String of string
           | Integer of int
           | List of t list
           | Dict of (key * t) list
exception UnorderedKeys of string list;

local
  infix 1 >>= >>
  infix 1 <*
  infix 4 <*> <$>
  infixr 1 <|>
  infix 0 <?>
  open Parsec
in

fun manyN n p finalizer =
    let fun runIt acc =
            p >>=
              (fn res => if length (res::acc) = n
                         then return (finalizer (rev (res::acc)))
                         else runIt (res::acc))
    in if n = 0
       then return (finalizer [])
       else runIt []
    end

fun benString n = manyN n anyChar (String o String.implode)
fun keyString n = manyN n anyChar (Key o String.implode)

val stringParser =
    integer >>= (fn l => (char#":" ) >> (benString l))

val keyParser =
    integer >>= (fn l => (char#":" ) >> (keyString l))

val integerParser =
    between (char#"i") (char#"e") integer
    >>= (fn i => return (Integer i))

fun listParser () =
    between (char#"l") (char#"e") (many1 (delay bencodeParser))
    >>= (fn l => return (List l))

and dictParser () =
    between (char#"d") (char#"e") (many1 (tupleParser()))
    >>= (fn res => return (Dict res))

and tupleParser () =
    keyParser
    >>= (fn k => bencodeParser()
    >>= (fn v => return (k,v)))

and bencodeParser () =
    stringParser
      <|> integerParser
      <|> (listParser())
      <|> (dictParser())

local
fun doEnc (String s) = Int.toString (String.size s) ^ ":" ^ s
  | doEnc (Integer i) = if i < 0
                         then "i-" ^ (Int.toString (~i)) ^ "e"
                         else "i" ^ (Int.toString i) ^ "e"
  | doEnc (List i) = concat (("l" :: (map doEnc i)) @ ["e"])
  | doEnc (Dict i) = (check i; concat (("d" :: (map doEncKv i)) @ ["e"]))
and doEncKv (Key i, value) = doEnc (String i) ^ doEnc value
and check [] = raise UnorderedKeys []
  | check ((Key _, _)::[]) = ()
  | check ((Key k, _)::(Key j, nxt)::rest) =
    if String.compare(k, j) = GREATER
    then raise UnorderedKeys [k,j]
    else check ((Key j, nxt)::rest)
in
fun encode vs =
    INR (doEnc vs)
    handle (UnorderedKeys keys) =>
           INL \> concat [ "Unordered keys: " ^ String.concatWith "," keys]
end

fun decode (input: string) : (string, t) either =
    case runParser (bencodeParser ()) input
     of Ok v => INR v
      | Err e => INL (PolyML.makestring e)

fun keys (Dict kv) = map (fn (Key s, _) => s) kv
  | keys _ = []

fun atKey k (Dict kv) = search k kv
  | atKey _ _ = NONE
and search _ [] = NONE
  | search k ((Key j, v)::rest) = if k = j
                                  then SOME v
                                  else search k rest

end
end (*local*)

structure Torrent : TORRENT = struct
type bencode = Bencode.t
datatype protocol = UDP | HTTP

type host = { protocol: protocol
            , hostname: string
            , port: int}

type t = { metaInfo : bencode
         , announceHost : host
         , peers : NetHostDB.in_addr list
         , infoHash : Bytestring.string
         }


fun parseInfo (filePath: string) : (string, bencode) either =
    Bencode.decode (TextIO.inputAll (TextIO.openIn filePath))
    handle (IO.Io {cause, name,...}) =>
           INL ("Failed to open "
                ^ filePath
                ^ "\nWith error: "
                ^ exnMessage cause ^ " " ^ name
                ^"\nCurrent working directory was: "
                ^ Posix.FileSys.getcwd())

fun parseUrl (unparsed: string) : host option =
    let val prefixLen = if String.isPrefix "udp://" unparsed then 6
                        else if String.isPrefix "http://" unparsed then 7
                        else 0
        val len = String.size unparsed - prefixLen
        val rest = String.substring(unparsed, prefixLen, len)
        val sep = fn c => c = #":" orelse c = #"/"
        val fields = String.fields sep rest
        val ints = map Int.fromString fields
    in case (fields, ints) of
                ((host::_), (NONE :: SOME port :: _)) => SOME {protocol=HTTP,
                                                               hostname=host,
                                                               port=port}
              | _ => NONE
    end

fun parseHost (metaInfo: bencode) : (string, host) either  =
    case Bencode.atKey "announce" metaInfo
     of (SOME (Bencode.String url)) => (parseUrl url) >| Either.fromOption("Counldn't parse announce host: "^url)
      | _=> INL "No 'announce' key present in .torrent";

local
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)
in

fun getHash (metaInfo: bencode) : (string, Bytestring.string) either =
    (Bencode.atKey "info" metaInfo)
    >| Either.fromOption "No 'info' key present"
    >>= Bencode.encode
    >>= INR o SHA1.hashString

fun openTorrent (filePath: string) : (string, t) either =
    (parseInfo filePath)
    >>= (fn metaInfo => getHash metaInfo
    >>= (fn hash => parseHost metaInfo
    >>= (fn host => INR {metaInfo = metaInfo
                         ,announceHost = host
                         ,peers = []
                         ,infoHash = hash})))

type lword = LargeWord.word
type txnId = Word32.word
type connectionId = lword
type buffer = Word8Array.array
type binary = Word8VectorSlice.slice
type udpSocket = INetSock.dgram_sock
structure PW32 = PackWord32Big
structure PW64 = PackWord64Big
fun get32(v,offset) = Word32.fromLarge(PW32.subVec(v,offset));


datatype udpTrackerProtocol
  = ConnectRequest of txnId
  | ConnectResponse of (txnId * connectionId)
  | AnnounceRequest of { txnId : txnId
                       , connectionId : connectionId
                       , infoHash : Bytestring.string
                       , peerId : Bytestring.string
                       , downloaded : lword
                       , left : lword
                       , uploaded : lword
                       , event : int
                       , ipAddress : Word32.word
                       , key : Word32.word
                       , numWant : Word32.word
                       , port : Word16.word}
  | AnnounceResponse of { txnId : txnId
                        , interval : Word32.word
                        , leechers : Word32.word
                        , seeders : Word32.word
                        , addresses : Word32.word list
                        , ports : Word16.word list }
         (* TODO:  | Scrape *)

fun newTxnId () : txnId =
    Word32.fromLargeInt(Time.toMilliseconds(Time.now ()));

fun deserialize (payload: Word8Vector.vector) : (string, udpTrackerProtocol) either  =
    if Word8Vector.length payload = 16 andalso get32(payload, 0) = 0w0
    then INR \> ConnectResponse (get32(payload, 1),
                                 PW64.subVec(payload, 1))
    else if Word8Vector.length payload >= 16 andalso get32(payload, 0) = 0w1
    then (PolyML.print_depth 100
         ; PolyML.print payload
          ; PolyML.print (get32(payload, 1));
          INR \> AnnounceResponse { txnId = get32(payload, 1)
                                 , interval = get32(payload, 2)
                                 , leechers = get32(payload, 3)
                                 , seeders = get32(payload, 4)
                                 , addresses = []
                                 , ports = []})
    else INL \> "Couldn't parse payload: " ^ Byte.bytesToString payload

fun serialize (msg: udpTrackerProtocol) : binary =
    let val buffer = Word8Array.array(16, 0w0)
        val out = Word8VectorSlice.full o Word8Vector.concat
        val w64 = ConvertWord.word64ToBytesB
        val w32 = ConvertWord.word32ToBytesB
        val w16 = ConvertWord.word16ToBytesB
    in case msg
        of ConnectRequest(id) => (
          app (fn (offset, v) => PW32.update(buffer, offset, v))
              [(0, 0wx417), (1, 0wx27101980), (3, (Word32.toLarge id))];
          Word8VectorSlice.full (Word8Array.vector buffer))
         | (AnnounceRequest ar) =>
           out [w64 (#connectionId ar)
               ,w32 0w1
               ,w32 (#txnId ar)
               ,Bytestring.toWord8Vector (#infoHash ar)
               ,Bytestring.toWord8Vector (#peerId ar)
               ,w64 (#downloaded ar)
               ,w64 (#left ar)
               ,w64 (#uploaded ar)
               ,w32 (Word32.fromInt (#event ar))
               ,w32 (#ipAddress ar)
               ,w32 (#key ar)
               ,w32 (#numWant ar)
               ,w16 (#port ar)]
         | other => raise Fail (PolyML.makestring other)
    end

fun getAddr ({hostname, port, ...}) =
    case NetHostDB.getByName hostname
     of NONE => INL ("Failed to resolve " ^ hostname)
      | SOME addr => INR (INetSock.toAddr (NetHostDB.addr addr, port))

fun sendReq (host: host) (payload: binary) : (string, Word8Vector.vector) either =
    getAddr host
    >>= (send (INetSock.UDP.socket()) (INetSock.any (#port host))  payload)
    >>= (recv 1024)

and send sock fromAddr payload toAddr =
    ( Socket.Ctl.setREUSEADDR (sock, true)
    ; Socket.bind (sock, fromAddr)
    ; Socket.sendVecTo (sock, toAddr, payload)
    ; INR sock)
    handle OS.SysErr e => INL \> "Failed to send payload: "
                                 ^ PolyML.makestring e
and recv n sock =
    withTimeout (Time.fromMilliseconds 1000)
                (fn () => Socket.recvVecFromNB(sock, n))
    >>= (fn (resp, _) => INR resp before Socket.close sock)
    handle OS.SysErr e => INL \> "Failed to recv on socket: "
                                 ^ PolyML.makestring e
         | Size => INL \> "Invalid size for recv: "
                          ^ Int.toString n;


(* Announce

    Choose a random transaction ID.
    Fill the announce request structure.
    Send the packet.

IPv4 announce request:

Offset  Size    Name    Value
0       64-bit integer  connection_id
8       32-bit integer  action          1 // announce
12      32-bit integer  transaction_id
16      20-byte string  info_hash
36      20-byte string  peer_id
56      64-bit integer  downloaded
64      64-bit integer  left
72      64-bit integer  uploaded
80      32-bit integer  event           0 // 0: none; 1: completed; 2: started; 3: stopped
84      32-bit integer  IP address      0 // default
88      32-bit integer  key
92      32-bit integer  num_want        -1 // default
96      16-bit integer  port
98

    Receive the packet.
    Check whether the packet is at least 20 bytes.
    Check whether the transaction ID is equal to the one you chose.
    Check whether the action is announce.
    Do not announce again until interval seconds have passed or an event has occurred.

Do note that most trackers will only honor the IP address field under limited circumstances.

IPv4 announce response:

Offset      Size            Name            Value
0           32-bit integer  action          1 // announce
4           32-bit integer  transaction_id
8           32-bit integer  interval
12          32-bit integer  leechers
16          32-bit integer  seeders
20 + 6 * n  32-bit integer  IP address
24 + 6 * n  16-bit integer  TCP port
20 + 6 * N
*)

fun getPeers (t: t) (ConnectResponse (txnId, connId)) =
    let val event = 3 (*started *)
        val req = AnnounceRequest { txnId = newTxnId ()
                                  , connectionId = connId
                                  , infoHash = #infoHash t
                                  , peerId = #infoHash t
                                  , downloaded = 0w0
                                  , left = 0w574823
                                  , uploaded = 0w0
                                  , event = event
                                  , ipAddress = 0w0
                                  , key = 0w0
                                  , numWant = Word32.fromInt (~1)
                                  , port = 0w6881
                                  }
    in
      sendReq (#announceHost t) (serialize req)
      >>= (fn resp => (PolyML.print (Word8Vector.length resp); INR resp))
      >>= deserialize
      >>= (fn r => INR (print ("\nIIINEr" ^ (PolyML.makestring r) ^ "\n")))
    end
  | getPeers _ other = raise Fail ("getPeers got: "^PolyML.makestring other)

fun connect (t as {announceHost=host, ...}) : (string, t) either =
    sendReq host (serialize (ConnectRequest(newTxnId())))
    >>= deserialize
    >>= getPeers t
    >>= (fn peers => INR t)

end

end
