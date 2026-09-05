structure Bencode : BENCODE = struct
datatype key = Key of string
datatype t = String of string
           | Integer of int
           | List of t list
           | Dict of (key * t) list

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

type host = {protocol: protocol,
             hostname: string,
             port: int}

type t = { metaInfo : bencode,
           announceHost : host,
           peers : NetHostDB.in_addr list
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
     of (SOME (Bencode.String url)) => (parseUrl url) >| Either.fromOption("Counldn't parse "^url)
      | _=> INL "No 'announce' key present in .torrent";

local
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)
in
fun openTorrent (filePath: string) : (string, t) either =
    (parseInfo filePath)
    >>= (fn metaInfo => parseHost metaInfo
    >>= (fn host => INR {metaInfo = metaInfo,
                         announceHost = host,
                         peers = []}))

type txnId = LargeWord.word
type connectionId = LargeWord.word
type buffer = Word8Array.array
type binary = Word8VectorSlice.slice
type udpSocket = INetSock.dgram_sock
structure PW32 = PackWord32Big
structure PW64 = PackWord64Big

datatype udpTrackerProtocol =
         ConnectRequest of txnId
         | ConnectResponse of (txnId * connectionId)
         (* TODO: |  Announce ; | Scrape *)

fun newTxnId () : txnId =
    LargeWord.fromLargeInt (Time.toMilliseconds(Time.now ()));

fun deserialize (payload: Word8Vector.vector) : udpTrackerProtocol option =
    if Word8Vector.length payload = 16 andalso PW32.subVec(payload, 0) = 0w0
    then let val txnId = PW32.subVec(payload, 1)
             val cxnId = PW64.subVec(payload, 1)
         in SOME (ConnectResponse (txnId, cxnId))
         end
    else NONE

fun serialize (msg: udpTrackerProtocol) : binary =
    let val buffer = Word8Array.array(16, 0w0)
    in case msg
        of ConnectRequest(id) => (
          app (fn (offset, v) => PW32.update(buffer, offset, v))
              [(0, 0wx417), (1, 0wx27101980), (3, id)];
          Word8VectorSlice.full (Word8Array.vector buffer))
         | ConnectResponse(_,_) => raise Fail "TODO"
    end

fun getAddr ({hostname, port, ...}) = (* : (string, INetSock.sock_addr) option = *)
    case NetHostDB.getByName hostname
     of NONE => INL ("Failed to resolve " ^ hostname)
      | SOME addr => INR (INetSock.toAddr (NetHostDB.addr addr, port))

fun sendReq (host: host) (payload: binary) : (string, Word8Vector.vector) either =
    getAddr host
    >>= (send (INetSock.UDP.socket()) (INetSock.any (#port host))  payload)
    >>= (recv 16)

and send sock fromAddr payload toAddr =
    (Socket.Ctl.setREUSEADDR (sock, true)
    ;Socket.bind (sock, fromAddr)
    ;Socket.sendVecTo (sock, toAddr, payload)
    ;INR sock)
    handle OS.SysErr _ => INL "Failed to send payload"
and recv n sock =
    withTimeout (Time.fromMilliseconds 1000)
                (fn () => Socket.recvVecFromNB(sock, n))
    >>= (fn (resp, _) => INR resp before Socket.close sock)
    handle OS.SysErr _ => INL "Failed to recv on socket"
         | Size => INL "Invalid size for recv";


fun getPeers (t as {announceHost=host, ...}) =
    let  val txnId = newTxnId()
         val request = ConnectRequest txnId
         val result = sendReq host (serialize request)
         val _ = print (PolyML.makestring result)
         val _ = print ("\n" ^ (PolyML.makestring ((Either.mapRight deserialize) result)))
  in
    t
  end
end

end
