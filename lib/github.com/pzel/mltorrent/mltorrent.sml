structure Torrent = struct
type bencode = Bencode.t
datatype protocol = UDP | HTTP | HTTPS

type host = { protocol: protocol
            , hostname: string
            , path: string
            , port: int}

type t = { metaInfo : bencode
         , announceHost : host
         , peers : (NetHostDB.in_addr * int) list
         , infoHash : Bytestring.string
         }

val updateT =
fn z => let fun from m a p i = {metaInfo=m,announceHost=a,peers=p,infoHash=i}
            fun to f {metaInfo=m,announceHost=a,peers=p,infoHash=i} = f m a p i
        in FRU.makeUpdate4 (from, from, to) end z;


local
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)
  fun $(a,f) = f a
in

fun parseInfo (filePath: string) : (string, bencode) either =
  Bencode.decode' "info file" (TextIO.inputAll (TextIO.openIn filePath))
  handle (IO.Io {cause, name,...}) =>
         INL ("Failed to open "
              ^ filePath
              ^ "\nWith error: "
              ^ exnMessage cause ^ " " ^ name
              ^"\nCurrent working directory was: "
              ^ Posix.FileSys.getcwd())

fun parseUrl (unparsed: string) : host option =
  let val (prefixLen, proto) =
        if String.isPrefix "udp://" unparsed then (6, UDP)
        else if String.isPrefix "http://" unparsed then (7, HTTP)
        else if String.isPrefix "https://" unparsed then (8, HTTPS)
        else raise Domain
      val len = String.size unparsed - prefixLen
      val rest = String.substring(unparsed, prefixLen, len)
      val sep = fn c => c = #":" orelse c = #"/"
      val fields = String.fields sep rest
      val ints = map Int.fromString fields
  in case (fields, ints)
      of ((host::_::path), (NONE :: SOME port :: _)) => SOME {
                                                   protocol=proto,
                                                   hostname=host,
                                                   port=port,
                                                   path=String.concat path
                                                 }
       | ((host::path), _) => SOME {
                            protocol=proto,
                            hostname=host,
                            port=if proto = HTTP then 80 else 443,
                            path=String.concat path}


       | _ => NONE
  end handle Domain => NONE

fun parseHost (metaInfo: bencode) : (string, host) either  =
  case Bencode.atKey "announce" metaInfo
   of (SOME (Bencode.String url)) => parseUrl url
                                     >| Either.fromOption("Couldn't parse announce: "^url)
    | _=> INL "No 'announce' key present in .torrent";


fun binToAddr (input: Bytestring.string) : (string, (NetHostDB.in_addr * int) list) either =
  if Bytestring.size input mod 6 <> 0
  then INL "BAD INPUT LENGHT"
  else let open ConvertWord
           val idxs = List.tabulate(Bytestring.size input div 6, id)
           fun ss(idx) = (Bytesubstring.substring(input, idx, 4),
                          Bytesubstring.substring(input, idx+4, 2))
           val in_addr = Option.mapPartial (NetHostDB.fromString
                                            o Int.toString
                                            o Word32.toInt)
           val ips = map ss idxs
            val ipaddrs = map (fn (ip,port) =>
                                  (in_addr (bytesToWord32SB' ip),
                                   Option.map Word32.toInt (bytesToWord16SB' port)))
                              ips
            val hosts = List.mapPartial (fn (SOME ip, SOME port) => SOME (ip, port)
                                        | _ => NONE) ipaddrs
       in INR hosts
       end

fun parsePeers (d: bencode) : (string, (NetHostDB.in_addr * int) list) either  =
  case Bencode.atKey "peers" d
   of (SOME (Bencode.String b)) => binToAddr (Bytestring.fromString b)
    | _=> INL "No 'peers' key present";

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
                        , leechers : int
                        , seeders : int
                        , addresses : Word32.word list
                        , ports : Word16.word list }
(* TODO:  | Scrape *)

fun newTxnId () : txnId =
  Word32.fromLargeInt(Time.toMilliseconds(Time.now ()));

local
  fun dConnectResponse payload =
    INR \> ConnectResponse (get32(payload, 1), PW64.subVec(payload, 1))
  fun dAnnounceResponse payload = let
    open Bytestring
    val leechers = Word32.toInt (get32(payload, 3))
    val seeders = Word32.toInt (get32(payload, 4))
    val sub = extract(fromWord8Vector payload, 20, NONE)
    val _ = PolyML.print (Word8Vector.length payload)
    val peers = binToAddr \> sub
    val _ = PolyML.print(peers)
  in
    INR \> AnnounceResponse { txnId = get32(payload, 1)
                            , interval = get32(payload, 2)
                            , leechers = leechers
                            , seeders = seeders
                            , addresses = []
                            , ports = []}
  end

in
fun deserialize (payload: Word8Vector.vector) : (string, udpTrackerProtocol) either  =
  if Word8Vector.length payload = 16 andalso get32(payload, 0) = 0w0
  then dConnectResponse payload
  else if Word8Vector.length payload >= 16 andalso get32(payload, 0) = 0w1
  then dAnnounceResponse payload
  else INL \> "Couldn't parse payload: " ^ Byte.bytesToString payload
end

fun serialize (msg: udpTrackerProtocol) : binary =
  let val buffer = Word8Array.array(16, 0w0)
      val out = Word8VectorSlice.full o Word8Vector.concat
      val w64 = ConvertWord.word64ToBytesB
      val w32 = ConvertWord.word32ToBytesB
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
             ,Word8Vector.fromList[0w0,0w0,0w0,0w0]]
       | other => raise Fail (PolyML.makestring other)
  end

fun getAddr ({hostname, port, ...}) =
  case NetHostDB.getByName hostname
   of NONE => INL ("Failed to resolve " ^ hostname)
    | SOME addr => INR (INetSock.toAddr (NetHostDB.addr addr, port))

fun sendReq (host: host) (payload: binary) : (string, Word8Vector.vector) either =
  getAddr host
  >>= send (INetSock.UDP.socket()) (INetSock.any (#port host)) payload
  >>= recv (1024*1024)

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

fun getPeersUdp (t: t) (ConnectResponse (_, connId)) : (string, t) either = let
  val _ = print "\nGetting UDP peer list\n"
  val event = 3 (*started *)
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
  >>= (fn resp => INR \> tap (PolyML.print o Word8Vector.length) resp)
  >>= deserialize
  >>= (fn r =>
          (print ("\nIIINEr" ^ (PolyML.makestring r) ^ "\n"); INR t))
end
  | getPeersUdp _ other = raise Fail ("getPeers got: "^PolyML.makestring other)

fun getPeersHttp (t as {announceHost=host, infoHash=h, metaInfo=m, ...}: t) (ConnectResponse (_, _)) : (string, t) either = let
  val _ = print "\nGetting HTTP peer list\n"
  val b = Bytestring.fromString
  val size = case Bencode.access ["info", "length"] m
              of SOME (Bencode.Integer i) => IntInf.toString i
               | _ => "0"
  val hostUrl = #hostname host ^ ":" ^ Int.toString (#port host) ^ "/" ^ #path host
  val u = Fetch.url hostUrl [("compact", b"1")
                            ,("info_hash", h)
                            ,("left", b size)
                            ,("peer_id", b"000000000000000simon")
                            ,("port", b"6881")
                            ,("uploaded", b"0")]
in
  Fetch.get u
  >>= Bencode.decode' "peers"
  >>= parsePeers
  >>= (fn p => INR \> updateT t (FRU.set#peers p) $)
end
  | getPeersHttp _ other = raise Fail ("getPeers got: "^PolyML.makestring other)


fun getPeers (t:t) (cr: udpTrackerProtocol) : (string, t) either =
  case #protocol (#announceHost t)
   of UDP => getPeersUdp t cr
    | _ => getPeersHttp t cr

fun connect (t as {announceHost=host, ...}) : (string, t) either =
  sendReq host (serialize (ConnectRequest(newTxnId())))
  >>= deserialize
  >>= getPeers t
end (*local*)

end (*struct*)

fun main () = let val t = hd (CommandLine.arguments())
                  val _ =  PolyML.print_depth 100
              in Torrent.openTorrent t
                 >| Either.mapRight (tap PolyML.print)
                 >| Either.bindRight Torrent.connect
                 >| Either.appLeft print
              end
