structure Torrent = struct
type bencode = Bencode.t
type bytestring = Bytestring.string
type tcp_socket = (INetSock.inet, Socket.active Socket.stream) Socket.sock

datatype info =
         SingleFile of { name: Bytestring.string
                    , pieceLength: int
                    , pieces: bytestring vector
                    , length : int
                    }
       | ManyFiles of { name: Bytestring.string
                   , pieceLength: int
                   , pieces: Bytestring.string vector
                   , files: ({ length : int, path: bytestring list} list)
                   }

type peer = { addr: NetHostDB.in_addr
            , port: int
            , pieces: int list
            , peerId: bytestring
            , state: PeerProtocol.state
            , socket: tcp_socket option
            }

val updatePeer =
fn z => let fun from a po pi pid st so = {addr=a,port=po,pieces=pi,peerId=pid,state=st,socket=so}
             fun to f {addr=a,port=po,pieces=pi,peerId=pid,state=st,socket=so} = f a po pi pid st so
         in FRU.makeUpdate6 (from, from, to) end z;

(* todo unhardcode *)
val peerId = Bytestring.fromString "000000000000000simon"

type t = { metaInfo : bencode
         , announceHost : URL.t
         , peers : peer list
         , info : info
         , infoHash : bytestring
         }

val updateT =
fn z => let fun from m a p i ih = {metaInfo=m,announceHost=a,peers=p,info=i,infoHash=ih}
            fun to f {metaInfo=m,announceHost=a,peers=p,info=i,infoHash=ih} = f m a p i ih
        in FRU.makeUpdate5 (from, from, to) end z;

type event = (bytestring * PeerProtocol.t)
fun showEvent (peerId, event:PeerProtocol.t) = (Bytestring.toStringHex peerId ^":" ^PeerProtocol.compactString event)

local
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)
  fun $(a,f) = f a
  val set = FRU.set
  val v = Word8VectorSlice.full
  val b = Bytestring.fromString
in

fun parseMetaInfoFile (filePath: string) : (string, bencode) either =
  Bencode.decode' "info file" (TextIO.inputAll (TextIO.openIn filePath))
  handle (IO.Io {cause, name,...}) =>
         INL ("Failed to open "
              ^ filePath
              ^ "\nWith error: "
              ^ exnMessage cause ^ " " ^ name
              ^"\nCurrent working directory was: "
              ^ Posix.FileSys.getcwd())

fun parseInfo (dict: Bencode.t) : (string, info) either = let
  open Bencode
  fun k key = Either.fromOption ("parseInfo: Failed to find key: "^key) (atKey key dict)
in
  k "name"
  >>= (fn n => k "length"
  >>= (fn l => k "piece length"
  >>= (fn pl => k "pieces"
  >>= (fn pcs =>
          case (n,l,pl,pcs)
           of (String n, Integer l, Integer pl, String pcs) =>
              INR \> SingleFile { name= Bytestring.fromString n
                                , length=Int.fromLarge l
                                , pieceLength=Int.fromLarge pl
                                , pieces = Vector.fromList [Bytestring.fromString pcs]
                                }
            | _ => INL "Malformed info data"))))
 end

fun getAnnounce (metaInfo: bencode) : (string, URL.t) either  =
  case Bencode.atKey "announce" metaInfo
   of (SOME (Bencode.String url)) => URL.fromString url
                                     >| Either.fromOption("Couldn't parse announce: "^url)
    | _=> INL "No 'announce' key present in .torrent";

fun binToPeers (input: Bytestring.string) : (string, peer list) either =
  if Bytestring.size input mod 6 <> 0
  then INL "BAD INPUT LENGTH"
  else
    let open ConvertWord
        val idxs = List.tabulate(Bytestring.size input div 6, id)
        fun ss(idx) = (Bytesubstring.substring(input, idx*6, 4),
                       Bytesubstring.substring(input, idx*6+4, 2))
        val in_addr = Option.mapPartial (NetHostDB.fromString
                                         o Int.toString
                                         o Word32.toInt)
        val ips = map ss idxs
        val ipaddrs = map (fn (ip,port) =>
                              (in_addr (bytesToWord32SB' ip),
                               Option.map Word32.toInt (bytesToWord16SB' port)))
                          ips
        val peers = List.mapPartial (fn (SOME ip, SOME port) =>
                                        SOME{addr=ip
                                            ,port=port
                                            ,pieces=[]
                                            ,peerId=b""
                                            ,state=PeerProtocol.Fresh
                                            ,socket=NONE}
                                    | _ => NONE) ipaddrs
    in INR peers
    end

fun parsePeers (d: bencode) : (string, peer list) either  =
  case Bencode.atKey "peers" d
   of (SOME (Bencode.String b)) => binToPeers (Bytestring.fromString b)
    | _=> INL "No 'peers' key present";

fun getHash (metaInfo: bencode) : (string, Bytestring.string) either =
  (Bencode.atKey "info" metaInfo)
  >| Either.fromOption "No 'info' key present"
  >>= Bencode.encode
  >>= INR o SHA1.hashString

fun openTorrent (filePath: string) : (string, t) either =
  parseMetaInfoFile filePath
  >>= (fn m=> getHash m
  >>= (fn h=> getAnnounce m
  >>= (fn url=> Either.fromOption "Couldn't find key: info in torrent metaInfo"
                                  (Bencode.atKey "info" m)
  >>= (fn infoKey=> parseInfo infoKey
  >>= (fn i=>
          INR {metaInfo = m
               ,announceHost = url
               ,peers = []
               ,info = i
               ,infoHash = h})))))

fun getAddr ({hostname, port, ...} : URL.t) =
  case NetHostDB.getByName hostname
   of NONE => INL ("Failed to resolve " ^ hostname)
    | SOME addr => INR (INetSock.toAddr (NetHostDB.addr addr, port))

fun getPeersHttp (t as {announceHost=url, infoHash=h, metaInfo=m, ...}: t): (string, t) either = let
  val size = case Bencode.access ["info", "length"] m
              of SOME (Bencode.Integer i) => IntInf.toString i
               | _ => "0"
  val u = Fetch.url (URL.toString url) [
      ("compact", b"1")
     ,("info_hash", h)
     ,("left", b size)
     ,("peer_id", peerId)
     ,("port", b"6881") (* TODO make port dynamic, makek peer_id dynamic too *)
     ,("uploaded", b"0")]
in
  Fetch.get u
  >>= Bencode.decode' "peers"
  >>= parsePeers
  >>= (fn p => INR \> updateT t (set#peers p) $)
end

fun getPeers (t: t) : (string, t) either =
  case #protocol (#announceHost t)
   of URL.UDP => raise Fail "REMOVED"
    | _ => getPeersHttp t

fun connectToPeer (peer as {addr,port,...}: peer) : peer option =
  (* socket will be NONE if unable to connect *)
  let val s = INetSock.TCP.socket ()
      val _ = printLn("Connecting to" ^ PolyML.makestring peer)
      val _ = Socket.connect (s, INetSock.toAddr(addr, port))
  in SOME (updatePeer peer (set#socket (SOME s)) $)
  end handle _ => ( printLn ("UNABLE TO CONNECT " ^ PolyML.makestring peer); NONE)

fun sendHandshake infoHash (peer as {socket=(SOME socket), state=PeerProtocol.Fresh,...}) : peer =
  let open PeerProtocol
      val _ = printLn("sending handshake "^PolyML.makestring peer);
      val sent = Socket.sendVec(socket, v (serialize (Handshake(infoHash, peerId))))
      val _ = printLn("sent n bytes: " ^ Int.toString sent)
  in updatePeer peer (set#state HandshakeSent) $
  end
  | sendHandshake _ peer : peer = (
    printLn("Not sending handshake "^PolyML.makestring peer)
  ; peer)

fun receiveHandshake infoHash (peer as {socket=(SOME socket), state=PeerProtocol.HandshakeSent,...}) : peer =
  let open PeerProtocol
      val _ = printLn("receving handshake "^PolyML.makestring peer);
      val received = Socket.recvVec(socket, 68)
  in case parse received
      of (INR(Handshake(hash,peerId))) => updatePeer peer
                                                 (set#state HandshakeReceived)
                                                 (set#peerId peerId) $
       | other => (printLn \> "received other: "^PolyML.makestring other; peer)
  end
  | receiveHandshake _ peer : peer = (
    printLn("Not receiving handshake "^PolyML.makestring peer)
  ; peer)

(*  po 25 pazdziernika *)
fun sendHandshakes (t as {peers,infoHash,...}: t) : (string, t) either =
  let val peers' = List.mapPartial connectToPeer peers >| map (sendHandshake infoHash)
  in INR \> updateT t (set#peers peers') $
  end

fun receiveHandshakes (t as {peers,infoHash,...}: t) : (string, t) either =
  let val peers' = map (receiveHandshake infoHash) peers
  in INR \> updateT t (set#peers peers') $
  end

fun connect (t: t) : (string, t) either =
  getPeers t
  >>= sendHandshakes
  >>= receiveHandshakes

fun recvOne ({socket=NONE,...}) : event option = NONE
  | recvOne ({peerId,socket=(SOME socket),...}) : event option =
    let
      infix 1 >>=
      val op >>= = (fn (pre,post) => Option.mapPartial post pre)
      val op ^ = Bytestring.^
      val join = Option.join
    in
      Socket.recvVecNB(socket, 4)
      >>= (fn lenB => ConvertWord.bytesToWord32B' lenB
      >>= (fn len => Socket.recvVecNB(socket, Word32.toInt len)
      >>= (fn payload =>
              case PeerProtocol.parse (lenB^payload)
               of INR proto => SOME (peerId, proto)
                | _ => NONE)))
  end

fun mkRequests (peerId, indexes) =
  map (fn idx => (peerId, PeerProtocol.Request(idx, 0, 1024*16))) indexes

fun sendToPeer t     _ {socket=NONE,...} = t
  | sendToPeer t proto {socket=SOME socket,...} = let
    val sent = Socket.sendVec(socket, v (PeerProtocol.serialize proto))
    val _ = printLn("sent n bytes: " ^ Int.toString sent ^ "(msg):" ^ PolyML.makestring proto)
  in t
  end

fun sendEvent (event as (peerId, proto), t) =
  case List.find (fn {peerId=p,...} => p = peerId) (#peers t)
   of (SOME peer) => sendToPeer t proto peer
    | NONE => (printLn ("Unknown peer" ^ showEvent event); t)

fun updateState ((peerId, proto) : event, (t: t, outbox: event list)) : (t * event list) = let
  open PeerProtocol
in case proto
    of Keepalive => (t, (peerId, Keepalive)::outbox)
     | Bitfield b => (t, [(peerId, Unchoke) , (peerId, Interested)]
                         @ (mkRequests (peerId, b))
                         @ outbox)
     | Piece(idx, begin, data) => (t, outbox)
     | Choke => (t, outbox) (* TODO handle *)
     | Unchoke => (t, outbox) (* TODO handle *)
     | other => (printLn ("GOT OTHER"^PolyML.makestring other); (t, outbox))
end


fun loop (t: t) : unit =
  let val _ = print "."
      val events = List.mapPartial recvOne (#peers t)
      val _ = app (printLn o showEvent) events
      val (t', outbox) = List.foldl updateState (t,[]) events
      val _ = OS.Process.sleep(Time.fromSeconds 1)
      val t'' = List.foldl sendEvent t' outbox
  in loop t'' end

end (*local*)
end (*struct*)
