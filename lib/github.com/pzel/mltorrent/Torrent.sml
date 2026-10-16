structure Torrent = struct
type bencode = Bencode.t
type bytestring = Bytestring.string
type tcp_socket = (INetSock.inet, Socket.active Socket.stream) Socket.sock
type w32 = Word32.word

datatype info =
         SingleFile of { name: bytestring
                    , pieceLength: w32
                    , pieces: bytestring vector
                    , length : w32
                    }
       | ManyFiles of { name: bytestring
                   , pieceLength: w32
                   , pieces: bytestring vector
                   , files: ({ length : w32, path: bytestring list} list)
                   }

type peer = { addr: NetHostDB.in_addr
            , port: int
            , pieces: w32 list
            , peerId: bytestring
            , state: PeerProtocol.state
            , socket: tcp_socket option
            }

val updatePeer =
fn z =>
   let fun from a po pi pid st so = {addr=a,port=po,pieces=pi,peerId=pid,state=st,socket=so}
       fun to f {addr=a,port=po,pieces=pi,peerId=pid,state=st,socket=so} = f a po pi pid st so
   in FRU.makeUpdate6 (from, from, to) end z;

(* todo unhardcode *)
val PEER_ID = Bytestring.fromString "000000000000000simon"
val PIECE_SIZE = 1024*16

type t = { metaInfo : bencode
         , announceHost : URL.t
         , peers : peer list
         , info : info
         , infoHash : bytestring
         , downloaded : Word8Array.array (* should be on disk or mmaped? *)
         , completed : bool Array.array
         }

val updateT =
fn z =>
   let fun from m a p i ih d cp = {
         metaInfo=m,announceHost=a,peers=p,info=i,infoHash=ih,downloaded=d,completed=cp}
       fun to f {
         metaInfo=m,announceHost=a,peers=p,info=i,infoHash=ih,downloaded=d,completed=cp} =
         f m a p i ih d cp
   in FRU.makeUpdate7 (from, from, to) end z;

type event = (bytestring * PeerProtocol.t)
fun showEvent (peerId, event:PeerProtocol.t) = (Bytestring.toStringHex peerId ^":" ^PeerProtocol.compactString event)

local
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)
  fun $(a,f) = f a
  val set = FRU.set
  val v = Word8VectorSlice.full
  val b = Bytestring.fromString
  val i2s = Int.toString
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


(* info-related stuff needs its own module *)
local
  open Bencode
  structure B = Bytestring
  val o2e = Either.fromOption
  fun split b = Vector.tabulate(B.size b div 20, fn i => B.substring(b, (i*20), 20))
  fun k key dict = o2e ("parseInfo: Failed to find key: " ^ key)
                       (atKey key dict)
in
fun parseInfo (dict: Bencode.t) : (string, info) either =
  parseSingle dict >| Either.bindLeft (fn _ => parseMulti dict)
and parseSingle d =
  k"name"d
  >>= (fn n => k"length"d
  >>= (fn l => k"piece length"d
  >>= (fn pl => k"pieces"d
  >>= (fn pcs =>
          case (n,l,pl,pcs)
           of (String n, Integer l, Integer pl, String pcs) =>
              INR \> SingleFile { name= Bytestring.fromString n
                                , length=Word32.fromLargeInt l
                                , pieceLength=Word32.fromLargeInt pl
                                , pieces = split(Bytestring.fromString pcs)
                                }
            | _ => INL "Malformed info data"))))
and parseMulti d =
  k"name"d
  >>= (fn n => Either.mapRight (parseFiles) (k"files"d)
  >>= (fn files => k"piece length"d
  >>= (fn pl => k"pieces"d
  >>= (fn pcs =>
          case (n,files,pl,pcs)
           of (String n, SOME files, Integer pl, String pcs) =>
              INR \> ManyFiles { name= Bytestring.fromString n
                                , files=files
                                , pieceLength=Word32.fromLargeInt pl
                                , pieces = split(Bytestring.fromString pcs)
                                }
            | _ => INL "Malformed info data"))))
and parseFiles (List(l)) =
  let val vs = map (fn (d) => (k"length"d, k"path"d)) l
      val good = List.mapPartial (fn (INR (Integer i), INR (List bs)) =>
                                     SOME {length=Word32.fromLargeInt i
                                          ,path=map (stringToBytes) bs}
                                     | _ => NONE) vs
  in if length good = length l then SOME good else NONE end
  | parseFiles _ = NONE
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
                                         o i2s
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


fun pieceLength(SingleFile{pieceLength=l,...}) = Word32.toInt l
  | pieceLength(ManyFiles{pieceLength=l,...}) = Word32.toInt l

fun pieceCount(SingleFile{pieces=p,...}) = Vector.length p
  | pieceCount(ManyFiles{pieces=p,...}) = Vector.length p

fun pieceHashes(SingleFile{pieces=p,...}) = p
  | pieceHashes(ManyFiles{pieces=p,...}) = p

fun fileName(SingleFile{name=n,...}) = Bytestring.toString n
  | fileName(ManyFiles{name=n,...}) = Bytestring.toString n

fun infoFiles(SingleFile{...}) = NONE
  | infoFiles(ManyFiles{files,...}) = SOME files

fun totalLength (info: info) : int =
  case info of (ManyFiles _) => pieceLength info * pieceCount info
             | (SingleFile{length=l,...}) => Word32.toInt l

fun pieceEnd (info: info) (idx: int) =
  if idx + 1  = pieceCount info
  then (totalLength info) mod (pieceLength info)
  else pieceLength info

fun hashPiece (t as {downloaded, info,...}:t) (idx : int) =
  Bytestring.substring(Word8Array.vector downloaded
                      , idx*pieceLength info
                      , pieceEnd info idx)
  >| SHA1.hashBytes


fun pieceIdx (info: info) (idx: int) (begin: int) : int = pieceLength info*idx + begin

fun writeFile (t as ({info,downloaded,...}: t)) =
    let val os = BinIO.openOut (fileName info)
    in (BinIO.output(os,Word8Array.vector downloaded)
       ; BinIO.closeOut os
       ; printLn \> "Wrote file " ^ fileName info
       ; OS.Process.exit OS.Process.success
       )
       handle X => (BinIO.closeOut os; raise X)
    end

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
               ,infoHash = h
               ,downloaded = Word8Array.array(totalLength i,0w0)
               ,completed = Array.array(totalLength i, false)
      })))))

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
     ,("peer_id", PEER_ID)
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
      val p = (Timeout.connect (s, INetSock.toAddr(addr, port), Time.fromSeconds 1)
              >>= (fn sock => INR (updatePeer peer (set#socket (SOME sock)) $)))
  in case p of
         (INR p) => SOME p
       | (INL e) => (printLn \> "CONNECT ERROR: "^e^"\nDROPPING PEER "^PolyML.makestring peer
                    ; NONE)
  end

fun sendHandshake infoHash (peer as {socket=(SOME socket), state=PeerProtocol.Fresh,...}) : peer =
  let open PeerProtocol
      val _ = printLn("sending handshake "^PolyML.makestring peer);
      val sent = Socket.sendVec(socket, v (serialize (Handshake(infoHash, PEER_ID))))
      val _ = printLn("sent n bytes: " ^ i2s sent)
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
  >>= (fn x=> (printLn (PolyML.makestring (#info x)); INR x))

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
                | other => (printLn(String.^("failed recv ",PolyML.makestring other)); NONE))))
  end

fun mkRequests (peerId, indexes) =
  map (fn idx => (peerId, PeerProtocol.Request(idx, 0, PIECE_SIZE))) indexes

fun sendToPeer t     _ {socket=NONE,...} = t
  | sendToPeer t proto {socket=SOME socket,peerId,...} = let
    val sent = Socket.sendVec(socket, v (PeerProtocol.serialize proto))
    val _ = printLn("OUT: "^showEvent (peerId, proto))
  in t
  end

fun sendEvent (event as (peerId, proto), t) =
  case List.find (fn {peerId=p,...} => p = peerId) (#peers t)
   of (SOME peer) => sendToPeer t proto peer
    | NONE => (printLn ("Unknown peer" ^ showEvent event); t)

fun checkCompletion (t as{info,downloaded,completed,...}:t) idx =
  let val nComplete = Array.foldr (fn (x,s) => if x then s+1 else s) 0 completed
      val allComplete = nComplete = pieceCount info
      val _ = printLn\> "Downloaded piece: " ^ i2s idx
      val _ = printLn\> String.concat[ "Completed "
                                     , i2s nComplete
                                     , "/"
                                     , i2s (pieceCount info)
                                     , " pieces"]
  in if allComplete
     then writeFile t
     else ()
  end


fun handlePiece (t as {downloaded,info,completed,...}) outbox (peerId, PeerProtocol.Piece(idx,begin,data)) =
  let val di = pieceIdx info idx begin
      val _ = Word8Array.copyVec{src=data,dst=downloaded,di=di}
      val endPoint = begin+(Bytestring.size data)
      val nextSize = Int.min(pieceEnd info idx - endPoint, PIECE_SIZE)
      val (outMsg, newHash) =
        if nextSize <> 0
        then ([(peerId, PeerProtocol.Request(idx, begin+PIECE_SIZE, nextSize))], NONE)
        else ([], SOME (hashPiece t idx))
  in if newHash = SOME (Vector.sub(pieceHashes info,idx))
     then (Array.update(completed, idx, true)
          ; checkCompletion t idx
          ; (t, outbox)) (* TODO send out HAVE *)
     else (t, outMsg @ outbox)
  end

fun updateState ((peerId, proto) : event, (t: t, outbox: event list)) : (t * event list) = let
  open PeerProtocol
in case proto
    of Keepalive => (t, (peerId, Keepalive)::outbox)
     | Bitfield b => (t, [(peerId, Unchoke) , (peerId, Interested)]
                         @ (mkRequests (peerId, b))
                         @ outbox)
     | Piece(idx, begin, data) => handlePiece t outbox (peerId, proto)
     | Choke => (t, outbox) (* TODO handle *)
     | Unchoke => (t, outbox) (* TODO handle *)
     | other => (printLn ("GOT OTHER"^PolyML.makestring other); (t, outbox))
end

fun loop (t: t) : unit =
  let (*  val _ = print "." *)
      val events = List.mapPartial recvOne (#peers t)
      val _ = app ((fn ev => printLn("IN:"^ev)) o showEvent) events
      val (t', outbox) = List.foldl updateState (t,[]) events
      val _ = if null events
              then OS.Process.sleep(Time.fromMilliseconds 100)
              else ()
      val t'' = List.foldl sendEvent t' outbox
  in loop t'' end

end (*local*)
end (*struct*)
