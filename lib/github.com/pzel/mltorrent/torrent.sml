structure Torrent = struct
type bencode = Bencode.t

type t = { metaInfo : bencode
         , announceHost : URL.t
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


fun getAnnounce (metaInfo: bencode) : (string, URL.t) either  =
  case Bencode.atKey "announce" metaInfo
   of (SOME (Bencode.String url)) => URL.fromString url
                                     >| Either.fromOption("Couldn't parse announce: "^url)
    | _=> INL "No 'announce' key present in .torrent";


fun binToAddr (input: Bytestring.string) : (string, (NetHostDB.in_addr * int) list) either =
  if Bytestring.size input mod 6 <> 0
  then INL "BAD INPUT LENGTH"
  else
    let open ConvertWord
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
  >>= (fn hash => getAnnounce metaInfo
  >>= (fn url => INR {metaInfo = metaInfo
                      ,announceHost = url
                      ,peers = []
                      ,infoHash = hash})))

fun getAddr ({hostname, port, ...} : URL.t) =
  case NetHostDB.getByName hostname
   of NONE => INL ("Failed to resolve " ^ hostname)
    | SOME addr => INR (INetSock.toAddr (NetHostDB.addr addr, port))

fun getPeersHttp (t as {announceHost=url, infoHash=h, metaInfo=m, ...}: t): (string, t) either = let
  val _ = print "\nGetting HTTP peer list\n"
  val b = Bytestring.fromString
  val size = case Bencode.access ["info", "length"] m
              of SOME (Bencode.Integer i) => IntInf.toString i
               | _ => "0"
  val u = Fetch.url (URL.toString url) [
      ("compact", b"1")
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


fun getPeers (t: t) : (string, t) either =
  case #protocol (#announceHost t)
   of URL.UDP => raise Fail "REMOVED"
    | _ => getPeersHttp t

fun connect (t: t) : (string, t) either =
  getPeers t

end (*local*)

end (*struct*)

fun main () = let val t = hd (CommandLine.arguments())
                  val _ =  PolyML.print_depth 100
              in Torrent.openTorrent t
                 >| Either.bindRight Torrent.connect
                 >| Either.mapRight (tap PolyML.print)
                 >| Either.appLeft print
              end
