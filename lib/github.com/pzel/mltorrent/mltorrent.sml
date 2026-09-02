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
type t = { metaInfo : bencode,
           announceHost : NetHostDB.in_addr list,
           peers : NetHostDB.in_addr list
         }
datatype protocol = UDP | HTTP
type host = {protocol: protocol,
             hostname: string,
             port: int}

fun parseInfo (filePath: string) : (string, bencode) either =
    Bencode.decode (TextIO.inputAll (TextIO.openIn filePath))
    handle (IO.Io {cause, name,...}) => INL ("Failed to open "
                                             ^ filePath
                                             ^ "\nWith error: "
                                             ^ exnMessage cause ^ " " ^ name
                                             ^"\nCurrent working directory was: "
                                             ^ Posix.FileSys.getcwd())

fun parseUrl (unparsed: string) : host option =
    if (* (String.isPrefix "udp://" unparsed)
       orelse *)
       (String.isPrefix "http://" unparsed)
    then let val len = String.size unparsed - 7
             val rest = String.substring(unparsed, 7, len)
             val sep = fn c => c = #":" orelse c = #"/"
             val fields = String.fields sep rest
             val ints = map Int.fromString fields
         in case (fields, ints) of
                ((host::_), (NONE :: SOME port :: _)) => SOME {protocol=HTTP,
                                                               hostname=host,
                                                               port=port}
              | _ => NONE
         end
    else NONE

fun getHostAddr (metaInfo: bencode) : (string, NetHostDB.in_addr list) either  =
    case Bencode.atKey "announce" metaInfo
     of (SOME (Bencode.String url)) =>
        Option.mapPartial (NetHostDB.getByName o #hostname) (parseUrl url)
        >| Option.map NetHostDB.addrs
        >| Either.fromOption ("Couldn't resolve host: " ^ url)
      | _=> INL "No 'announce' key present in .torrent";

local
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)
in
fun openTorrent (filePath: string) : (string, t) either =
    (parseInfo filePath)
    >>= (fn metaInfo => getHostAddr metaInfo
    >>= (fn addrs => INR {metaInfo = metaInfo,
                          announceHost = addrs,
                          peers = []}))

(*
connect request:

Offset  Size            Name            Value
0       64-bit integer  protocol_id     0x41727101980 // magic constant
8       32-bit integer  action          0 // connect
12      32-bit integer  transaction_id
16

*)


fun newTxnId () =
    LargeWord.fromLargeInt (Time.toMilliseconds(Time.now ()));

fun connectMsg () : Word8ArraySlice.slice =
    let
      val buffer = Word8Array.array(16, 0w0);
      val _ = PackWord32Big.update(buffer, 0, 0wx417);
      val _ = PackWord32Big.update(buffer, 1, 0wx27101980);
      val _ = PackWord32Big.update(buffer, 3, newTxnId());
    in Word8ArraySlice.full buffer
    end

fun getPeers (t as {announceHost as (addr::_), ...}) = let
  val toAddr = INetSock.toAddr (addr, 1337)
  val fromAddr = INetSock.any 1337
  val sock = INetSock.UDP.socket ()
  val _ = print(PolyML.makestring (connectMsg()) ^ "\n")
  val _ = Socket.bind(sock, fromAddr)
  val _ = Socket.Ctl.setREUSEADDR(sock, true)
  val _ = Socket.sendArrTo (sock, toAddr, connectMsg())
  val (response, sock') = Socket.recvVecFrom (sock, 1024)
  val _ = print (PolyML.makestring response)
in
  t
end

end

end
