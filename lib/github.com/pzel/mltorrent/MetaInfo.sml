structure MetaInfo = struct
local
  structure BS = Bytestring
  structure B = Bencode
  type bencode = B.t
  type bytestring = Bytestring.string
  type w32 = Word32.word

  val o2e = Either.fromOption
  fun split b = Vector.tabulate(BS.size b div 20, fn i => BS.substring(b, (i*20), 20))
  fun k key dict = o2e ("parseInfo: Failed to find key: " ^ key)
                       (B.atKey key dict)
  infix 1 >>=
  val op >>= = (fn (pre,post) => Either.bindRight post pre)

in

datatype t =
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

local type i = t; open Bencode in
fun parseInfo (dict: bencode) : (string, i) either=
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
                                          ,path=map stringToBytes bs}
                                     | _ => NONE) vs
  in if length good = length l then SOME good else NONE end
  | parseFiles _ = NONE
end
end

fun pieceLength (SingleFile{pieceLength=l,...}) = Word32.toInt l
  | pieceLength(ManyFiles{pieceLength=l,...}) = Word32.toInt l

fun pieceCount (SingleFile{pieces=p,...}) = Vector.length p
  | pieceCount (ManyFiles{pieces=p,...}) = Vector.length p

fun pieceHashes (SingleFile{pieces=p,...}) = p
  | pieceHashes (ManyFiles{pieces=p,...}) = p

fun fileName (SingleFile{name=n,...}) = Bytestring.toString n
  | fileName (ManyFiles{name=n,...}) = Bytestring.toString n

fun infoFiles (SingleFile{...}) = NONE
  | infoFiles (ManyFiles{files,...}) = SOME files

fun totalLength (info: t) : int =
  case info of (ManyFiles _) => pieceLength info * pieceCount info
             | (SingleFile{length=l,...}) => Word32.toInt l

fun pieceEnd (info: t) (idx: int) =
  if idx + 1  = pieceCount info
  then (totalLength info) mod (pieceLength info)
  else pieceLength info

fun pieceIdx (info: t) (idx: int) (begin: int) : int = pieceLength info*idx + begin

end
