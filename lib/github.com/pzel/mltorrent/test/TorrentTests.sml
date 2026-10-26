val bs = Bytestring.fromString
structure T = Torrent

val _ = [
  It "provides a reasonable error message when file not found"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  val t = T.openTorrent "./test/nonexistentfile"
                  val prefix = Either.mapLeft (fn x=> String.substring(x,0,154)) t
                  val res = Either.asLeft prefix
              in res
                 == SOME \>
                    "Failed to open ./test/nonexistentfile\n"
                    ^"With error: SysErr (\"No such file or directory\", SOME ENOENT) "
                    ^"./test/nonexistentfile\nCurrent working directory was: "
                       (* skiping concrete cwd info here *)
              end)

 ,It "provides a reasonable error message when tracker is unparseable"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  val res = T.openTorrent "./test/bad.torrent"
                            >| Either.asLeft
              in res == SOME \> "Couldn't parse announce: "
                               ^"uudp://tracker.opentrackr.org:1337/announce"
              end)

 ,It "reads a real info file"
     (fn ()=> case T.openTorrent "./test/local.torrent" of
                  INR _ => succeed "parsed"
                | INL x => Assert.fail x)

 ,It "retains contains all the fields from the file in the rawMeta field"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (B.keys o #rawMeta)
            == INR ["announce", "created by", "creation date", "info"]
         end)

 ,It "contains the url in 'announce'"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             fun join (SOME (SOME x)) = (SOME x)
               | join _ = NONE
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (B.atKey "announce" o #rawMeta)
            >| Either.asRight
            >| join
            == SOME (B.String "http://localhost:6969/announce")
         end)

 ,It "parses info for single file"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             val (INR t) = T.openTorrent "./test/local.torrent"
             val expectedPiece = Word8Vector.fromList[0wxB4, 0wx39, 0wx43, 0wx88, 0wx42, 0wxEC, 0wxC4, 0wx95, 0wx87, 0wx52, 0wx2F, 0wx62, 0wx96, 0wx89, 0wx2F, 0wx10, 0wx46, 0wx67, 0wxA9, 0wx61]
         in (#info t) == MetaInfo.SingleFile {
              name= bs "scratch.png"
             ,length=0w100506
             ,pieceLength=0w262144
             ,pieces= Vector.fromList[expectedPiece]
            }
         end)

 ,It "parses info for multi-file"
     (fn ()=> ((T.openTorrent "./test/sample.torrent" 
               >| Either.mapRight (MetaInfo.infoFiles o #info))
                 === INR (SOME [{length=0w574823, path=[bs"sample_640x360.mp4"]}])) 
                PolyML.makestring)

 ,It "calculates the info_hash"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (Bytestring.toStringHex o #infoHash)
            == INR "6444400e83ddbb7100010718094a4686032fc45b"
         end)

 ,It "can get announce IPs"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (#hostname o #announceHost)
            == INR "localhost"
         end)

 ,It "gets all pieces"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local2.torrent"
            >| Either.mapRight (MetaInfo.pieceCount o #info)
            == INR 2
         end)

 ,It "gets the correct piece hashes (concatenated pieces == original string in info)"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             open MetaInfo
             fun force (INR s) = s | force (_) = raise (Fail "force");
             val t = T.openTorrent "./test/local2.torrent"
             val rawMeta = force \> Either.mapRight #rawMeta t
             val info = force \> Either.mapRight #info t
             val v2l = Vector.foldr op:: []
             val parsed = Bytestring.concat(v2l(pieceHashes info))
             val raw = Option.map Bencode.stringToBytes (Bencode.access ["info", "pieces"] rawMeta)
         in SOME parsed == raw
         end)

 ,It "can get a list of peers (sham non-deterministic data atm)"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local.torrent"
            >| Either.bindRight T.connect
            >| Either.mapRight #peers
            >| Either.map (const false, fn x => List.length x >0)
            >| Either.proj
           == true
         end)

] >| addTests "torrent tests"
