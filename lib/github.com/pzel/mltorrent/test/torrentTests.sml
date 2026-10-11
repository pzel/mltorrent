structure T = Torrent

val _ = [
  It "provides a reasonable error message when file not found"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  val res = T.openTorrent "./test/nonexistentfile"
                  val prefix = Either.mapLeft (fn x=> String.substring(x,0,154)) res
              in prefix
                 == INL \>
                    "Failed to open ./test/nonexistentfile\n"
                    ^"With error: SysErr (\"No such file or directory\", SOME ENOENT) "
                    ^"./test/nonexistentfile\nCurrent working directory was: "
                       (* skiping concrete cwd info here *)
              end)

 ,It "provides a reasonable error message when tracker is unparseable"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  val res = T.openTorrent "./test/bad.torrent"
              in res == INL \> "Couldn't parse announce: "
                               ^"uudp://tracker.opentrackr.org:1337/announce"
              end)


 ,It "reads a real info file"
     (fn ()=> case T.openTorrent "./test/local.torrent" of
                  INR _ => succeed "parsed"
                | INL x => Assert.fail x)

 ,It "contains all the fields in the file"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (B.keys o #metaInfo)
            == INR ["announce", "created by", "creation date", "info"]
         end)

 ,It "contains the url in 'announce'"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             fun join (SOME (SOME x)) = (SOME x)
               | join _ = NONE
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (B.atKey "announce" o #metaInfo)
            >| Either.asRight
            >| join
            == SOME (B.String "http://localhost:6969/announce")
         end)

 ,It "contains the info dict in 'info'"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             fun join (SOME (SOME x)) = (SOME x)
               | join _ = NONE
         in T.openTorrent "./test/local.torrent"
            >| Either.mapRight (B.atKey "info" o #metaInfo)
            >| Either.asRight
            >| join
            >| Option.map B.keys
            == SOME ["length", "name", "piece length", "pieces"]
         end)

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

 ,It "can get a list of peers (sham non-deterministic data atm)"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/local.torrent"
            >| Either.bindRight T.connect
            >| Either.mapRight #peers
            >| Either.map (const 0, List.length)
            >| Either.proj
           == 2
         end)

] >| addTests "torrent tests"



