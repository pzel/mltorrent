structure T = Torrent

val torrentTests = [
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

 ,It "parses UDP host correctly"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = parseUrl "udp://foo.bar.com:5678"
              in res == SOME {protocol=UDP, hostname="foo.bar.com",
                              port=5678, path=""}
              end)

 ,It "parses HTTP host correctly (with default port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = parseUrl "http://foo.bar.com/announce"
              in res == SOME {protocol=HTTP, hostname="foo.bar.com",
                              port=80, path="announce"}
              end)

 ,It "parses HTTP host correctly (with explicit port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = parseUrl "http://foo.bar.com:1234/announce"
              in res == SOME {protocol=HTTP, hostname="foo.bar.com",
                              port=1234, path="announce"}
              end)

 ,It "parses HTTPS host correctly (with default port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = parseUrl "https://foo.bar.com/announce.php"
              in res == SOME {protocol=HTTPS, hostname="foo.bar.com",
                              port=443, path="announce.php"}
              end)

 ,It "parses HTTPS host correctly (with explicit port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = parseUrl "https://foo.bar.com:1234/announce"
              in res == SOME {protocol=HTTPS, hostname="foo.bar.com",
                              port=1234, path="announce"}
              end)

 ,It "reads a real info file"
     (fn ()=> case T.openTorrent "./test/sample.torrent" of
                  INR _ => succeed "parsed"
                | INL x => Assert.fail x)

 ,It "contains all the fields in the file"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight (B.keys o #metaInfo)
            == INR ["announce", "created by", "creation date", "info"]
         end)

 ,It "contains the url in 'announce'"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             fun join (SOME (SOME x)) = (SOME x)
               | join _ = NONE
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight (B.atKey "announce" o #metaInfo)
            >| Either.asRight
            >| join
            == SOME (B.String "http://tracker.opentrackr.org:1337/announce")
         end)

 ,It "contains the info dict in 'info'"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
             fun join (SOME (SOME x)) = (SOME x)
               | join _ = NONE
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight (B.atKey "info" o #metaInfo)
            >| Either.asRight
            >| join
            >| Option.map B.keys
            == SOME ["files", "name", "piece length", "pieces", "private"]
         end)

 ,It "calculates the info_hash"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight (Bytestring.toStringHex o #infoHash)
            == INR "4240f5eb1bcd5f847fd1f636c5341c44ef7449e7"
         end)


 ,It "can get announce IPs"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight (#hostname o #announceHost)
            == INR "tracker.opentrackr.org"
         end)

 ,It "can get a list of peers (sham non-deterministic data atm)"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/sample.torrent"
            >| Either.bindRight T.connect
            >| Either.mapRight #peers
            >| Either.map (const 0, List.length)
            >| Either.proj
           == 2
         end)

     ]
val _ = addTests "torrent tests" torrentTests


