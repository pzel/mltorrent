local
  val op == = Assert.eq PolyML.makestring
  structure B = Bencode
  structure T = Torrent
  val dec = B.decode
in
val bencodeTests = [
  It "decodes a string of length 4"
     (fn _=> dec "4:spam"
             ==
             INR (B.String "spam"))
 ,It "decodes the null string"
     (fn _=> dec "0:" == INR (B.String""))
 ,It "decodes a utf8-encoded string" (* łóżko: 8 bytes encoded *)
     (fn _=> dec ("8:" ^ "\197\130\195\179\197\188\107\111")
             == INR (B.String "\197\130\195\179\197\188ko"))
 ,It "decodes a string of length 7"
     (fn _=> dec "7:welcome"
             == INR (B.String "welcome"))
 ,It "decodes a positive integer"
     (fn _=> dec "i345e"
             == INR (B.Integer 345))
 ,It "decodes zero"
     (fn _=> dec "i0e"
             == INR (B.Integer 0))
 ,It "decodes negative integers"
     (fn _=> dec"i-789e"
             == INR (B.Integer ~789))

 ,It "decodes a list"
     (fn _=> dec"l4:spam4:eggsi34ee"
             == (INR \> B.List [
                   B.String "spam",
                   B.String "eggs",
                   B.Integer 34]))
 ,It "parses dictionaries (example 1)"
     (fn _=> dec"d3:cow3:moo4:spam4:eggse"
             == (INR \> B.Dict [(B.Key "cow", B.String "moo")
                               ,(B.Key "spam", B.String "eggs")]))
 ,It "can show keys of a dictionary"
     (fn _=>
         let val op == = Assert.eq PolyML.makestring (* rebind *)
         in B.keys (B.Dict [(B.Key "cow", B.String "moo")
                           ,(B.Key "spam", B.String "eggs")])
            == ["cow", "spam"]
         end)

]

val torrentTests = [

  It "provides a reasonable error message when file not found"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  val res = T.openTorrent "./test/nonexistentfile"
                  val prefix = Either.mapLeft (fn x=> String.substring(x,0,154)) res
              in prefix == INL ("Failed to open ./test/nonexistentfile\nWith error: SysErr (\"No such file or directory\", SOME ENOENT) ./test/nonexistentfile\nCurrent working directory was: ") (* skip concrete cwd info here *)
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

 ,It "can get announce IPs"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight (#hostname o #announceHost)
            == INR "tracker.opentrackr.org"
         end)

(* <<0,0,4,23,39,16,25,128,0,0,0,0,190,85,94,183>> *)
 ,It "can get a list of peers"
     (fn ()=>
         let val op == = Assert.eq PolyML.makestring
         in T.openTorrent "./test/sample.torrent"
            >| Either.mapRight T.getPeers
            >| Either.mapRight #peers
            == INR []
         end)

     ]

  end

  fun main () =
	    runTestsWith (bencodeTests @ torrentTests) (CommandLine.arguments())
