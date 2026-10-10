local open URL in
val urlTests = [
  It "parses UDP host correctly"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = URL.fromString "udp://foo.bar.com:5678"
              in res == SOME {protocol=UDP, hostname="foo.bar.com",
                              port=5678, path=""}
              end)

 ,It "parses HTTP host correctly (with default port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = URL.fromString "http://foo.bar.com/announce"
              in res == SOME {protocol=HTTP, hostname="foo.bar.com",
                              port=80, path="announce"}
              end)

 ,It "parses HTTP host correctly (with explicit port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = URL.fromString "http://foo.bar.com:1234/announce"
              in res == SOME {protocol=HTTP, hostname="foo.bar.com",
                              port=1234, path="announce"}
              end)

 ,It "parses HTTPS host correctly (with default port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = URL.fromString "https://foo.bar.com/announce.php"
              in res == SOME {protocol=HTTPS, hostname="foo.bar.com",
                              port=443, path="announce.php"}
              end)

 ,It "parses HTTPS host correctly (with explicit port)"
     (fn ()=> let val op == = Assert.eq PolyML.makestring
                  open Torrent
                  val res = URL.fromString "https://foo.bar.com:1234/announce"
              in res == SOME {protocol=HTTPS, hostname="foo.bar.com",
                              port=1234, path="announce"}
              end)

]
end
val _ = addTests "url tests" urlTests
