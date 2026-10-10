fun main () = let val t = hd (CommandLine.arguments())
                  val _ =  PolyML.print_depth 100
              in Torrent.openTorrent t
                 >| Either.bindRight Torrent.connect
                 >| Either.mapRight (tap PolyML.print)
                 >| Either.appLeft print
              end
