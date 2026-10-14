fun main () = let
  val t = hd (CommandLine.arguments())
  val _ =  PolyML.print_depth 100
in Torrent.openTorrent t
   >| Either.bindRight Torrent.connect
   >| Either.mapLeft (fn x => (print x; ignore (OS.Process.exit OS.Process.failure)))
   >| Either.appRight Torrent.loop
end handle e => (printLn \> exnMessage e; OS.Process.exit OS.Process.failure)
