structure Timeout = struct

local
  val zero = Word8VectorSlice.slice(Word8Vector.fromList[], 0, NONE)
  val cc = String.concat
in

fun withTimeout (timeout: Time.time) (f: ({} -> 'b option)) : (string, 'b) either = let
  val rt = Timer.startRealTimer ()
  val op > = Time.>
  fun loop () =
    case f()
     of NONE => if Timer.checkRealTimer rt > timeout
                then INL \> cc["Reached timeout ", Time.toString timeout]
                else (OS.Process.sleep (Time.fromMilliseconds 5);
                      loop ())
      | SOME result => INR result
in loop ()
end

fun connect (s,addr,t) =
  (if Socket.connectNB (s, addr)
   then INR s
   else case Socket.select{rds=[]
                          ,wrs=[Socket.sockDesc s]
                          ,exs=[]
                          ,timeout=SOME t}
         of {rds=[],wrs=[_],exs=[]} => (ignore \> Socket.sendVec(s, zero) ; INR s)
          | other => INL \> cc["Socket to "
                              , NetHostDB.toString(#1(INetSock.fromAddr addr))
                              , ":" , Int.toString(#2(INetSock.fromAddr addr))
                              , " not ready after timeout: "
                              , PolyML.makestring t
                              , " (",PolyML.makestring other,")"])
  handle (OS.SysErr (e,_)) => INL e
end
end (* struct *)
