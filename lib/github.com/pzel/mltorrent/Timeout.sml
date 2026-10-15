structure Timeout = struct

fun withTimeout (timeout: Time.time) (f: ({} -> 'b option)) : (string, 'b) either = let
  val rt = Timer.startRealTimer ()
  val op > = Time.>
  fun loop () =
    case f()
     of NONE => if Timer.checkRealTimer rt > timeout
                then INL \>
                     "Reached timeout " ^ Time.toString timeout
                else (OS.Process.sleep (Time.fromMilliseconds 5);
                      loop ())
      | SOME result => INR result
in loop ()
end

local val zero = Word8VectorSlice.slice(Word8Vector.fromList[], 0, NONE) in

fun connect (s,addr,t) =
  (if Socket.connectNB (s, addr)
   then INR s
   else case Socket.select{rds=[]
                          ,wrs=[Socket.sockDesc s]
                          ,exs=[]
                          ,timeout=SOME t}
         of {rds=[],wrs=[_],exs=[]} => (Socket.sendVec(s, zero) ; INR s)
          | other => INL ("Socket not ready: " ^ PolyML.makestring other))
  handle (OS.SysErr (e,_)) => INL e
end
end (* struct *)
