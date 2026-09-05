
fun withTimeout (timeout: Time.time) (f: ({} -> 'b option)) : (string, 'b) either = let
  val rt = Timer.startRealTimer ()
  fun loop () = case f()
                 of NONE => if Timer.checkRealTimer rt > timeout
                            then INL \>
                                 "Reached timeout " ^ Time.toString timeout
                            else (OS.Process.sleep (Time.fromMilliseconds 5);
                                  loop ())
                  | SOME result => INR result
in loop ()
end
