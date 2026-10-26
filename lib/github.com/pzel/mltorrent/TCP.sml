structure TCP = struct
local
  val cc = String.concat
  val zero = Word8VectorSlice.slice(Word8Vector.fromList[], 0, NONE)

  fun connectT (s,addr,t) =
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
                                , Time.toString t
                                , " (",PolyML.makestring other,")"])
    handle (OS.SysErr (e,_)) => INL e;

in

type socket = (INetSock.inet, Socket.active Socket.stream) Socket.sock

fun connect (addr: NetHostDB.in_addr) (port: int) : (string,socket) either =
  connectT (INetSock.TCP.socket (),
            INetSock.toAddr(addr, port),
            Time.fromSeconds 1)


end
end (*struct*)
