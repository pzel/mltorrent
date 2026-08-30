local
  val op == = Assert.eq PolyML.makestring
  structure B = Bencode
  val dec = B.decode o Byte.stringToBytes

in
val bencodeTests = [
  It "decodes a string of length 4"
     (fn _=> dec "4:spam"
                 ==
                 INR (B.String "spam"))
 ,It "decodes a string of length 7"
    (fn _=> dec "7:welcome"
                ==
                INR (B.String "welcome"))
 ,It "decodes a positive integer"
    (fn _=> dec "i345e"
                ==
                INR (B.Integer 345))
 ,It "decodes zero"
    (fn _=> dec "i0e"
                ==
                INR (B.Integer 0))
 ,It "decodes negative integers"
    (fn _=> dec"i-789e"
               ==
               INR (B.Integer ~789))

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

]

end

fun main () =
	runTestsWith bencodeTests (CommandLine.arguments())
