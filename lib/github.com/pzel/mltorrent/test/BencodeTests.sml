structure B = Bencode
val dec = B.decode
val enc = B.encode

local  val op == = Assert.eq PolyML.makestring in
val bdecodeTests = [
  It "decodes a string of length 4"
     (fn _=> dec "4:spam" == INR (B.String "spam"))
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
 ,It "can get nested dictionary values"
     (fn _=>
         let val op == = Assert.eq PolyML.makestring (* rebind *)
             val inner = B.Dict [(B.Key "inner", B.String "i")]
             val outer = B.Dict [(B.Key "outer", inner)]
         in B.access ["outer", "inner"] outer == SOME (B.String "i")
         end)
 ,It "can get nested dictionary values (not present)"
     (fn _=>
         let val op == = Assert.eq PolyML.makestring (* rebind *)
             val inner = B.Dict [(B.Key "inner", B.String "i")]
             val outer = B.Dict [(B.Key "outer", inner)]
         in B.access ["outer", "foo"] outer == NONE
         end)
 ,It "can get nested dictionary values (singleton)"
     (fn _=>
         let val op == = Assert.eq PolyML.makestring (* rebind *)
             val inner = B.Dict [(B.Key "inner", B.String "i")]
             val outer = B.Dict [(B.Key "outer", inner)]
         in B.access ["outer"] outer == SOME inner
         end)
] end


local val op == = Assert.eq PolyML.makestring in
val bencodeTests = [
 It "encodes a string of length 4"
     (fn _=> enc (B.String "spam") == INR "4:spam")
 ,It "encodes a utf8-encoded string" (* łóżko: 8 bytes encoded *)
     (fn _=> enc (B.String "\197\130\195\179\197\188ko")
                 == INR "8:\197\130\195\179\197\188\107\111")
 ,It "encodes a positive integer"
     (fn _=> enc (B.Integer 345) == INR "i345e")
 ,It "encodes zero"
     (fn _=> enc (B.Integer 0) == INR "i0e")
 ,It "encodes negative integers"
     (fn _=> enc (B.Integer ~789) == INR "i-789e")
 ,It "encodes a list"
     (fn _=> enc (B.List [
                   B.String "spam",
                   B.String "eggs",
                   B.Integer 34]) == INR "l4:spam4:eggsi34ee")
,It "encodes a dictionary"
     (fn _=> enc (B.Dict [(B.Key "cow", B.String "moo")
                         ,(B.Key "spam", B.String "eggs")])
             == INR "d3:cow3:moo4:spam4:eggse")
,It "fails to encode a mis-ordered dictionary"
     (fn _=> enc (B.Dict [(B.Key "cow", B.String "moo")
                         ,(B.Key "alpha", B.String "beta")])
             == INL "Unordered keys: cow,alpha")
] end
val _ = addTests "bencode-encode" bencodeTests
val _ = addTests "bencode-decode" bdecodeTests

