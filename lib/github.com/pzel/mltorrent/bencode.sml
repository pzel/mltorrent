structure Bencode : BENCODE = struct
datatype key = Key of string
datatype t = String of string
           | Integer of IntInf.int
           | List of t list
           | Dict of (key * t) list
exception UnorderedKeys of string list;

local
  infix 1 >>= >>
  infix 1 <*
  infix 4 <*> <$>
  infixr 1 <|>
  infix 0 <?>
  open Parsec
in

fun manyN n p finalizer =
  let fun runIt acc =
        p >>=
        (fn res => if length (res::acc) = n
                   then return (finalizer (rev (res::acc)))
                   else runIt (res::acc))
  in if n = 0
     then return (finalizer [])
     else runIt []
  end

fun benString n = manyN n anyChar (String o String.implode)
fun keyString n = manyN n anyChar (Key o String.implode)

val stringParser =
  integer >>= (fn l => (char#":" ) >> (benString l))

val keyParser =
  integer >>= (fn l => (char#":" ) >> (keyString l))


val intInf = let
  val sign = (char #"~" >> return ~1)
             <|> (char #"-" >> return ~1)
             <|> return 1
  val digits = many1 digit
in
  sign >>= (
  fn sgn =>
     digits >>= (
       fn ds =>
          return (sgn * (valOf (IntInf.fromString (implode ds))))))
end

val integerParser =
  between (char#"i") (char#"e") intInf
  >>= (fn i => return (Integer i))

fun listParser () =
  between (char#"l") (char#"e") (many1 (delay bencodeParser))
  >>= (fn l => return (List l))

and dictParser () =
  between (char#"d") (char#"e") (many1 (tupleParser()))
  >>= (fn res => return (Dict res))

and tupleParser () =
  keyParser
  >>= (fn k => bencodeParser()
               >>= (fn v => return (k,v)))

and bencodeParser () =
  (try (dictParser()))
  <|> (try stringParser)
  <|> (try integerParser)
  <|> (try (listParser()))


local
  fun doEnc (String s) = Int.toString (String.size s) ^ ":" ^ s
    | doEnc (Integer i) = if i < 0
                          then "i-" ^ (IntInf.toString (~i)) ^ "e"
                          else "i" ^ (IntInf.toString i) ^ "e"
    | doEnc (List i) = concat (("l" :: (map doEnc i)) @ ["e"])
    | doEnc (Dict i) = (check i; concat (("d" :: (map doEncKv i)) @ ["e"]))
  and doEncKv (Key i, value) = doEnc (String i) ^ doEnc value
  and check [] = raise UnorderedKeys []
    | check ((Key _, _)::[]) = ()
    | check ((Key k, _)::(Key j, nxt)::rest) =
      if String.compare(k, j) = GREATER
      then raise UnorderedKeys [k,j]
      else check ((Key j, nxt)::rest)
in
fun encode vs =
  INR (doEnc vs)
  handle (UnorderedKeys keys) =>
         INL \> concat [ "Unordered keys: " ^ String.concatWith "," keys]
end


fun decode' (subject: string) (input: string) : (string, t) either =
  let fun trim i = String.extract(i, 0, SOME (Int.min(80,String.size i)))
  in case runParser (bencodeParser ()) input
     of Ok v => INR v
      | Err e => INL ("Error decoding "
                      ^ subject ^ ": "
                      ^ Parsec.errorToString e
                      ^ "\nfrom: " ^ trim input)
  end

val decode = decode' ""

fun keys (Dict kv) = map (fn (Key s, _) => s) kv
  | keys _ = []

fun atKey k (Dict kv) = search k kv
  | atKey _ _ = NONE
and search _ [] = NONE
  | search k ((Key j, v)::rest) = if k = j
                                  then SOME v
                                  else search k rest
and access [] _ = NONE
  | access [k] v = atKey k v
  | access (k::ks) v = case atKey k v
                        of SOME newV => access ks newV
                         | _ => NONE

end
end (*local*)
