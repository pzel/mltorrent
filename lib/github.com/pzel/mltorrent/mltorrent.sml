structure Bencode = struct
datatype key = Key of string
datatype t = String of string
           | Integer of int
           | List of t list
           | Dict of (key * t) list

type binary = Word8Vector.vector;

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
    in runIt []
    end

fun benString n = manyN n anyChar (String o String.implode)
fun keyString n = manyN n anyChar (Key o String.implode)

val stringParser =
    integer >>= (fn l => (char#":" ) >> (benString l))

val integerParser =
    between (char#"i") (char#"e") integer
            >>=
            (fn i => return (Integer i))

val keyParser =
    integer >>= (fn l => (char#":" ) >> (keyString l))

fun listParser () =
    between (char#"l") (char#"e") (many1 (delay bencodeParser))
            >>=
            (fn l => return (List l))

and dictParser () =
    between (char#"d") (char#"e") (many1 (tupleParser()))
            >>= (fn res => return (Dict res))

and tupleParser () =
    keyParser
        >>= (fn k => bencodeParser()
        >>= (fn v => return (k,v)))

and bencodeParser () =
    stringParser
        <|> integerParser
        <|> (listParser())
        <|> (dictParser())

fun decode (input: binary) : (string, t) either =
    case runParser (bencodeParser ()) (Byte.bytesToString input)
     of Ok v => INR v
      | Err e => INL (PolyML.makestring e)

end
end
