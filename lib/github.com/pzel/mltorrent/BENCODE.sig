signature BENCODE = sig
datatype key = Key of string
datatype t = String of string
           | Integer of IntInf.int
           | List of t list
           | Dict of (key * t) list

val encode : t -> (string, string) either
val decode : string -> (string, t) either
val decode' : string -> string -> (string, t) either
val keys : t -> string list
val atKey : string -> t -> t option
val access : string list -> t -> t option
end
