module Mode exposing (..)


type Mode
    = Decimal
    | Hexadecimal
    | Octal


next : Mode -> Mode
next mode =
    case mode of
        Decimal ->
            Hexadecimal

        Hexadecimal ->
            Octal

        Octal ->
            Decimal


toString : Mode -> String
toString mode =
    case mode of
        Decimal ->
            "Decimal"

        Hexadecimal ->
            "Hexadecimal"

        Octal ->
            "Octal"
