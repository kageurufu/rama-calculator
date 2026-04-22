module Utils exposing (decDigit, digits, hexDigit, octDigit)


decDigit : Int -> String
decDigit val =
    if val == -1 then
        "img/dec-sub.png"

    else
        "img/dec-" ++ String.fromInt val ++ ".png"


hexDigit : Int -> String
hexDigit val =
    if val == -1 then
        "img/dec-sub.png"

    else
        "img/hex-" ++ String.fromInt val ++ ".png"


octDigit : Int -> String
octDigit val =
    if val == -1 then
        "img/dec-sub.png"

    else
        "img/oct-" ++ String.fromInt val ++ ".png"


digits : Int -> Int -> List Int
digits base val =
    -- Take a list of characters for a base, return a list of characters for the digits in the number
    if val < 0 then
        -1 :: digits base (-1 * val)

    else if val < base then
        [ val ]

    else
        digits base (val // base) ++ [ modBy base val ]
