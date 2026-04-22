module Keys exposing (Keycode(..), keyDecoder, keyToKeycode)

import Json.Decode as Decode
import Types exposing (Operation(..))


type Keycode
    = Input Int
    | Operation Operation
    | Equals
    | Escape
    | Tab
    | Unknown String


keyDecoder : Decode.Decoder Keycode
keyDecoder =
    Decode.map keyToKeycode <|
        Decode.field "key" Decode.string


keyToKeycode : String -> Keycode
keyToKeycode key =
    case key of
        "0" ->
            Input 0x00

        "1" ->
            Input 0x01

        "2" ->
            Input 0x02

        "3" ->
            Input 0x03

        "4" ->
            Input 0x04

        "5" ->
            Input 0x05

        "6" ->
            Input 0x06

        "7" ->
            Input 0x07

        "8" ->
            Input 0x08

        "9" ->
            Input 0x09

        "a" ->
            Input 0x0A

        "b" ->
            Input 0x0B

        "c" ->
            Input 0x0C

        "d" ->
            Input 0x0D

        "e" ->
            Input 0x0E

        "f" ->
            Input 0x0F

        "+" ->
            Operation Addition

        "-" ->
            Operation Subtraction

        "=" ->
            Equals

        "Enter" ->
            Equals

        "Tab" ->
            Tab

        "Escape" ->
            Escape

        _ ->
            Unknown key
