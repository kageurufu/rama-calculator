module State exposing (Model, Msg(..))

import Keys
import Mode exposing (Mode)
import Types exposing (Operation)


type alias Model =
    { x : Int
    , y : Int
    , displayMode : Mode
    , operation : Maybe Operation
    , result : Maybe Int
    }


type Msg
    = Number { base : Int, val : Int }
    | Operation Operation
    | Operate
    | Keypress Keys.Keycode
    | NoOp
