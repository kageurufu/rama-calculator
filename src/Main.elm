module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp)
import Colors
import Css
import Css.Global
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (colspan, css, disabled, src, tabindex, type_, width)
import Html.Styled.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode
import Keys exposing (keyDecoder)
import Mode exposing (Mode(..))
import State exposing (Model, Msg(..))
import String
import Types exposing (..)
import Utils exposing (..)


main =
    Browser.document
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( { x = 0
      , y = 0
      , displayMode = Decimal
      , operation = Nothing
      , result = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (Decode.map Keypress keyDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Number { base, val } ->
            if model.operation == Nothing then
                ( { model | x = model.x * base + val, result = Nothing }, Cmd.none )

            else
                ( { model | y = model.y * base + val, result = Nothing }, Cmd.none )

        Operation op ->
            if model.x == 0 then
                ( model, Cmd.none )

            else
                ( { model | operation = Just op, result = Nothing }, Cmd.none )

        Operate ->
            case model.operation of
                Just Addition ->
                    ( { model | x = 0, y = 0, result = Just (model.x + model.y), operation = Nothing }, Cmd.none )

                Just Subtraction ->
                    ( { model | x = 0, y = 0, result = Just (model.x - model.y), operation = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Keypress keyCode ->
            case ( model.displayMode, keyCode ) of
                ( displayMode, Keys.Tab ) ->
                    ( { model | displayMode = Mode.next displayMode }, Cmd.none )

                ( Mode.Decimal, Keys.Input val ) ->
                    update (Number { base = 10, val = val }) model

                ( Mode.Hexadecimal, Keys.Input val ) ->
                    update (Number { base = 16, val = val }) model

                ( Mode.Octal, Keys.Input val ) ->
                    update (Number { base = 8, val = val }) model

                ( _, Keys.Operation op ) ->
                    update (Operation op) model

                ( _, Keys.Equals ) ->
                    update Operate model

                ( _, Keys.Escape ) ->
                    ( { model | x = 0, y = 0, operation = Nothing, result = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


document : Model -> { title : String, body : List (Html.Html Msg) }
document model =
    { title = "Rama Calculator"
    , body =
        [ toUnstyled <|
            div
                [ preventDefaultOn "keydown" (Decode.map (\msg -> ( msg, True )) (Decode.succeed NoOp))
                , tabindex 0
                , css
                    [ Css.pseudoClass "focus"
                        [ Css.outline Css.zero
                        ]
                    ]
                ]
            <|
                view model
        ]
    }


globalStyle : List Css.Global.Snippet
globalStyle =
    [ Css.Global.body
        [ Css.backgroundColor Colors.black
        , Css.color (Css.rgb 255 255 255)
        ]
    , Css.Global.button
        [ Css.disabled [ Css.property "filter" "opacity(0.5)" ]
        ]
    ]


view : Model -> List (Html Msg)
view model =
    let
        modeColor mode =
            if model.displayMode == mode then
                Colors.red

            else
                Colors.black
    in
    [ Css.Global.global globalStyle
    , section
        [ css
            [ Css.outline3 (Css.px 1) Css.solid (modeColor Mode.Decimal)
            , Css.padding (Css.px 5)
            ]
        ]
        [ viewModel viewDecimalNumber viewDecimalOperation model
        , viewInputDecimal model
        ]
    , section
        [ css
            [ Css.outline3 (Css.px 1) Css.solid (modeColor Mode.Hexadecimal)
            , Css.padding (Css.px 5)
            ]
        ]
        [ viewModel viewHexNumber viewHexOperation model
        , viewInputHex model
        ]
    , section
        [ css
            [ Css.outline3 (Css.px 1) Css.solid (modeColor Mode.Octal)
            , Css.padding (Css.px 5)
            ]
        ]
        [ viewModel viewOctNumber viewOctOperation model
        , viewInputOctal model
        ]
    ]


viewModel : (Int -> Html Msg) -> (Operation -> Html Msg) -> Model -> Html Msg
viewModel viewNumber viewOperation model =
    div []
        (case ( model.result, model.operation ) of
            ( Just val, _ ) ->
                [ viewNumber val ]

            ( _, Nothing ) ->
                [ viewNumber model.x ]

            ( _, Just op ) ->
                [ viewNumber model.x
                , viewOperation op
                , viewNumber model.y
                ]
        )


viewDecimalNumber : Int -> Html Msg
viewDecimalNumber val =
    span []
        (List.map (\digit -> img [ src <| decDigit digit ] [])
            (digits 10 val)
        )


viewDecimalOperation : Operation -> Html Msg
viewDecimalOperation op =
    case op of
        Addition ->
            img [ src "img/dec-add.png" ] []

        Subtraction ->
            img [ src "img/dec-sub.png" ] []


viewHexNumber : Int -> Html Msg
viewHexNumber val =
    span [] <|
        List.map (\digit -> img [ src <| hexDigit digit ] [])
            (digits 16 val)


viewHexOperation : Operation -> Html Msg
viewHexOperation op =
    case op of
        Addition ->
            img [ src "img/hex-add.png" ] []

        Subtraction ->
            img [ src "img/hex-sub.png" ] []


viewOctNumber : Int -> Html Msg
viewOctNumber val =
    span []
        (img [ src "img/oct-prefix.png" ] []
            :: List.map (\digit -> img [ src <| octDigit digit ] [])
                (digits 8 val)
        )


viewOctOperation : Operation -> Html Msg
viewOctOperation op =
    case op of
        Addition ->
            img [ src "img/oct-add.png" ] []

        Subtraction ->
            img [ src "img/oct-sub.png" ] []


viewInputDecimal : Model -> Html Msg
viewInputDecimal model =
    let
        myButton val =
            input
                [ type_ "image"
                , src <| decDigit val
                , onClick (Number { base = 10, val = val })
                ]
                [ text (String.fromInt val) ]
    in
    table
        []
        [ tr []
            [ td [] [ myButton 0 ]
            , td [] [ myButton 1 ]
            , td [] [ myButton 2 ]
            , td [] [ myButton 3 ]
            , td [] [ myButton 4 ]
            , td []
                (if model.operation == Nothing then
                    [ unstyledButton [ onClick (Operation Addition) ] [ img [ src "img/dec-add.png" ] [] ] ]

                 else
                    []
                )
            ]
        , tr []
            [ td [] [ myButton 5 ]
            , td [] [ myButton 6 ]
            , td [] [ myButton 7 ]
            , td [] [ myButton 8 ]
            , td [] [ myButton 9 ]
            , td []
                [ if model.operation == Nothing then
                    unstyledButton [ onClick (Operation Subtraction) ] [ img [ src "img/dec-sub.png" ] [] ]

                  else
                    unstyledButton [ onClick Operate ] [ img [ src "img/dec-eq.png" ] [] ]
                ]
            ]
        ]


viewInputHex : Model -> Html Msg
viewInputHex model =
    let
        myButton val =
            unstyledButton [ onClick (Number { base = 16, val = val }) ]
                [ img [ src (hexDigit val) ] [] ]
    in
    table []
        [ tr []
            [ td [] [ myButton 0x00 ]
            , td [] [ myButton 0x01 ]
            , td [] [ myButton 0x02 ]
            , td [] [ myButton 0x03 ]
            , td [] [ myButton 0x04 ]
            , td [] [ myButton 0x05 ]
            , td [] [ myButton 0x06 ]
            , td [] [ myButton 0x07 ]
            , td []
                (if model.operation == Nothing then
                    [ unstyledButton [ onClick (Operation Addition) ] [ img [ src "img/hex-add.png" ] [] ] ]

                 else
                    []
                )
            ]
        , tr []
            [ td [] [ myButton 0x08 ]
            , td [] [ myButton 0x09 ]
            , td [] [ myButton 0x0A ]
            , td [] [ myButton 0x0B ]
            , td [] [ myButton 0x0C ]
            , td [] [ myButton 0x0D ]
            , td [] [ myButton 0x0E ]
            , td [] [ myButton 0x0F ]
            , td []
                (if model.operation == Nothing then
                    [ unstyledButton [ onClick (Operation Subtraction) ] [ img [ src "img/hex-sub.png" ] [] ] ]

                 else
                    [ unstyledButton [ onClick Operate ] [ img [ src "img/hex-eq.png" ] [] ] ]
                )
            ]
        ]


viewInputOctal : Model -> Html Msg
viewInputOctal model =
    let
        myButton val =
            unstyledButton [ onClick (Number { base = 8, val = val }) ]
                [ img [ src (octDigit val) ] []
                ]
    in
    table []
        [ tr []
            [ td []
                [ img [ src "img/oct-prefix.png" ] []
                ]
            , td [] [ myButton 0 ]
            , td [] [ myButton 1 ]
            , td [] [ myButton 2 ]
            , td [] [ myButton 3 ]
            , td [] [ myButton 4 ]
            , td [] [ myButton 5 ]
            , td [] [ myButton 6 ]
            , td [] [ myButton 7 ]
            , td []
                (if model.operation == Nothing then
                    [ unstyledButton [ onClick (Operation Addition) ] [ img [ src "img/oct-add.png" ] [] ]
                    , unstyledButton [ onClick (Operation Subtraction) ] [ img [ src "img/oct-sub.png" ] [] ]
                    ]

                 else
                    [ unstyledButton [ onClick Operate ] [ img [ src "img/oct-eq.png" ] [] ] ]
                )
            ]
        ]


unstyledButton : List (Attribute msg) -> List (Html msg) -> Html msg
unstyledButton attr =
    button
        (attr
            ++ [ type_ "button"
               , css
                    [ Css.borderWidth (Css.px 0)
                    , Css.backgroundColor (Css.rgba 0 0 0 0)
                    , Css.disabled [ Css.property "filter" "opacity(0.5)" ]
                    , Css.hover
                        [ Css.property "filter" "brightness(1.5)" ]
                    ]
               ]
        )
