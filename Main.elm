module Main exposing (main)

import Browser exposing (element)
import Browser.Events
import Json.Decode as Decode
import Model exposing (..)
import Time exposing (every)
import Update exposing (update)
import View exposing (view)


main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg )
init flags =
    let model =
            { snake = []
            , currentDirection = Down
            , nextDirections = []
            , foods = []
            , gameState = GameOver
            , options = Options 31
            , score = 0
            }
    in ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    let delay = 100 in
    case model.gameState of
        Playing ->
            Sub.batch
                [ every delay Tick
                , Browser.Events.onKeyDown keyDecoder
                ]
        _ -> Browser.Events.onKeyDown keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyDown
