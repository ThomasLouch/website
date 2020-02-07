module Model exposing (..)

import Time exposing (Posix)


type Msg
    = Tick Posix
    | KeyDown String
    | NewFood Int
    | StartGame
    | ChangeDirection Direction
    | TogglePause

type Direction
    = Up
    | Down
    | Left
    | Right

type GameState
    = Playing
    | Paused
    | GameOver

type alias Options = { gameBoardSize : Int }

type alias Point =
    { x : Int
    , y : Int
    }

type alias Model =
    { snake : List Point
    , currentDirection : Direction
    , nextDirections : List Direction -- last item in list is next direction
    , foods : List Point
    , gameState : GameState
    , options : Options
    , score : Int
    }
