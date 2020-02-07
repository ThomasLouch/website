module View exposing (view)

import Canvas exposing (Renderable, Shape, rect, shapes)
import Canvas.Settings exposing (..)
import Color
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Model exposing (..)


view : Model -> Html Msg
view model =
    let wrapper = \body -> div [] [ body, footerView ] in
    case model.gameState of
        Playing -> wrapper (gameView model)
        Paused -> wrapper (gameView model)
        GameOver -> wrapper (gameOverView model)

footerView : Html Msg
footerView =
    footer []
        [ em []
            [ a [ href "https://www.github.com/ThomasLouch", target "_blank" ] [ text "Github" ]
            -- , span [] [ text " // " ]
            -- , a [ href "http://linkedin.com", target "_blank" ] [ text "Linkedin" ]
            ]
        ]

gameOverView : Model -> Html Msg
gameOverView model =
    div [ class "container" ]
        [ gameBoardView model
        , instructionsView model
        ]

instructionsView : Model -> Html Msg
instructionsView model = 
    div [ class "instructions" ]
        [ em [] [ text "WASD space - "
        , span [] [ model.score |> String.fromInt |> text ] ]
        ]

gameView : Model -> Html Msg
gameView model =
    div [ class "container" ]
        [ gameBoardView model
        , instructionsView model
        ]

gameBoardView : Model -> Html Msg
gameBoardView model =
    Canvas.toHtml ( gameSize.x, gameSize.y )
        []
        [ shapes
            [ fill (Color.rgb255 0x44 0x44 0x44) ]
            [ rect ( 0, 0 ) (toFloat gameSize.x) (toFloat gameSize.y) ]
        , snakeView model
        , foodView model
        ]

foodView : Model -> Renderable
foodView model =
    shapes [ fill (Color.rgb255 0xFF 0x47 0x5D) ]
        (List.map (\seg -> pointToRectangle model seg) model.foods)

snakeView : Model -> Renderable
snakeView model =
    shapes [ fill (Color.rgb255 0xF1 0xCB 0xFF) ]
        (List.map (\seg -> pointToRectangle model seg) model.snake)

pointToRectangle : Model -> Point -> Shape
pointToRectangle model point =
    let cellWidth = toFloat gameSize.x / toFloat model.options.gameBoardSize
        cellHeight = toFloat gameSize.x / toFloat model.options.gameBoardSize
        pixelsX = toFloat point.x * cellWidth
        pixelsY = toFloat point.y * cellHeight
    in
    rect ( pixelsX, pixelsY ) cellWidth cellHeight

gameSize : Point
gameSize = Point 650 650
