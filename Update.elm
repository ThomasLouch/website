module Update exposing (update)

import Array
import Model exposing (..)
import Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ -> gameTick model

        KeyDown keyCode ->
            let maybeMessage = handleKeyPress keyCode model in
            case maybeMessage of
                Just nextMsg -> update nextMsg model
                Nothing -> ( model, Cmd.none )

        NewFood index ->
            let emptyCells = findEmptyCells model
                newFood =
                    Maybe.withDefault
                        (Point 1 1)
                        (Array.get index (Array.fromList emptyCells))
            in
            ({ model | foods = newFood :: model.foods }, Cmd.none)

        StartGame ->
            let gameBoardSize = model.options.gameBoardSize
                emptyCellsCount = (gameBoardSize * gameBoardSize) - 3
            in
            ( { model
                | gameState = Playing
                , snake = [ Point 0 0, Point 1 0, Point 2 0 ]
                , currentDirection = Down
                , foods = []
                , nextDirections = []
                , score = 0
              }
            , Random.generate NewFood (Random.int -1 emptyCellsCount)
            )

        ChangeDirection direction ->
            ( { model
                | nextDirections = updateNextDirections model direction
              }
            , Cmd.none
            )

        TogglePause ->
            case model.gameState of
                Playing -> ( { model | gameState = Paused }, Cmd.none )
                Paused -> ( { model | gameState = Playing }, Cmd.none )
                _ -> ( model, Cmd.none )


handleKeyPress : String -> Model -> Maybe Msg
handleKeyPress keyCode model =
    case model.gameState of
        Model.GameOver ->
            Just StartGame
        _ ->
            case keyCode of
                "ArrowLeft" ->
                    Just (ChangeDirection Left)
                "a" ->
                    Just (ChangeDirection Left)
                "ArrowUp" ->
                    Just (ChangeDirection Up)
                "w" ->
                    Just (ChangeDirection Up)
                "ArrowRight" ->
                    Just (ChangeDirection Right)
                "d" ->
                    Just (ChangeDirection Right)
                "ArrowDown" ->
                    Just (ChangeDirection Down)
                "s" ->
                    Just (ChangeDirection Down)
                " " ->
                    Just TogglePause
                _ ->
                    Nothing

updateNextDirections : Model -> Direction -> List Direction
updateNextDirections model direction =
    if List.any (\d -> d == direction) model.nextDirections then
        model.nextDirections
    else direction :: model.nextDirections

gameTick : Model -> (Model, Cmd Msg)
gameTick model =
    let nextDirection =
            case model.nextDirections |> List.reverse |> List.head of
                Just d ->
                    if oppositeDirections d model.currentDirection then
                        model.currentDirection
                    else d
                Nothing -> model.currentDirection
        advancedSnake =
            List.foldl
                (\segment newSnake -> advanceSnakeSegment nextDirection model.snake newSnake segment)
                []
                (List.reverse model.snake)
        snakeHead = advancedSnake |> List.reverse |> List.head
        eatenFood =
            List.head
                (List.filter
                    (\food -> Just food == snakeHead)
                    model.foods
                )
        emptyCellCount =
            (model.options.gameBoardSize * model.options.gameBoardSize) - List.length advancedSnake - List.length model.foods
        nextDirections =
            List.take
                (List.length model.nextDirections - 1)
                model.nextDirections
        hasDied = detectDeath snakeHead advancedSnake model.options.gameBoardSize
    in
    if hasDied then ( { model | gameState = GameOver }, Cmd.none )
    else case eatenFood of
        Just f ->
            ( { model
                | snake = growSnake advancedSnake
                , currentDirection = nextDirection
                , nextDirections = nextDirections
                , foods = List.filter (\f2 -> f /= f2) model.foods
                , score = model.score + 1
                }
            , Random.generate NewFood (Random.int -1 emptyCellCount)
            )
        Nothing ->
            ( { model
                | snake = advancedSnake
                , currentDirection = nextDirection
                , nextDirections = nextDirections
                }
            , Cmd.none
            )

detectDeath : Maybe Point -> List Point -> Int -> Bool
detectDeath snakeHead snake gameBoardSize =
    case snakeHead of
        Just sh ->
            let outsideGameBoard = sh.x < 0 || sh.y < 0 || sh.x >= gameBoardSize || sh.y >= gameBoardSize
                snakeTail = List.take (List.length snake - 1) snake
                eatSelf = List.any (\seg -> seg == sh) snakeTail
            in outsideGameBoard || eatSelf
        Nothing -> False


growSnake : List Point -> List Point
growSnake shortSnake =
    let lastSegment = List.head shortSnake
        growSpeed = 3
    in
    case lastSegment of
        Just s ->List.append (List.repeat growSpeed s) shortSnake
        Nothing -> shortSnake

findEmptyCells : Model -> List Point
findEmptyCells model =
    let gameBoardSize = model.options.gameBoardSize
        totalCells = gameBoardSize * gameBoardSize
        nonEmptyCells = List.append model.snake model.foods
        range = List.range 0 (totalCells - 1)
        allCells = List.map (\c -> Point (c // gameBoardSize) (modBy gameBoardSize c)) range
    in
    List.filter (\c -> not (List.any (\c2 -> c == c2) nonEmptyCells)) allCells

advanceSnakeSegment : Direction -> List Point -> List Point -> Point -> List Point
advanceSnakeSegment direction oldSnake newSnake segment =
    if List.length newSnake == 0 then
        case direction of
            Up -> [ segment, Point segment.x (segment.y - 1) ]
            Down -> [ segment, Point segment.x (segment.y + 1) ]
            Left -> [ segment, Point (segment.x - 1) segment.y ]
            Right -> [ segment, Point (segment.x + 1) segment.y ]
    else if List.length newSnake == List.length oldSnake then
        newSnake
    else segment :: newSnake

oppositeDirections : Direction -> Direction -> Bool
oppositeDirections d1 d2 =
    if (d1 == Left || d2 == Left) && (d1 == Right || d2 == Right) then
        True
    else if (d1 == Up || d2 == Up) && (d1 == Down || d2 == Down) then
        True
    else False
