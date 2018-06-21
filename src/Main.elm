module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import Dict
import Dict exposing (Dict)


---- MODEL ----

type Color
    = Black
    | White


type alias Position
    = (Int, Int)

type alias Board
    = Dict Position (Maybe Color)

type alias Model
    = { board : Board
      , turn : Color
      }


init : ( Model, Cmd Msg )
init =
    let
        emptyBoard
            = product (List.range 1 8) (List.range 1 8)
            |> List.map (\p -> (p, Nothing))
            |> Dict.fromList
        initialStones
            = [((4, 4), Just Black), ((4, 5), Just White), ((5, 4), Just White), ((5, 5), Just Black)]
            |> Dict.fromList
        board = Dict.union initialStones emptyBoard
    in
        ( { board = board, turn = Black }, Cmd.none )


---- UPDATE ----


type Msg
    = NoOp
    | Click Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { board } =
    div [class "board"]
        <| Dict.foldr (\_ cell seq -> dispCell cell :: seq) [] board



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


product : List a -> List b -> List (a, b)
product xs ys =
    List.map (\x -> List.map (\y -> (x,y)) ys) xs
        |> List.concat


dispCell : Maybe Color -> Html Msg
dispCell stone =
    case stone of
        Nothing  -> div [class "cell"] []
        (Just s) -> div [class <| "cell"]
                    [ div [class <| String.toLower <| toString s] []
                    ]

mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe func seq =
    let
        undefinedFunc x = undefinedFunc x
        undefined = undefinedFunc ()
    in
        List.map (Maybe.withDefault undefined)
            <| List.filter (\e -> e /= Nothing)
            <| List.map func seq

reversiMerge : Dict Position (List Position) -> Dict Position (List Position) -> Dict Position (List Position)
reversiMerge dict1 dict2 =
    Dict.merge
        (\pos cand buf -> Dict.insert pos cand buf)
        (\pos cand1 cand2 buf -> Dict.insert pos (cand1 ++ cand2) buf)
        (\pos cand buf -> Dict.insert pos cand buf)
        dict1
        dict2
        Dict.empty

search : Color -> Board -> Dict Position (List Position)
search color board =
    let
        bases = board
              |> Dict.filter (\_ s -> s == Just color)
              |> Dict.keys

        directions = product [-1, 0, 1] [-1, 0, 1]

        candidates = flip List.map directions
                     <| \pos -> mapMaybe (searchSegment board [] color pos) bases

        result = List.foldl reversiMerge Dict.empty <| List.map Dict.fromList candidates
    in
        result

searchSegment : Board -> List Position -> Color -> Position -> Position -> Maybe (Position, List Position)
searchSegment board flippables edgeColor (dx, dy) (baseX, baseY) =
    let
        nextPos = (baseX + dx, baseY + dy)
        nextCell = Dict.get nextPos board

        (finish, flippables_) =
            case nextCell of
                Nothing -> (True, [])  -- board上にないセル、つまり枠外 ==> 置けない
                Just Nothing -> (True, flippables)  -- 空きセル ==> ひっくり返せる石が1個でもあれば置ける
                Just (Just stone) ->
                    if stone == edgeColor then
                        (True, [])  -- 同じ色の石で塞いである ==> 置けない
                    else
                        (False, nextPos :: flippables)  -- 反対の色の石がある ==> ひっくり返せる石に加えて次の石を見る
    in
        if finish then
            if List.isEmpty flippables_ then
                Nothing
            else
                Just (nextPos, flippables_)
        else
            searchSegment board flippables_ edgeColor (dx, dy) nextPos
