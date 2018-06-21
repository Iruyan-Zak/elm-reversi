module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html
import Html.Events as Events
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
            = [((4, 4), Just White), ((4, 5), Just Black), ((5, 4), Just Black), ((5, 5), Just White)]
            |> Dict.fromList
        board = Dict.union initialStones emptyBoard
    in
        ( { board = board, turn = Black }, Cmd.none )


---- UPDATE ----

type alias Flips = List Position

type Msg
    = NoOp
    | Hand Position Flips


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ board, turn } as model) =
    case msg of
        NoOp -> ( model, Cmd.none )
        Hand pos flips ->
            let
                board_ = List.foldl
                        (\p b -> Dict.update p (always <| Just <| Just turn) b)
                        board
                        (pos :: flips)

                puttables = search (flipColor turn) board_

                _ = Debug.log (toString puttables) ()
                turn_ =
                    if Dict.isEmpty puttables then
                        turn
                    else
                        flipColor turn

                model =
                    { board = board_
                    , turn = turn_
                    }
            in
                model ! []

flipColor : Color -> Color
flipColor color =
    case color of
        White -> Black
        Black -> White


---- VIEW ----


view : Model -> Html Msg
view ({ board, turn } as model) =
    let
        puttables = search turn board
        boardState = calcState board puttables
        cells = Dict.foldr (\pos cell seq -> dispCell pos cell :: seq) [] boardState

        message =
            if Dict.isEmpty puttables then
                resultString <| countStones board
            else
                turnString turn
    in
        div [class "board"]
            <| cells ++ [Html.p [] [text message]]


dispCell : Position -> (Maybe Color, Flips) -> Html Msg
dispCell pos (stone, flips) =
    let
        classes = class "cell" ::
                  (case flips of
                       [] -> []
                       _  -> [class "puttable", Events.onClick (Hand pos flips)]
                  )
        children =
            case stone of
                Nothing -> []
                Just s  -> [ div [class <| String.toLower <| toString s] [] ]
    in
        div classes children

calcState : Board -> Dict Position (List Position) -> Dict Position (Maybe Color, Flips)
calcState board puttables =
    Dict.merge
        (\k color d -> Dict.insert k (color, []) d)
        (\k color puts d -> Dict.insert k (color, puts) d)
        (\k puts d -> Dict.insert k (Nothing, puts) d)  -- このコードパスはないはず
        board
        puttables
        Dict.empty


resultString : (Int, Int) -> String
resultString (blacks, whites) =
    let
        first =
            "黒" ++ toString blacks ++ " 対 白" ++ toString whites ++ " で、"
        second =
            if blacks == whites then
                "引き分けです。"
            else
                (if blacks > whites then "黒" else "白") ++ "の勝ちです。"
    in
        first ++ second

turnString : Color -> String
turnString color =
    (if color == Black then "黒" else "白") ++ "の番です。"

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


----- utils -----

product : List a -> List b -> List (a, b)
product xs ys =
    List.map (\x -> List.map (\y -> (x,y)) ys) xs
        |> List.concat

mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe func seq =
    let
        fromJust x =
            case x of
                Just x -> x
                Nothing -> Debug.crash "error: fromJust Nothing"
    in
        List.map fromJust
            <| List.filter (\e -> e /= Nothing)
            <| List.map func seq

----- logic -----

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

reversiMerge : Dict Position (List Position) -> Dict Position (List Position) -> Dict Position (List Position)
reversiMerge dict1 dict2 =
    Dict.merge
        (\pos cand buf -> Dict.insert pos cand buf)
        (\pos cand1 cand2 buf -> Dict.insert pos (cand1 ++ cand2) buf)
        (\pos cand buf -> Dict.insert pos cand buf)
        dict1
        dict2
        Dict.empty


countStones : Board -> (Int, Int)
countStones board =
    Dict.values board
        |> \vs ->
            (List.length <| List.filter (\v -> v == Just Black) vs
            , List.length <| List.filter (\v -> v == Just White) vs)

