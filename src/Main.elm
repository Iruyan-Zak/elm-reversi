module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events as Events
import Html.Attributes exposing (class)
import Dict
import Dict exposing (Dict)

---- MODEL ----

type Color
    = Black
    | White

flipColor : Color -> Color
flipColor color =
    case color of
        White -> Black
        Black -> White

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

                model =
                    { board = board_
                    , turn = flipColor turn
                    }
            in
                model ! []


---- VIEW ----


view : Model -> Html Msg
view { board, turn } =
    let
        puttables = search turn board
        boardState =
            Dict.merge
                (\k color d -> Dict.insert k (color, Nothing) d)
                (\k color puts d -> Dict.insert k (color, Just puts) d)
                (\k puts d -> Dict.insert k (Just turn, Just puts) d)  -- このコードパスはないはず
                board
                puttables
                Dict.empty
    in
        div [class "board"]
            <| Dict.foldr (\pos cell seq -> dispCell pos cell :: seq) [] boardState


dispCell : Position -> (Maybe Color, Maybe (List Position)) -> Html Msg
dispCell pos (stone, flips) =
    let
        classes = class "cell" ::
                  (case flips of
                       Nothing -> []
                       Just flips_ -> [class "puttable", Events.onClick (Hand pos flips_)]
                  )
        children =
            case stone of
                Nothing -> []
                Just s  -> [ div [class <| String.toLower <| toString s] [] ]
    in
        div classes children

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

{-
undefined : undefined
undefined =
    let
        _ = Debug.log "Undefined code path is executed." ()
        undefinedFunc x = undefinedFunc x
    in
        undefinedFunc ()
-}

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
