module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Html.Events exposing (onClick)


-- component import example

import Components.Hello exposing (hello)


-- APP
-- MODEL


type alias Cell =
    { value : Int
    , x : Int
    , y : Int
    }


type alias Model =
    { dimension : Int
    , cells : List Cell
    }


model : Model
model =
    { dimension = 4
    , cells =
        [ { value = 10, x = 0, y = 0 }
        , { value = 20, x = 2, y = 0 }
        , { value = 30, x = 3, y = 0 }
        ]
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- UPDATE


type Msg
    = NoOp
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        _ ->
            model


moveRightAll grid dimension =
    grid


getYes : Int -> Array (Array (Maybe Cell)) -> Array (Maybe Cell)
getYes y arr =
    let
        res =
            Array.get y arr
    in
        case res of
            Just res ->
                res

            Nothing ->
                Array.fromList []


filterNothings : Array (Maybe Cell) -> Array Cell
filterNothings arr =
    (Array.fromList
        (List.filterMap
            identity
            (Array.toList arr)
        )
    )


cellsToGrid cells dimension =
    let
        maybeGrid =
            (List.foldl
                (\cell ->
                    (\arr ->
                        (Array.set cell.x (Array.set cell.y (Just cell) (getYes cell.y arr)) arr)
                    )
                )
                (Array.repeat dimension (Array.repeat dimension Nothing))
                cells
            )
    in
        (Array.map
            filterNothings
            maybeGrid
        )


moveRight model =
    model


getDims dimension =
    List.map (\num -> num) (List.range 0 (dimension - 1))


getCell grid x y =
    let
        cellArray =
            Array.get x grid
    in
        case cellArray of
            Just yArray ->
                Array.get y yArray

            Nothing ->
                Nothing


getCellView grid x y =
    let
        cell =
            getCell grid x y
    in
        case cell of
            Just cell ->
                strong [] [ text (toString cell.value) ]

            Nothing ->
                text ""


view : Model -> Html Msg
view model =
    let
        grid =
            cellsToGrid model.cells model.dimension

        dims =
            getDims model.dimension
    in
        table [ onClick Increment ]
            [ thead [] (List.concat [ [ (th [] []) ], (List.map (\x -> (th [] [ text (toString (x + 1)) ])) dims) ])
            , tbody []
                (List.map
                    (\y ->
                        tr []
                            (List.concat
                                [ [ td [ class "first" ] [ (text (toString (y + 1))) ] ]
                                , (List.map (\x -> td [] [ getCellView grid x y ]) dims)
                                ]
                            )
                    )
                    dims
                )
            ]
