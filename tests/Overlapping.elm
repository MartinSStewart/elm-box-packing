module Overlapping exposing (..)

import Quantity exposing (Quantity)
import Set exposing (Set)


type alias VectorMath number a =
    { add : ( number, a ) -> ( number, a ) -> ( number, a )
    , multiply : ( number, a ) -> ( number, a ) -> ( number, a )
    , dotProduct : ( number, a ) -> ( number, a ) -> number
    }


vectorMath : VectorMath number a -> VectorMath number ( number, a )
vectorMath previous =
    { add = \( a, aRest ) ( b, bRest ) -> ( a + b, previous.add aRest bRest )
    , multiply = \( a, aRest ) ( b, bRest ) -> ( a * b, previous.multiply aRest bRest )
    , dotProduct = \( a, aRest ) ( b, bRest ) -> a * b + previous.dotProduct aRest bRest
    }


vectorMath1 : VectorMath number ()
vectorMath1 =
    { add = \( a, () ) ( b, () ) -> ( a + b, () )
    , multiply = \( a, () ) ( b, () ) -> ( a * b, () )
    , dotProduct = \( a, () ) ( b, () ) -> a * b
    }


lineIntersections : List { start : Quantity number units, end : Quantity number units } -> List ( Int, Int )
lineIntersections lineSegments =
    lineSegments
        |> List.indexedMap
            (\index { start, end } ->
                [ { index = index, value = start }
                , { index = index, value = end }
                ]
            )
        |> List.concat
        |> Quantity.sortBy .value
        |> List.foldl
            (\{ index } ( sweeping, intersections ) ->
                if Set.member index sweeping then
                    ( Set.remove index sweeping, intersections )

                else
                    ( Set.insert index sweeping
                    , Set.toList sweeping
                        |> List.map (\otherIndex -> ( min index otherIndex, max index otherIndex ))
                        |> (\a -> a ++ intersections)
                    )
            )
            ( Set.empty, [] )
        |> Tuple.second


boxIntersections :
    List
        { a
            | x : Quantity number units
            , y : Quantity number units
            , width : Quantity number units
            , height : Quantity number units
        }
    -> Set ( Int, Int )
boxIntersections boxes =
    let
        xOverlap =
            boxes
                |> List.map (\box -> { start = box.x, end = Quantity.plus box.x box.width })
                |> lineIntersections
                |> Set.fromList

        yOverlap =
            boxes
                |> List.map (\box -> { start = box.y, end = Quantity.plus box.y box.height })
                |> lineIntersections
                |> Set.fromList
    in
    Set.intersect xOverlap yOverlap
