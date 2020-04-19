module Overlapping exposing (..)

import Quantity exposing (Quantity)
import Set exposing (Set)


lineIntersections : List { id : Int, start : Quantity number units, end : Quantity number units } -> List ( Int, Int )
lineIntersections lineSegments =
    lineSegments
        |> List.concatMap
            (\{ id, start, end } ->
                if start == end then
                    []

                else
                    [ { id = id, isStart = True, value = Quantity.min start end }
                    , { id = id, isStart = False, value = Quantity.max start end }
                    ]
            )
        |> List.sortWith
            (\a b ->
                case Quantity.compare a.value b.value of
                    EQ ->
                        if a.isStart && not b.isStart then
                            GT

                        else if not a.isStart && b.isStart then
                            LT

                        else
                            EQ

                    LT ->
                        LT

                    GT ->
                        GT
            )
        |> List.foldl
            (\{ id, isStart } ( sweeping, intersections ) ->
                if isStart then
                    ( Set.insert id sweeping
                    , Set.toList sweeping
                        |> List.map (\otherId -> ( min id otherId, max id otherId ))
                        |> (\a -> a ++ intersections)
                    )

                else
                    ( Set.remove id sweeping, intersections )
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
        boxesWithIds =
            boxes |> List.indexedMap Tuple.pair

        xOverlap =
            boxesWithIds
                |> List.map (\( id, box ) -> { id = id, start = box.x, end = Quantity.plus box.x box.width })
                |> lineIntersections
                |> Set.fromList

        yOverlap =
            boxesWithIds
                |> List.map (\( id, box ) -> { id = id, start = box.y, end = Quantity.plus box.y box.height })
                |> lineIntersections
                |> Set.fromList
    in
    Set.intersect xOverlap yOverlap
