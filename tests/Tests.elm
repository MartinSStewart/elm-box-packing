module Tests exposing (overlapTests, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Overlapping
import Pack
import Quantity exposing (Quantity(..))
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Packing tests"
        [ test "Pack random boxes and make sure they don't overlap" <|
            \_ ->
                randomBoxes
                    |> Pack.pack Pack.defaultConfig
                    |> validPackingData
                    |> Expect.equal Nothing
        , test "Box with width exactly matching container width doesn't cause stack overflow" <|
            \_ ->
                Pack.pack
                    Pack.defaultConfig
                    [ { width = Quantity.Quantity 128, height = Quantity.zero, data = () } ]
                    |> Expect.equal
                        { width = Quantity.Quantity 128
                        , height = Quantity.zero
                        , boxes =
                            [ { x = Quantity.zero
                              , y = Quantity.zero
                              , width = Quantity.Quantity 128
                              , height = Quantity.zero
                              , data = ()
                              }
                            ]
                        }
        , test "Box with width exceeding container width doesn't cause stack overflow" <|
            \_ ->
                Pack.pack
                    Pack.defaultConfig
                    [ { width = Quantity.Quantity 129, height = Quantity.zero, data = () } ]
                    |> Expect.equal
                        { width = Quantity.Quantity 256
                        , height = Quantity.zero
                        , boxes =
                            [ { x = Quantity.zero
                              , y = Quantity.zero
                              , width = Quantity.Quantity 129
                              , height = Quantity.zero
                              , data = ()
                              }
                            ]
                        }
        ]


overlapTests : Test
overlapTests =
    describe "Overlap tests"
        [ test "Check 2 overlapping boxes" <|
            \_ ->
                Overlapping.boxIntersections
                    [ { x = Quantity.zero, y = Quantity.zero, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                    , { x = Quantity.Quantity 2, y = Quantity.Quantity 2, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                    ]
                    |> Expect.equal (Set.fromList [ ( 0, 1 ) ])
        , test "Check 2 non-overlapping boxes" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                , { x = Quantity.Quantity 5, y = Quantity.Quantity 2, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                ]
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "Check 2 non-overlapping boxes reverse order" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                , { x = Quantity.Quantity 5, y = Quantity.Quantity 2, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                ]
                    |> List.reverse
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "Check 2 non-overlapping vertical boxes" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                , { x = Quantity.zero, y = Quantity.Quantity 5, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                ]
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "Check 2 non-overlapping vertical boxes reverse order" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                , { x = Quantity.zero, y = Quantity.Quantity 5, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                ]
                    |> List.reverse
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "a" <|
            \_ ->
                [ { data = (), height = Quantity 27, width = Quantity 9, x = Quantity 78, y = Quantity 150 }
                , { data = (), height = Quantity 15, width = Quantity 12, x = Quantity 66, y = Quantity 150 }
                , { data = (), height = Quantity 6, width = Quantity 15, x = Quantity 87, y = Quantity 120 }
                , { data = (), height = Quantity 0, width = Quantity 15, x = Quantity 72, y = Quantity 120 }
                , { data = (), height = Quantity 9, width = Quantity 15, x = Quantity 57, y = Quantity 120 }
                ]
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        ]


type Error number units a
    = BoxOutside (Pack.PlacedBox number units a) (List (Pack.PlacedBox number units a))
    | BoxOverlap ( Int, Int ) (List ( Int, Int ))


validPackingData : Pack.PackingData number units a -> Maybe (Error number units a)
validPackingData packingData =
    let
        boxesOutside =
            List.filter
                (\box ->
                    (box.x |> Quantity.lessThan Quantity.zero)
                        || (box.y |> Quantity.lessThan Quantity.zero)
                        || (Quantity.plus box.x box.width |> Quantity.greaterThan packingData.width)
                        || (Quantity.plus box.y box.height |> Quantity.greaterThan packingData.height)
                )
                packingData.boxes
    in
    case boxesOutside of
        head :: rest ->
            BoxOutside head rest |> Just

        [] ->
            case Overlapping.boxIntersections packingData.boxes |> Set.toList of
                head :: rest ->
                    BoxOverlap head rest |> Just

                [] ->
                    Nothing


twoBoxesOverlap : Pack.PlacedBox number units a -> Pack.PlacedBox number units a -> Bool
twoBoxesOverlap first second =
    twoLinesOverlap first.x (Quantity.plus first.x first.width) second.x (Quantity.plus second.x second.width)
        && twoLinesOverlap first.y (Quantity.plus first.y first.height) second.y (Quantity.plus second.y second.height)


twoLinesOverlap :
    Quantity.Quantity number units
    -> Quantity.Quantity number units
    -> Quantity.Quantity number units
    -> Quantity.Quantity number units
    -> Bool
twoLinesOverlap a0 a1 b0 b1 =
    let
        aMin =
            Quantity.min a0 a1

        aMax =
            Quantity.max a0 a1

        bMin =
            Quantity.min b0 b1

        bMax =
            Quantity.max b0 b1
    in
    (aMax |> Quantity.greaterThan bMin) && (bMax |> Quantity.greaterThan aMin)


{-| A list of random box sizes. We dont use a fuzzer because they keep causing stack overflows.
-}
randomBoxes =
    [ { data = (), height = Quantity 27, width = Quantity -9 }
    , { data = (), height = Quantity 12, width = Quantity 30 }
    , { data = (), height = Quantity 21, width = Quantity 0 }
    , { data = (), height = Quantity -30, width = Quantity 12 }
    , { data = (), height = Quantity 30, width = Quantity 21 }
    , { data = (), height = Quantity 9, width = Quantity -15 }
    , { data = (), height = Quantity 6, width = Quantity -30 }
    , { data = (), height = Quantity 3, width = Quantity -3 }
    , { data = (), height = Quantity -27, width = Quantity -6 }
    , { data = (), height = Quantity 30, width = Quantity -24 }
    , { data = (), height = Quantity -9, width = Quantity -3 }
    , { data = (), height = Quantity -30, width = Quantity 9 }
    , { data = (), height = Quantity 30, width = Quantity 24 }
    , { data = (), height = Quantity 27, width = Quantity -24 }
    , { data = (), height = Quantity 9, width = Quantity 15 }
    , { data = (), height = Quantity 21, width = Quantity 0 }
    , { data = (), height = Quantity 24, width = Quantity 21 }
    , { data = (), height = Quantity -15, width = Quantity 6 }
    , { data = (), height = Quantity 0, width = Quantity -30 }
    , { data = (), height = Quantity -18, width = Quantity -30 }
    , { data = (), height = Quantity -9, width = Quantity -9 }
    , { data = (), height = Quantity -30, width = Quantity 30 }
    , { data = (), height = Quantity 6, width = Quantity -15 }
    , { data = (), height = Quantity 21, width = Quantity -21 }
    , { data = (), height = Quantity -3, width = Quantity -6 }
    , { data = (), height = Quantity -24, width = Quantity -27 }
    , { data = (), height = Quantity 12, width = Quantity -27 }
    , { data = (), height = Quantity -30, width = Quantity 30 }
    , { data = (), height = Quantity 24, width = Quantity -12 }
    , { data = (), height = Quantity 21, width = Quantity 12 }
    , { data = (), height = Quantity 30, width = Quantity 21 }
    , { data = (), height = Quantity 27, width = Quantity -24 }
    , { data = (), height = Quantity 9, width = Quantity 15 }
    , { data = (), height = Quantity 0, width = Quantity -15 }
    , { data = (), height = Quantity 6, width = Quantity 15 }
    , { data = (), height = Quantity -27, width = Quantity -15 }
    , { data = (), height = Quantity -30, width = Quantity -30 }
    , { data = (), height = Quantity -12, width = Quantity -6 }
    , { data = (), height = Quantity -30, width = Quantity -30 }
    , { data = (), height = Quantity 15, width = Quantity -12 }
    , { data = (), height = Quantity -27, width = Quantity -9 }
    ]
