module Tests exposing (overlapTests, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Overlapping
import Pack exposing (Box)
import Quantity exposing (Quantity(..))
import Set
import Test exposing (..)


mapBoxes :
    (Pack.PlacedBox number units a -> Pack.PlacedBox number units a)
    -> Pack.PackingData number units a
    -> Pack.PackingData number units a
mapBoxes mapBoxFunc packingData =
    { packingData | boxes = packingData.boxes |> List.map mapBoxFunc }


suite : Test
suite =
    describe "Packing tests"
        [ test "Pack random boxes and make sure they don't overlap" <|
            \_ ->
                randomBoxes
                    |> Pack.pack { minimumWidth = Quantity 50, powerOfTwoSize = False, spacing = Quantity.zero }
                    |> validPackingData
                    |> Expect.equal Nothing
        , test "Pack random boxes with spacing and make sure they don't overlap and keep that minimum spacing" <|
            \_ ->
                let
                    packingData =
                        randomBoxes
                            |> Pack.pack { minimumWidth = Quantity 50, powerOfTwoSize = False, spacing = Quantity 2 }

                    expanded0 =
                        mapBoxes
                            (\{ x, y, width, height, data } ->
                                { x = Quantity.plus x (Quantity -1)
                                , y = Quantity.plus y (Quantity -1)
                                , width = Quantity.plus width (Quantity 2)
                                , height = Quantity.plus height (Quantity 2)
                                , data = data
                                }
                            )
                            packingData

                    expanded1 =
                        mapBoxes
                            (\{ x, y, width, height, data } ->
                                { x = Quantity.plus x (Quantity -2)
                                , y = Quantity.plus y (Quantity -2)
                                , width = Quantity.plus width (Quantity 4)
                                , height = Quantity.plus height (Quantity 4)
                                , data = data
                                }
                            )
                            packingData
                in
                ( Overlapping.boxIntersections expanded0.boxes |> Set.isEmpty
                , Overlapping.boxIntersections expanded1.boxes |> Set.isEmpty
                )
                    |> Expect.equal ( True, False )
        , test "Box with width exactly matching container width doesn't cause stack overflow" <|
            \_ ->
                Pack.pack
                    Pack.defaultConfig
                    [ { width = Quantity 128, height = Quantity.zero, data = () } ]
                    |> Expect.equal
                        { width = Quantity 128
                        , height = Quantity.zero
                        , boxes =
                            [ { x = Quantity.zero
                              , y = Quantity.zero
                              , width = Quantity 128
                              , height = Quantity.zero
                              , data = ()
                              }
                            ]
                        }
        , test "Box with width exceeding container width doesn't cause stack overflow" <|
            \_ ->
                Pack.pack
                    Pack.defaultConfig
                    [ { width = Quantity 129, height = Quantity.zero, data = () } ]
                    |> Expect.equal
                        { width = Quantity 256
                        , height = Quantity.zero
                        , boxes =
                            [ { x = Quantity.zero
                              , y = Quantity.zero
                              , width = Quantity 129
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
                    [ { x = Quantity.zero, y = Quantity.zero, width = Quantity 5, height = Quantity 5 }
                    , { x = Quantity 2, y = Quantity 2, width = Quantity 5, height = Quantity 5 }
                    ]
                    |> Expect.equal (Set.fromList [ ( 0, 1 ) ])
        , test "Check 2 non-overlapping boxes" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity 5, height = Quantity 5 }
                , { x = Quantity 5, y = Quantity 2, width = Quantity 5, height = Quantity 5 }
                ]
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "Check 2 non-overlapping boxes reverse order" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity 5, height = Quantity 5 }
                , { x = Quantity 5, y = Quantity 2, width = Quantity 5, height = Quantity 5 }
                ]
                    |> List.reverse
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "Check 2 non-overlapping vertical boxes" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity 5, height = Quantity 5 }
                , { x = Quantity.zero, y = Quantity 5, width = Quantity 5, height = Quantity 5 }
                ]
                    |> Overlapping.boxIntersections
                    |> Expect.equal Set.empty
        , test "Check 2 non-overlapping vertical boxes reverse order" <|
            \_ ->
                [ { x = Quantity.zero, y = Quantity.zero, width = Quantity 5, height = Quantity 5 }
                , { x = Quantity.zero, y = Quantity 5, width = Quantity 5, height = Quantity 5 }
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
