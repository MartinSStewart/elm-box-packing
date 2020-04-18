module Tests exposing (overlapTests, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Overlapping
import Pack
import Quantity
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Packing tests"
        [ fuzz (fuzzPack Pack.defaultConfig) "Pack random boxes and make sure they don't overlap" <|
            Debug.log "aaa"
                >> validPackingData
                >> Debug.log ""
                >> Expect.equal Nothing
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
                Overlapping.boxIntersections
                    [ { x = Quantity.zero, y = Quantity.zero, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                    , { x = Quantity.Quantity 5, y = Quantity.Quantity 2, width = Quantity.Quantity 5, height = Quantity.Quantity 5 }
                    ]
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


fuzzBox : Fuzzer (Pack.Box Int units ())
fuzzBox =
    Fuzz.map2
        (\w h ->
            { width = Quantity.Quantity (w * 3)
            , height = Quantity.Quantity (h * 3)
            , data = ()
            }
        )
        (Fuzz.intRange -20 20)
        (Fuzz.intRange -20 20)


fuzzPack : Pack.Config Int units -> Fuzzer (Pack.PackingData Int units ())
fuzzPack config =
    Fuzz.map (Pack.pack config) (Fuzz.list fuzzBox)
