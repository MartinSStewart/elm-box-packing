module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pack
import Quantity
import Test exposing (..)


suite : Test
suite =
    describe "Packing tests"
        [ Test.fuzz (fuzzPack Pack.defaultConfig) "Pack random boxes and make sure they don't overlap" <|
            validPackingData
                >> Expect.equal Nothing
        ]


type Error number units a
    = BoxOutside (Pack.PlacedBox number units a) (List (Pack.PlacedBox number units a))
    | BoxOverlap ( Pack.PlacedBox number units a, Pack.PlacedBox number units a )


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
            0


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
            { width = Quantity.Quantity w
            , height = Quantity.Quantity h
            , data = ()
            }
        )
        Fuzz.int
        Fuzz.int


fuzzPack : Pack.Config Int units -> Fuzzer (Pack.PackingData Int units ())
fuzzPack config =
    Fuzz.map (Pack.pack config) (Fuzz.list fuzzBox)
