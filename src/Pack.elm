module Pack exposing
    ( pack, defaultConfig, Box, Config, PackingData, PlacedBox
    , textureAtlas, ImageBox, TextureAtlas
    , textureAtlasCodec, packingDataCodec
    , packingDataCodecFloat
    )

{-|


# Generic packing

@docs pack, defaultConfig, Box, Config, PackingData, PlacedBox


# Image packing

@docs textureAtlas, ImageBox, TextureAtlas


# Serialization

@docs textureAtlasCodec, packingDataCodec

-}

import Array exposing (Array)
import Bytes
import Codec.Bytes as Codec exposing (Codec)
import Image exposing (Image)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..))


{-| -}
type alias PackingData number units a =
    { width : Quantity number units
    , height : Quantity number units
    , boxes : List (PlacedBox number units a)
    }


{-| Our box, now placed within a larger rectangle.
The width and height of these placed boxes will be positive even if the corresponding `Box` had a negative width or height.
-}
type alias PlacedBox number units a =
    { x : Quantity number units
    , y : Quantity number units
    , width : Quantity number units
    , height : Quantity number units
    , data : a
    }


{-| -}
type alias Box number units a =
    { width : Quantity number units
    , height : Quantity number units
    , data : a
    }


{-| -}
type alias ImageBox a =
    { image : Image
    , data : a
    }


{-| -}
type alias TextureAtlas a =
    { packingData : PackingData Int Pixels a, atlas : Image }


{-| -}
textureAtlasCodec : Codec a -> Codec (TextureAtlas a)
textureAtlasCodec dataCodec =
    Codec.object TextureAtlas
        |> Codec.field .packingData (packingDataCodec dataCodec)
        |> Codec.field .atlas imageCodec
        |> Codec.buildObject


imageCodec : Codec Image
imageCodec =
    Codec.bytes |> Codec.map (Image.decode >> Maybe.withDefault (Image.fromList 1 [ 0 ])) Image.toPng


quantityCodec : Codec (Quantity Int units)
quantityCodec =
    Codec.unsignedInt32 Bytes.BE |> Codec.map Quantity.Quantity rawQuantity


quantityCodecFloat : Codec (Quantity Float units)
quantityCodecFloat =
    Codec.float64 |> Codec.map Quantity.Quantity rawQuantity


{-| -}
packingDataCodec : Codec a -> Codec (PackingData Int units a)
packingDataCodec dataCodec =
    Codec.object PackingData
        |> Codec.field .width quantityCodec
        |> Codec.field .height quantityCodec
        |> Codec.field .boxes (Codec.list (placeBoxCodec dataCodec))
        |> Codec.buildObject


{-| -}
packingDataCodecFloat : Codec a -> Codec (PackingData Float units a)
packingDataCodecFloat dataCodec =
    Codec.object PackingData
        |> Codec.field .width quantityCodecFloat
        |> Codec.field .height quantityCodecFloat
        |> Codec.field .boxes (Codec.list (placeBoxCodecFloat dataCodec))
        |> Codec.buildObject


placeBoxCodec : Codec a -> Codec (PlacedBox Int units a)
placeBoxCodec dataCodec =
    Codec.object PlacedBox
        |> Codec.field .x quantityCodec
        |> Codec.field .y quantityCodec
        |> Codec.field .width quantityCodec
        |> Codec.field .height quantityCodec
        |> Codec.field .data dataCodec
        |> Codec.buildObject


placeBoxCodecFloat : Codec a -> Codec (PlacedBox Float units a)
placeBoxCodecFloat dataCodec =
    Codec.object PlacedBox
        |> Codec.field .x quantityCodecFloat
        |> Codec.field .y quantityCodecFloat
        |> Codec.field .width quantityCodecFloat
        |> Codec.field .height quantityCodecFloat
        |> Codec.field .data dataCodec
        |> Codec.buildObject


mapPackingData : (a -> b) -> PackingData number units a -> PackingData number units b
mapPackingData mapFunc packedData =
    { width = packedData.width
    , height = packedData.height
    , boxes =
        List.map
            (\box -> { x = box.x, y = box.y, width = box.width, height = box.height, data = mapFunc box.data })
            packedData.boxes
    }


{-|

  - `minimumWidth` sets how wide the container is. If any boxes are too wide to fit then the widest box will be used instead.
  - `powerOfTwoSize` decides if the width and height of the container should snap to a power of two. Sometimes this is needed when creating texture atlases.
  - `spacing` controls how much separation there are between boxes. This does not affect spacing between boxes and the edge of the container.

-}
type alias Config number units =
    { spacing : Quantity number units
    }


defaultConfig : Config number units
defaultConfig =
    { spacing = Quantity.zero }


boxWidth : { a | width : Quantity number units } -> Quantity number units
boxWidth =
    .width >> Quantity.abs


boxHeight : { a | height : Quantity number units } -> Quantity number units
boxHeight =
    .height >> Quantity.abs


boxArea : { a | width : Quantity number units, height : Quantity number units } -> Quantity number (Quantity.Product units units)
boxArea box =
    Quantity.times (boxWidth box) (boxHeight box)


{-| Pack images together to create a single texture atlas image.
-}
textureAtlas : Config Int Pixels -> List (ImageBox a) -> TextureAtlas a
textureAtlas config images =
    let
        packedData : PackingData Int Pixels ( Image, a )
        packedData =
            images
                |> List.map
                    (\{ image, data } ->
                        { width = Image.dimensions image |> .width |> Pixels.pixels
                        , height = Image.dimensions image |> .height |> Pixels.pixels
                        , data = ( image, data )
                        }
                    )
                |> pack config

        imageBoxes : List { x : Quantity Int Pixels, y : Quantity Int Pixels, image : Array (Array Image.Pixel) }
        imageBoxes =
            List.map
                (\box -> { x = box.x, y = box.y, image = Image.toArray2d (Tuple.first box.data) })
                packedData.boxes
    in
    { packingData = mapPackingData Tuple.second packedData
    , atlas =
        List.foldl
            (\imageBox atlas ->
                Array.foldl
                    (\row ( currentRow, atlas_ ) ->
                        let
                            atlasRow : Array Image.Pixel
                            atlasRow =
                                Array.get currentRow atlas_ |> Maybe.withDefault Array.empty

                            newRow =
                                setImageRow imageBox.x row atlasRow
                        in
                        ( currentRow + 1
                        , Array.set currentRow newRow atlas_
                        )
                    )
                    ( Pixels.inPixels imageBox.y, atlas )
                    imageBox.image
                    |> Tuple.second
            )
            (Array.repeat (Pixels.inPixels packedData.height) (Array.repeat (Pixels.inPixels packedData.width) 0))
            imageBoxes
            |> Image.fromArray2d
    }


setImageRow : Quantity Int Pixels -> Array Image.Pixel -> Array Image.Pixel -> Array Image.Pixel
setImageRow offset imageRow atlasRow =
    Array.foldl
        (\value ( offset_, atlasRow_ ) -> ( offset_ + 1, Array.set offset_ value atlasRow_ ))
        ( Pixels.inPixels offset, atlasRow )
        imageRow
        |> Tuple.second


rawQuantity : Quantity number units -> number
rawQuantity (Quantity.Quantity value) =
    value


{-| Pack generic boxes together.
-}
pack : Config number units -> List (Box number units a) -> PackingData number units a
pack config list =
    let
        validatedConfig =
            { spacing = Quantity.max Quantity.zero config.spacing
            }

        sortedBoxes =
            List.sortWith
                (\boxA boxB ->
                    case
                        ( Quantity.compare (boxWidth boxA) (boxWidth boxB)
                        , Quantity.compare (boxHeight boxA) (boxHeight boxB)
                        )
                    of
                        ( LT, LT ) ->
                            LT

                        ( GT, GT ) ->
                            GT

                        ( _, _ ) ->
                            Quantity.compare (boxArea boxA) (boxArea boxB)
                )
                list
                |> List.reverse
    in
    List.foldl
        (\box ( { width, height, boxes }, regions ) ->
            let
                updatePackingData placedBox =
                    { width = Quantity.max width (Quantity.sum [ placedBox.x, placedBox.width, validatedConfig.spacing ])
                    , height = Quantity.max height (Quantity.sum [ placedBox.y, placedBox.height, validatedConfig.spacing ])
                    , boxes = placedBox :: boxes
                    }
            in
            case bestRegion validatedConfig.spacing box regions of
                Just ( bestRegion_, regionsLeft ) ->
                    let
                        ( placedBox, newRegions ) =
                            placeBoxInRegion validatedConfig.spacing (List.length boxes |> modBy 2 |> (==) 0) box bestRegion_
                    in
                    ( updatePackingData placedBox
                    , newRegions ++ regionsLeft
                    )

                Nothing ->
                    let
                        fitsBottom =
                            width |> Quantity.greaterThanOrEqualTo (Quantity.plus validatedConfig.spacing (boxWidth box))

                        fitsRight =
                            height |> Quantity.greaterThanOrEqualTo (Quantity.plus validatedConfig.spacing (boxHeight box))

                        packBottom =
                            if fitsBottom && fitsRight then
                                height |> Quantity.lessThan width

                            else
                                fitsBottom

                        newSpot =
                            if packBottom then
                                { x = Quantity.zero
                                , y = height
                                , width = width
                                , height = Quantity.plus validatedConfig.spacing (boxHeight box)
                                }

                            else
                                { x = width
                                , y = Quantity.zero
                                , width = Quantity.plus validatedConfig.spacing (boxWidth box)
                                , height = height
                                }

                        ( placedBox, newRegions ) =
                            placeBoxInRegion validatedConfig.spacing True box newSpot
                    in
                    ( updatePackingData placedBox
                    , newRegions ++ regions
                    )
        )
        ( { width = Quantity.zero, height = Quantity.zero, boxes = [] }
        , []
        )
        sortedBoxes
        |> Tuple.first
        |> (\a -> { a | width = a.width |> Quantity.minus validatedConfig.spacing, height = a.height |> Quantity.minus validatedConfig.spacing })


bestRegion :
    Quantity number units
    -> Box number units data
    -> List (Region number units)
    -> Maybe ( Region number units, List (Region number units) )
bestRegion spacing box regions =
    let
        maybeBestRegion =
            regions
                |> List.filter
                    (\region ->
                        (region.width |> Quantity.greaterThanOrEqualTo (Quantity.plus spacing (boxWidth box)))
                            && (region.height |> Quantity.greaterThanOrEqualTo (Quantity.plus spacing (boxHeight box)))
                    )
                |> Quantity.minimumBy
                    (\region ->
                        Quantity.min
                            (region.width |> Quantity.minus box.width)
                            (region.height |> Quantity.minus box.height)
                    )
    in
    case maybeBestRegion of
        Just bestRegion_ ->
            Just ( bestRegion_, List.filter ((/=) bestRegion_) regions )

        Nothing ->
            Nothing


type alias Region number units =
    { x : Quantity number units
    , y : Quantity number units
    , width : Quantity number units
    , height : Quantity number units
    }


placeBoxInRegion :
    Quantity number units
    -> Bool
    -> Box number units data
    -> Region number units
    -> ( PlacedBox number units data, List (Region number units) )
placeBoxInRegion spacing splitVertically box region =
    let
        boxWidth_ =
            Quantity.plus spacing (boxWidth box)

        boxHeight_ =
            Quantity.plus spacing (boxHeight box)
    in
    ( { x = region.x
      , y = region.y
      , width = boxWidth box
      , height = boxHeight box
      , data = box.data
      }
    , [ { x = Quantity.plus region.x boxWidth_
        , y = region.y
        , width = region.width |> Quantity.minus boxWidth_
        , height =
            if splitVertically then
                region.height

            else
                boxHeight_
        }
      , { x = region.x
        , y = Quantity.plus region.y boxHeight_
        , width =
            if splitVertically then
                boxWidth_

            else
                region.width
        , height = region.height |> Quantity.minus boxHeight_
        }
      ]
        |> List.filter
            (\region_ ->
                Quantity.greaterThan Quantity.zero region_.width
                    && Quantity.greaterThan Quantity.zero region_.height
            )
    )



--type alias Spot number units =
--    { x : Quantity number units
--    , y : Quantity number units
--    , width : Quantity number units
--    , height : Quantity number units
--    }
--
--
--bestSpot : Box number units data -> List (Spot number units) -> Maybe (Spot number units)
--bestSpot rect spots =
--    spots
--        |> List.filter (\spot -> (spot.width |> Quantity.greaterThanOrEqualTo rect.width) && (spot.height |> Quantity.greaterThanOrEqualTo rect.height))
--        |> Quantity.sortBy (\spot -> Quantity.max (Quantity.plus spot.x rect.width) (Quantity.plus spot.y rect.height))
--        |> List.head
--
--
--spotCut : PlacedBox number units data -> Spot number units -> Spot number units
--spotCut placedBox spot =
--    let
--        intervalIntersect start1 end1 start2 end2 =
--            Quantity.min (end1 |> Quantity.minus start2) (end2 |> Quantity.minus start1)
--                |> Quantity.greaterThan Quantity.zero
--
--        horizontalIntersect =
--            intervalIntersect
--                spot.x
--                (Quantity.plus spot.x spot.width)
--                placedBox.x
--                (Quantity.plus placedBox.x placedBox.width)
--
--        verticalIntersect =
--            intervalIntersect
--                spot.y
--                (Quantity.plus spot.y spot.height)
--                placedBox.y
--                (Quantity.plus placedBox.y placedBox.height)
--    in
--    if horizontalIntersect && (placedBox.y |> Quantity.greaterThanOrEqualTo spot.y) then
--        { x = spot.x
--        , y = spot.y
--        , width = spot.width
--        , height = Quantity.min (placedBox.y |> Quantity.minus spot.y) spot.height
--        }
--
--    else if verticalIntersect && (placedBox.x |> Quantity.greaterThanOrEqualTo spot.x) then
--        { x = spot.x
--        , y = spot.y
--        , width = Quantity.min (placedBox.x |> Quantity.minus spot.x) spot.width
--        , height = spot.height
--        }
--
--    else
--        spot
--
--
--initialSpots : List (Spot number units)
--initialSpots =
--    [ { x = Quantity 0, y = Quantity 0, width = Quantity 9999999, height = Quantity 9999999 } ]
--
--
--putRectangles : List (Box number units data) -> List (Spot number units) -> List (PlacedBox number units data)
--putRectangles boxes spots =
--    case boxes of
--        [] ->
--            []
--
--        head :: rest ->
--            putRectangles rest (putRectangle head spots)
--
--
--putRectangle : Box number units data -> List (Spot number units) -> List (Spot number units)
--putRectangle rect spots =
--    case bestSpot rect spots of
--        Just best ->
--            let
--                placedRect =
--                    { x = best.x, y = best.y, width = rect.width, height = rect.height, data = rect.data }
--
--                right =
--                    { x = Quantity.plus best.x rect.width
--                    , y = best.y
--                    , width = best.width |> Quantity.minus rect.width
--                    , height = best.height
--                    }
--
--                top =
--                    { x = best.x
--                    , y = Quantity.plus best.y rect.height
--                    , width = best.width
--                    , height = best.height |> Quantity.minus rect.height
--                    }
--            in
--            spots
--                |> List.map
--                    (\spot ->
--                        if spot == best then
--                            spotCut placedRect spot
--
--                        else
--                            spot
--                    )
--                |> (++) [ right, top ]
--
--        Nothing ->
--            spots


nextPowerOf2 : Quantity number units -> Quantity number units
nextPowerOf2 (Quantity.Quantity value) =
    let
        helperPlus n =
            if 2 ^ n < value then
                helperPlus (n + 1)

            else
                2 ^ n
    in
    helperPlus 0 |> Quantity.Quantity
