module Pack exposing
    ( pack, defaultConfig, Box, Config, PackingData, PlacedBox
    , textureAtlas, ImageBox, TextureAtlas
    , textureAtlasCodec, packingDataCodec
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
type alias PackingData units a =
    { width : Quantity Int units
    , height : Quantity Int units
    , boxes : List (PlacedBox units a)
    }


{-| -}
type alias PlacedBox units a =
    { x : Quantity Int units
    , y : Quantity Int units
    , width : Quantity Int units
    , height : Quantity Int units
    , data : a
    }


{-| -}
type alias Box units a =
    { width : Quantity Int units
    , height : Quantity Int units
    , data : a
    }


{-| -}
type alias ImageBox a =
    { image : Image
    , data : a
    }


{-| -}
type alias TextureAtlas a =
    { packingData : PackingData Pixels a, atlas : Image }


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


{-| -}
packingDataCodec : Codec a -> Codec (PackingData units a)
packingDataCodec dataCodec =
    Codec.object PackingData
        |> Codec.field .width quantityCodec
        |> Codec.field .height quantityCodec
        |> Codec.field .boxes (Codec.list (placeBoxCodec dataCodec))
        |> Codec.buildObject


placeBoxCodec : Codec a -> Codec (PlacedBox units a)
placeBoxCodec dataCodec =
    Codec.object PlacedBox
        |> Codec.field .x quantityCodec
        |> Codec.field .y quantityCodec
        |> Codec.field .width quantityCodec
        |> Codec.field .height quantityCodec
        |> Codec.field .data dataCodec
        |> Codec.buildObject


mapPackingData : (a -> b) -> PackingData units a -> PackingData units b
mapPackingData mapFunc packedData =
    { width = packedData.width
    , height = packedData.height
    , boxes =
        List.map
            (\box -> { x = box.x, y = box.y, width = box.width, height = box.height, data = mapFunc box.data })
            packedData.boxes
    }


type alias Config units =
    { minimumWidth : Quantity Int units
    , nearestPowerOfTwoSize : Bool
    , spacing : Quantity Int units
    }


defaultConfig : Config units
defaultConfig =
    { minimumWidth = Quantity.Quantity 128, nearestPowerOfTwoSize = True, spacing = Quantity.Quantity 1 }


{-| Pack images together to create a single texture atlas image.
-}
textureAtlas : Config Pixels -> List (ImageBox a) -> TextureAtlas a
textureAtlas config images =
    let
        packedData : PackingData Pixels ( Image, a )
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
pack : Config units -> List (Box units a) -> PackingData units a
pack config list =
    let
        validatedConfig =
            { minimumWidth = Quantity.max Quantity.zero config.minimumWidth
            , nearestPowerOfTwoSize = config.nearestPowerOfTwoSize
            , spacing = Quantity.max Quantity.zero config.spacing
            }

        sortedBoxes =
            Quantity.sortBy (\box -> Quantity.times box.width box.height) list

        maxWidth =
            sortedBoxes
                |> List.map .width
                |> Quantity.minimum
                |> Maybe.withDefault config.minimumWidth
                |> Quantity.max config.minimumWidth
                |> (if config.nearestPowerOfTwoSize then
                        rawQuantity >> toFloat >> logBase 2 >> ceiling >> (\a -> a ^ 2) >> Quantity.Quantity

                    else
                        identity
                   )

        { row, rest } =
            placeRow validatedConfig.spacing [] Quantity.zero Quantity.zero maxWidth list
    in
    packHelper
        validatedConfig.spacing
        maxWidth
        (rowMaxY Quantity.zero row |> Quantity.plus config.spacing)
        []
        row
        rest


rowMaxY : Quantity Int units -> List (PlacedBox units a) -> Quantity Int units
rowMaxY offset row =
    row |> List.map (\box -> Quantity.plus box.y box.height) |> Quantity.maximum |> Maybe.withDefault offset


packHelper :
    Quantity Int units
    -> Quantity Int units
    -> Quantity Int units
    -> List (List (PlacedBox units a))
    -> List (PlacedBox units a)
    -> List (Box units a)
    -> PackingData units a
packHelper spacing maxWidth y rows lastRow boxes =
    let
        { row, rest } =
            placeRow spacing [] Quantity.zero y maxWidth boxes

        newY =
            rowMaxY y row

        newRows =
            lastRow :: rows
    in
    if List.isEmpty rest then
        { width = maxWidth
        , height = newY
        , boxes = row :: newRows |> List.concat
        }

    else
        packHelper spacing maxWidth newY newRows row rest


placeRow :
    Quantity Int units
    -> List (PlacedBox units a)
    -> Quantity Int units
    -> Quantity Int units
    -> Quantity Int units
    -> List (Box units a)
    -> { row : List (PlacedBox units a), rest : List (Box units a) }
placeRow spacing currentList x y maxWidth list =
    case list of
        head :: rest ->
            let
                nextX =
                    Quantity.sum [ spacing, head.width, x ]
            in
            if nextX |> Quantity.lessThanOrEqualTo maxWidth then
                placeRow
                    spacing
                    ({ x = x, y = y, width = head.width, height = head.height, data = head.data } :: currentList)
                    nextX
                    y
                    maxWidth
                    rest

            else
                { row = currentList, rest = list }

        [] ->
            { row = currentList, rest = list }
