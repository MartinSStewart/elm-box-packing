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

  - `minimumWidth` sets how wide the container is provided there aren't any boxes that are too wide to fit.
  - `powerOfTwoSize` decides if the width and height of the container should snap to a power of two size. Can be useful for creating texture atlases.
  - `spacing` controls how much separation there are between boxes. This does not affect spacing between boxes and the edge of the container.

-}
type alias Config number units =
    { minimumWidth : Quantity number units
    , powerOfTwoSize : Bool
    , spacing : Quantity number units
    }


defaultConfig : Config number units
defaultConfig =
    { minimumWidth = Quantity.Quantity 128, powerOfTwoSize = True, spacing = Quantity.Quantity 1 }


boxWidth : { a | width : Quantity number units } -> Quantity number units
boxWidth =
    .width >> Quantity.abs


boxHeight : { a | height : Quantity number units } -> Quantity number units
boxHeight =
    .height >> Quantity.abs


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
            { minimumWidth = Quantity.max Quantity.zero config.minimumWidth
            , nearestPowerOfTwoSize = config.powerOfTwoSize
            , spacing = Quantity.max Quantity.zero config.spacing
            }

        sortedBoxes =
            Quantity.sortBy (\box -> Quantity.times (boxWidth box) (boxHeight box)) list

        maxWidth =
            sortedBoxes
                |> List.map boxWidth
                |> Quantity.minimum
                |> Maybe.withDefault config.minimumWidth
                |> Quantity.max config.minimumWidth
                |> (if config.powerOfTwoSize then
                        nextPowerOf2

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


rowMaxY : Quantity number units -> List (PlacedBox number units a) -> Quantity number units
rowMaxY offset row =
    row |> List.map (\box -> Quantity.plus box.y (boxHeight box)) |> Quantity.maximum |> Maybe.withDefault offset


packHelper :
    Quantity number units
    -> Quantity number units
    -> Quantity number units
    -> List (List (PlacedBox number units a))
    -> List (PlacedBox number units a)
    -> List (Box number units a)
    -> PackingData number units a
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
    Quantity number units
    -> List (PlacedBox number units a)
    -> Quantity number units
    -> Quantity number units
    -> Quantity number units
    -> List (Box number units a)
    -> { row : List (PlacedBox number units a), rest : List (Box number units a) }
placeRow spacing currentList x y maxWidth list =
    case list of
        head :: rest ->
            let
                nextX =
                    Quantity.sum [ spacing, boxWidth head, x ]
            in
            if nextX |> Quantity.lessThanOrEqualTo maxWidth then
                placeRow
                    spacing
                    ({ x = x, y = y, width = boxWidth head, height = boxHeight head, data = head.data } :: currentList)
                    nextX
                    y
                    maxWidth
                    rest

            else
                { row = currentList, rest = list }

        [] ->
            { row = currentList, rest = list }
