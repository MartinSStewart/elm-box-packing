module Pack exposing
    ( pack, defaultConfig, Box, Config, PackedBoxes, PackedBox
    , textureAtlas, ImageBox, TextureAtlas
    , textureAtlasCodec, TextureAtlasCodecError(..), packedBoxesCodec, packedBoxesCodecFloat
    )

{-|


### Efficiently pack 2D boxes together.

Note that this package uses [`Quantity`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity) instead of raw `Int`s and `Float`s.
You'll need to install [`ianmackenzie/elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) to get started.


# Generic packing

Functions for packing boxes together. Scroll down to the Image Packing section if you want to work specifically with images.

@docs pack, defaultConfig, Box, Config, PackedBoxes, PackedBox


# Image packing

Suppose you have a lot of images and you want to load them in as a single image to reduce HTTP requests or simplify texture management in WebGL.
With these functions you can generate a texture atlas (also called a sprite sheet) to pack all the images together.

@docs textureAtlas, ImageBox, TextureAtlas


# Serialization

If you want to save your packed boxes/images and then later load them into your app, these functions (when used with [`elm-serialize`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/)) should help.

@docs textureAtlasCodec, TextureAtlasCodecError, packedBoxesCodec, packedBoxesCodecFloat

-}

import Array exposing (Array)
import Image exposing (Image)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..))
import Serialize as Codec exposing (Codec)


{-| All of the boxes we've packed together and how large the containing region is.
-}
type alias PackedBoxes number units a =
    { width : Quantity number units
    , height : Quantity number units
    , boxes : List (PackedBox number units a)
    }


{-| A box, now packed into a larger rectangle.
The width and height of these boxes will be positive even if the corresponding `Box` had a negative width or height.
-}
type alias PackedBox number units a =
    { x : Quantity number units
    , y : Quantity number units
    , width : Quantity number units
    , height : Quantity number units
    , data : a
    }


{-| A box we want to pack and its accompanying data. Use this with [`pack`](#pack).
-}
type alias Box number units a =
    { width : Quantity number units
    , height : Quantity number units
    , data : a
    }


{-| An image we want to pack and its accompanying data. Use this with [`textureAtlas`](#textureAtlas).
-}
type alias ImageBox a =
    { image : Image
    , data : a
    }


{-| A texture atlas image and accompanying data.
Note that [`Image`](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#Image) may contain functions.
[Don't store this data structure in you model](https://discourse.elm-lang.org/t/implications-of-storing-functions-in-model-or-msg-type/5472).
Instead convert `Image` into a [string](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#toPngUrl) or [bytes](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#toPng) and store that.
-}
type alias TextureAtlas a =
    { packedBoxes : PackedBoxes Int Pixels a, atlas : Image }


{-| A codec for [`TextureAtlas`](#TextureAtlas).
-}
textureAtlasCodec : Codec e a -> Codec (TextureAtlasCodecError e) (TextureAtlas a)
textureAtlasCodec dataCodec =
    Codec.record TextureAtlas
        |> Codec.field .packedBoxes (packedBoxesCodec dataCodec |> Codec.mapError PackedBoxDataError)
        |> Codec.field .atlas imageCodec
        |> Codec.finishRecord


imageCodec : Codec (TextureAtlasCodecError e) Image
imageCodec =
    Codec.bytes |> Codec.mapValid (Image.decode >> Result.fromMaybe FailedToParseImage) Image.toPng


{-| These are possible errors we can get when decoding with `textureAtlasCodec`.

  - `FailedToParseImage`: The image we encoded is corrupted.
  - `PackedBoxDataError`: The extra data in `PackedBoxes` is invalid in some way.

-}
type TextureAtlasCodecError e
    = FailedToParseImage
    | PackedBoxDataError e


quantityCodec : Codec e (Quantity Int units)
quantityCodec =
    Codec.int |> Codec.map Quantity.Quantity rawQuantity


quantityCodecFloat : Codec e (Quantity Float units)
quantityCodecFloat =
    Codec.float |> Codec.map Quantity.Quantity rawQuantity


{-| A codec for [`PackedBoxes`](#PackedBoxes) when dealing with integer values.
-}
packedBoxesCodec : Codec e a -> Codec e (PackedBoxes Int units a)
packedBoxesCodec dataCodec =
    Codec.record PackedBoxes
        |> Codec.field .width quantityCodec
        |> Codec.field .height quantityCodec
        |> Codec.field .boxes (Codec.list (placeBoxCodec dataCodec))
        |> Codec.finishRecord


{-| A codec for [`PackedBoxes`](#PackedBoxes) when dealing with floating point values.
-}
packedBoxesCodecFloat : Codec e a -> Codec e (PackedBoxes Float units a)
packedBoxesCodecFloat dataCodec =
    Codec.record PackedBoxes
        |> Codec.field .width quantityCodecFloat
        |> Codec.field .height quantityCodecFloat
        |> Codec.field .boxes (Codec.list (placeBoxCodecFloat dataCodec))
        |> Codec.finishRecord


placeBoxCodec : Codec e a -> Codec e (PackedBox Int units a)
placeBoxCodec dataCodec =
    Codec.record PackedBox
        |> Codec.field .x quantityCodec
        |> Codec.field .y quantityCodec
        |> Codec.field .width quantityCodec
        |> Codec.field .height quantityCodec
        |> Codec.field .data dataCodec
        |> Codec.finishRecord


placeBoxCodecFloat : Codec e a -> Codec e (PackedBox Float units a)
placeBoxCodecFloat dataCodec =
    Codec.record PackedBox
        |> Codec.field .x quantityCodecFloat
        |> Codec.field .y quantityCodecFloat
        |> Codec.field .width quantityCodecFloat
        |> Codec.field .height quantityCodecFloat
        |> Codec.field .data dataCodec
        |> Codec.finishRecord


mapPackedBoxes : (a -> b) -> PackedBoxes number units a -> PackedBoxes number units b
mapPackedBoxes mapFunc packedData =
    { width = packedData.width
    , height = packedData.height
    , boxes =
        List.map
            (\box -> { x = box.x, y = box.y, width = box.width, height = box.height, data = mapFunc box.data })
            packedData.boxes
    }


{-|

  - `powerOfTwoSize` sets if the width and height of the container should grow to a power of two size.
    Sometimes this is needed when creating texture atlases.
  - `spacing` sets the minimum separation between boxes.

-}
type alias Config number units =
    { spacing : Quantity number units
    , powerOfTwoSize : Bool
    }


{-| Default configuration for packing boxes. Zero spacing and no growing the container width and height to a power of two.
-}
defaultConfig : Config number units
defaultConfig =
    { spacing = Quantity.zero, powerOfTwoSize = False }


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

Note that this is a slow process.
Even for a small texture atlas (256x256 pixels) it can take a couple seconds and for large images it's probably not a good idea to try.
If you need something fast then it's probably a better idea to just use [`pack`](#pack) and then draw the texture atlas using an HTML5 canvas or something.

-}
textureAtlas : Config Int Pixels -> List (ImageBox a) -> TextureAtlas a
textureAtlas config images =
    let
        packedData : PackedBoxes Int Pixels ( Image, a )
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
    { packedBoxes = mapPackedBoxes Tuple.second packedData
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
pack : Config number units -> List (Box number units a) -> PackedBoxes number units a
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

        minBoxWidth =
            sortedBoxes |> List.map .width |> Quantity.minimum |> Maybe.withDefault Quantity.zero |> Quantity.plus validatedConfig.spacing

        minBoxHeight =
            sortedBoxes |> List.map .height |> Quantity.minimum |> Maybe.withDefault Quantity.zero |> Quantity.plus validatedConfig.spacing
    in
    List.foldl
        (\box ( { width, height, boxes }, regions ) ->
            let
                updatePackedBoxes placedBox =
                    { width = Quantity.max width (Quantity.sum [ placedBox.x, placedBox.width, validatedConfig.spacing ])
                    , height = Quantity.max height (Quantity.sum [ placedBox.y, placedBox.height, validatedConfig.spacing ])
                    , boxes = placedBox :: boxes
                    }
            in
            case bestRegion validatedConfig.spacing box regions of
                Just ( bestRegion_, regionsLeft ) ->
                    let
                        ( placedBox, newRegions ) =
                            placeBoxInRegion minBoxWidth minBoxHeight validatedConfig.spacing (List.length boxes |> modBy 2 |> (==) 0) box bestRegion_
                    in
                    ( updatePackedBoxes placedBox
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
                            placeBoxInRegion minBoxWidth minBoxHeight validatedConfig.spacing True box newSpot
                    in
                    ( updatePackedBoxes placedBox
                    , newRegions ++ regions
                    )
        )
        ( { width = Quantity.zero, height = Quantity.zero, boxes = [] }
        , []
        )
        sortedBoxes
        |> Tuple.first
        |> (\a ->
                let
                    getFinalDimension dimension =
                        dimension
                            |> Quantity.minus validatedConfig.spacing
                            |> (if config.powerOfTwoSize then
                                    nextPowerOf2

                                else
                                    identity
                               )
                in
                { a
                    | width = getFinalDimension a.width
                    , height = getFinalDimension a.height
                }
           )


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
    -> Quantity number units
    -> Quantity number units
    -> Bool
    -> Box number units data
    -> Region number units
    -> ( PackedBox number units data, List (Region number units) )
placeBoxInRegion minBoxWidth minBoxHeight spacing splitVertically box region =
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
                Quantity.greaterThanOrEqualTo minBoxWidth region_.width
                    && Quantity.greaterThanOrEqualTo minBoxHeight region_.height
            )
    )


nextPowerOf2 : Quantity number units -> Quantity number units
nextPowerOf2 (Quantity.Quantity value) =
    if value == 0 then
        Quantity.zero

    else
        let
            helperPlus n =
                if 2 ^ n < value then
                    helperPlus (n + 1)

                else
                    2 ^ n
        in
        helperPlus 0 |> Quantity.Quantity
