module Pack exposing (Box, PackedData, PlacedBox, pack, textureAtlas)

import Array exposing (Array)
import Image exposing (Image)


{-| -}
type alias PackedData a =
    { width : Int
    , height : Int
    , boxes : List (PlacedBox a)
    }


{-| -}
type alias PlacedBox a =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , data : a
    }


{-| -}
type alias Box a =
    { width : Int
    , height : Int
    , data : a
    }


{-| Generate a texture atlas from the packed data.
-}
textureAtlas : (a -> Image) -> PackedData a -> Image
textureAtlas getImage packedData =
    let
        imageBoxes =
            List.map
                (\box -> { x = box.x, y = box.y, imageData = Image.toArray2d (getImage box.data) })
                packedData.boxes
    in
    List.foldl
        (\imageBox atlas ->
            Array.foldl
                (\row ( currentRow, atlas_ ) ->
                    let
                        atlasRow : Array Int
                        atlasRow =
                            Array.get currentRow atlas_ |> Maybe.withDefault Array.empty

                        newRow =
                            setImageRow imageBox.x row atlasRow
                    in
                    ( currentRow + 1
                    , Array.set currentRow newRow atlas_
                    )
                )
                ( imageBox.y, atlas )
                imageBox.imageData
                |> Tuple.second
        )
        (Array.repeat packedData.height (Array.repeat packedData.width 0))
        imageBoxes
        |> Image.fromArray2d


setImageRow : Int -> Array Int -> Array Int -> Array Int
setImageRow offset imageRow atlasRow =
    Array.foldl
        (\value ( offset_, atlasRow_ ) -> ( offset_ + 1, Array.set offset_ value atlasRow_ ))
        ( offset, atlasRow )
        imageRow
        |> Tuple.second


{-| -}
pack : List (Box a) -> PackedData a
pack list =
    let
        sortedBoxes =
            List.sortBy (\box -> box.width * box.height) list

        maxWidth =
            sortedBoxes |> List.map .width |> List.minimum |> Maybe.withDefault 128 |> max 128

        { row, rest } =
            placeRow [] 0 0 maxWidth list
    in
    packHelper maxWidth 0 [] row rest


packHelper : Int -> Int -> List (List (PlacedBox a)) -> List (PlacedBox a) -> List (Box a) -> PackedData a
packHelper maxWidth y rows lastRow boxes =
    let
        { row, rest } =
            placeRow [] 0 y maxWidth boxes

        newY =
            row |> List.map (\box -> box.y + box.height) |> List.maximum |> Maybe.withDefault y

        newRows =
            lastRow :: rows
    in
    if List.isEmpty rest then
        { width = maxWidth
        , height = newY
        , boxes = row :: newRows |> List.concat
        }

    else
        packHelper maxWidth newY newRows row rest


placeRow : List (PlacedBox a) -> Int -> Int -> Int -> List (Box a) -> { row : List (PlacedBox a), rest : List (Box a) }
placeRow currentList x y maxWidth list =
    case list of
        head :: rest ->
            if head.width + x <= maxWidth then
                placeRow
                    ({ x = x, y = y, width = head.width, height = head.height, data = head.data } :: currentList)
                    (x + head.width)
                    y
                    maxWidth
                    rest

            else
                { row = currentList, rest = list }

        [] ->
            { row = currentList, rest = list }
