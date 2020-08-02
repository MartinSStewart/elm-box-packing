# Changelog

## 1.0.0

* Initial release

## 1.0.1

* Fix typo in `README.md`

## 2.0.0

* Replaced `elm-codec-bytes` dependency with `elm-serialize`. This is a breaking change for any data you've encoded with `textureAtlasCodec`, `packedBoxesCodec`, or `packedBoxesCodecFloat`.

## 3.0.0

* Add missing error type for `textAtlasCodec`. This is not a breaking change for data encoded with version 2.0.0 of this package.
* Fix mistakes in documentation