type header = {
    major : int ;
    minor : int ;
    hdrSize : int ;
    offSize : int ;
  }

type sid

type number = Integer of int | Real of float

type top_dict_operator = [
  | `BaseFontBlend
  | `BaseFontName
  | `CIDCount
  | `CIDFontRevision
  | `CIDFontType
  | `CIDFontVersion
  | `CharStrings
  | `CharstringType
  | `Copyright
  | `Encoding
  | `FDArray
  | `FDSelect
  | `FamilyName
  | `FontBBox
  | `FontMatrix
  | `FontName
  | `FullName
  | `ItalicAngle
  | `Notice
  | `PaintType
  | `Postscript
  | `Private
  | `ROS
  | `StrokeWidth
  | `SyntheticBase
  | `UIDBase
  | `UnderlinePosition
  | `UnderlineThickness
  | `UniqueID
  | `Version
  | `Weight
  | `XUID
  | `charset
  | `isFixedPitch
  ]

type top_dict_token = [
  | `Integer of int
  | `Real of string
  | `Operator of top_dict_operator
  ]

val string_of_sid : sid -> string

val parse : bytes -> header * string list * top_dict_token list list * string list

val test_string : string
