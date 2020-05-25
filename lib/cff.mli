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
  | `PostScript
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

type top_dict_entry = [
  | `Version of string
  | `Notice of string
  | `Copyright of string
  | `FullName of string
  | `FamilyName of string
  | `Weight of string
  | `isFixedPitch of bool
  | `ItalicAngle of number
  | `UnderlinePosition of number
  | `UnderlineThickness of number
  | `PaintType of number
  | `CharstringType of number
  | `FontMatrix of number array
  | `UniqueID of number
  | `FontBBox of number array
  | `StrokeWidth of number
  | `XUID of number array
  | `charset of int
  | `Encoding of int
  | `CharStrings of int
  | `Private of int * int
  | `SyntheticBase of number
  | `PostScript of string
  | `BaseFontName of string
  | `BaseFontBlend of number array
  ]

val parse : bytes -> header * string list * top_dict_token list list * (top_dict_entry list list, string) result * string list * string list * string list option list

val test_string : string
