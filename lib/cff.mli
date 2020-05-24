type header = {
    major : int ;
    minor : int ;
    hdrSize : int ;
    offSize : int ;
  }

type sid

type number = Integer of int | Real of float

type top_dict_entry = [
  | `Version of sid
  | `Notice of sid
  | `Copyright of sid
  | `FullName of sid
  | `FamilyName of sid
  | `Weight of sid
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
  | `charset of number
  | `Encoding of number
  | `CharStrings of number
  | `Private of number * number
  | `SyntheticBase of number
  | `PostScript of sid
  | `BaseFontName of sid
  | `BaseFontBlend of sid
  ]

val string_of_sid : sid -> string

val parse : bytes -> header * string list * top_dict_entry array list
