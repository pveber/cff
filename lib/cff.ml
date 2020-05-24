type header = {
    major : int ;
    minor : int ;
    hdrSize : int ;
    offSize : int ;
  }

type sid = int

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

let standard_strings = [|
    ".notdef"; "space"; "exclam"; "quotedbl"; "numbersign";
    "dollar"; "percent"; "ampersand"; "quoteright"; "parenleft"; "parenright";
    "asterisk"; "plus"; "comma"; "hyphen"; "period"; "slash"; "zero"; "one";
    "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "colon";
    "semicolon"; "less"; "equal"; "greater"; "question"; "at"; "A"; "B"; "C";
    "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R";
    "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; "bracketleft"; "backslash";
    "bracketright"; "asciicircum"; "underscore"; "quoteleft"; "a"; "b"; "c";
    "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r";
    "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"; "braceleft"; "bar"; "braceright";
    "asciitilde"; "exclamdown"; "cent"; "sterling"; "fraction"; "yen"; "florin";
    "section"; "currency"; "quotesingle"; "quotedblleft"; "guillemotleft";
    "guilsinglleft"; "guilsinglright"; "fi"; "fl"; "endash"; "dagger";
    "daggerdbl"; "periodcentered"; "paragraph"; "bullet"; "quotesinglbase";
    "quotedblbase"; "quotedblright"; "guillemotright"; "ellipsis"; "perthousand";
    "questiondown"; "grave"; "acute"; "circumflex"; "tilde"; "macron"; "breve";
    "dotaccent"; "dieresis"; "ring"; "cedilla"; "hungarumlaut"; "ogonek"; "caron";
    "emdash"; "AE"; "ordfeminine"; "Lslash"; "Oslash"; "OE"; "ordmasculine"; "ae";
    "dotlessi"; "lslash"; "oslash"; "oe"; "germandbls"; "onesuperior";
    "logicalnot"; "mu"; "trademark"; "Eth"; "onehalf"; "plusminus"; "Thorn";
    "onequarter"; "divide"; "brokenbar"; "degree"; "thorn"; "threequarters";
    "twosuperior"; "registered"; "minus"; "eth"; "multiply"; "threesuperior";
    "copyright"; "Aacute"; "Acircumflex"; "Adieresis"; "Agrave"; "Aring";
    "Atilde"; "Ccedilla"; "Eacute"; "Ecircumflex"; "Edieresis"; "Egrave";
    "Iacute"; "Icircumflex"; "Idieresis"; "Igrave"; "Ntilde"; "Oacute";
    "Ocircumflex"; "Odieresis"; "Ograve"; "Otilde"; "Scaron"; "Uacute";
    "Ucircumflex"; "Udieresis"; "Ugrave"; "Yacute"; "Ydieresis"; "Zcaron";
    "aacute"; "acircumflex"; "adieresis"; "agrave"; "aring"; "atilde"; "ccedilla";
    "eacute"; "ecircumflex"; "edieresis"; "egrave"; "iacute"; "icircumflex";
    "idieresis"; "igrave"; "ntilde"; "oacute"; "ocircumflex"; "odieresis";
    "ograve"; "otilde"; "scaron"; "uacute"; "ucircumflex"; "udieresis"; "ugrave";
    "yacute"; "ydieresis"; "zcaron"; "exclamsmall"; "Hungarumlautsmall";
    "dollaroldstyle"; "dollarsuperior"; "ampersandsmall"; "Acutesmall";
    "parenleftsuperior"; "parenrightsuperior"; "twodotenleader"; "onedotenleader";
    "zerooldstyle"; "oneoldstyle"; "twooldstyle"; "threeoldstyle"; "fouroldstyle";
    "fiveoldstyle"; "sixoldstyle"; "sevenoldstyle"; "eightoldstyle";
    "nineoldstyle"; "commasuperior"; "threequartersemdash"; "periodsuperior";
    "questionsmall"; "asuperior"; "bsuperior"; "centsuperior"; "dsuperior";
    "esuperior"; "isuperior"; "lsuperior"; "msuperior"; "nsuperior"; "osuperior";
    "rsuperior"; "ssuperior"; "tsuperior"; "ff"; "ffi"; "ffl"; "parenleftinferior";
    "parenrightinferior"; "Circumflexsmall"; "hyphensuperior"; "Gravesmall";
    "Asmall"; "Bsmall"; "Csmall"; "Dsmall"; "Esmall"; "Fsmall"; "Gsmall"; "Hsmall";
    "Ismall"; "Jsmall"; "Ksmall"; "Lsmall"; "Msmall"; "Nsmall"; "Osmall"; "Psmall";
    "Qsmall"; "Rsmall"; "Ssmall"; "Tsmall"; "Usmall"; "Vsmall"; "Wsmall"; "Xsmall";
    "Ysmall"; "Zsmall"; "colonmonetary"; "onefitted"; "rupiah"; "Tildesmall";
    "exclamdownsmall"; "centoldstyle"; "Lslashsmall"; "Scaronsmall"; "Zcaronsmall";
    "Dieresissmall"; "Brevesmall"; "Caronsmall"; "Dotaccentsmall"; "Macronsmall";
    "figuredash"; "hypheninferior"; "Ogoneksmall"; "Ringsmall"; "Cedillasmall";
    "questiondownsmall"; "oneeighth"; "threeeighths"; "fiveeighths"; "seveneighths";
    "onethird"; "twothirds"; "zerosuperior"; "foursuperior"; "fivesuperior";
    "sixsuperior"; "sevensuperior"; "eightsuperior"; "ninesuperior"; "zeroinferior";
    "oneinferior"; "twoinferior"; "threeinferior"; "fourinferior"; "fiveinferior";
    "sixinferior"; "seveninferior"; "eightinferior"; "nineinferior"; "centinferior";
    "dollarinferior"; "periodinferior"; "commainferior"; "Agravesmall";
    "Aacutesmall"; "Acircumflexsmall"; "Atildesmall"; "Adieresissmall"; "Aringsmall";
    "AEsmall"; "Ccedillasmall"; "Egravesmall"; "Eacutesmall"; "Ecircumflexsmall";
    "Edieresissmall"; "Igravesmall"; "Iacutesmall"; "Icircumflexsmall";
    "Idieresissmall"; "Ethsmall"; "Ntildesmall"; "Ogravesmall"; "Oacutesmall";
    "Ocircumflexsmall"; "Otildesmall"; "Odieresissmall"; "OEsmall"; "Oslashsmall";
    "Ugravesmall"; "Uacutesmall"; "Ucircumflexsmall"; "Udieresissmall";
    "Yacutesmall"; "Thornsmall"; "Ydieresissmall"; "001.000"; "001.001"; "001.002";
    "001.003"; "Black"; "Bold"; "Book"; "Light"; "Medium"; "Regular"; "Roman";
    "Semibold"
  |]

let string_of_sid i = standard_strings.(i)

module Reader_monad = struct
  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Read_byte : char t
    | Peek_byte : char t
    | Read_int16 : int t
    | Read_uint16 : int t
    | Read_int32 : int t
    | Seek : int -> unit t
    | Error : string -> 'a t
    | Tell : int t

  let return x = Return x
  let ( >>= ) x f = Bind (x, f)
  let ( >>| ) x f = x >>= fun y -> return (f y)
  let read_byte = Read_byte
  let peek_byte = Peek_byte
  let read_int16 = Read_int16
  let read_uint16 = Read_uint16
  let read_int32 = Read_int32
  let read_u8 = read_byte >>| Char.code
  let seek i = Seek i
  let error msg = Error msg
  let errorf fmt = Printf.ksprintf (fun msg -> Error msg) fmt
  let tell = Tell

  let rec from_bytes_aux : type s. bytes -> int -> s t -> int * s = fun s i m ->
    match m with
    | Return x -> i, x
    | Bind (x, f) ->
       let i, y = from_bytes_aux s i x in
       from_bytes_aux s i (f y)
    | Read_byte -> i + 1, Bytes.get s i
    | Peek_byte -> i, Bytes.get s i
    | Read_int16 -> i + 2, Bytes.get_int16_be s i
    | Read_uint16 -> i + 2, Bytes.get_uint16_be s i
    | Read_int32 -> i + 4, Bytes.get_int32_be s i |> Int32.to_int
    | Seek i -> i, ()
    | Error msg -> failwith msg
    | Tell -> i, i

  let from_bytes s m =
    from_bytes_aux s 0 m
    |> snd
end

open Reader_monad

let card8 = read_u8
  
let card16 =
  card8 >>= fun b0 ->
  card8 >>| fun b1 ->
  (b0 lsl 8) lor b1

let card24 =
  card8 >>= fun b0 ->
  card8 >>= fun b1 ->
  card8 >>| fun b2 ->
  (b0 lsl 16) lor (b1 lsl 8) lor b2

let card32 =
  card8 >>= fun b0 ->
  card8 >>= fun b1 ->
  card8 >>= fun b2 ->
  card8 >>| fun b3 ->
  (b0 lsl 14) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
  
let integer =
  read_u8 >>= fun b0 ->
  if b0 >= 32 then
    if b0 < 247 then return (b0 - 139)
    else
      read_u8 >>= fun b1 ->
      if b0 < 251 then return ((b0 - 247) * 256 + b1 + 108)
      else return (- (b0 - 251) * 256 - b1 - 108)
  else
    if b0 = 28 then read_int16
    else if b0 = 29 then read_int32
    else error "invalid integer"

let integer_test input value =
  from_bytes (Bytes.of_string input) integer = value

let%test _ = integer_test "\x8b" 0
let%test _ = integer_test "\xef" 100
let%test _ = integer_test "\x27" (-100)
let%test _ = integer_test "\xfa\x7c" 1_000
let%test _ = integer_test "\xfe\x7c" (-1_000)
let%test _ = integer_test "\x1c\x27\x10" 10_000
let%test _ = integer_test "\x1c\xd8\xf0" (-10_000)
let%test _ = integer_test "\x1d\x00\x01\x86\xa0" 100_000
let%test _ = integer_test "\x1d\xff\xfe\x79\x60" (-100_000)

let boolean =
  integer >>= function
  | 0 -> return false
  | 1 -> return true
  | _ -> error "invalid boolean"

type nibble_repr =
  | Digit of char
  | Decimal_point
  | E
  | Eminus
  | Minus
  | End_of_number

let interpret_nibble = function
  | 0 -> Digit '0'
  | 1 -> Digit '1'
  | 2 -> Digit '2'
  | 3 -> Digit '3'
  | 4 -> Digit '4'
  | 5 -> Digit '5'
  | 6 -> Digit '6'
  | 7 -> Digit '7'
  | 8 -> Digit '8'
  | 9 -> Digit '9'
  | 10 -> Decimal_point
  | 11 -> E
  | 12 -> Eminus
  | 14 -> Minus
  | 15 -> End_of_number
  | _ -> failwith "invalid nibble"

let string_of_nibble_list xs =
  List.map (function
      | Digit c -> String.make 1 c
      | Decimal_point -> "."
      | E -> "e"
      | Eminus -> "e-"
      | Minus -> "-"
      | End_of_number -> ""
    ) xs
  |> String.concat ""

let interpret_int_as_nibble_pair i =
  interpret_nibble ((i land 0xf0) lsr 4),
  interpret_nibble (i land 0x0f)

let real =
  let rec loop acc =
    read_u8 >>= fun b ->
    match interpret_int_as_nibble_pair b with
    | End_of_number, End_of_number -> return (List.rev acc)
    | n, End_of_number -> return (List.rev (n :: acc))
    | End_of_number, _ -> error "invalid real"
    | n1, n2 -> loop (n2 :: n1 :: acc)
  in
  read_u8 >>= fun b0 ->
  if b0 <> 30 then error "invalid real"
  else
    (loop [] >>| string_of_nibble_list)

let real_test input value =
  let r = from_bytes (Bytes.of_string input) real in
  r = value

let%test _ = real_test "\x1e\xe2\xa2\x5f" "-2.25"
let%test _ = real_test "\x1e\x0a\x14\x05\x41\xc3\xff" "0.140541e-3"

let number =
  peek_byte >>= function
  | '\x1e' -> real >>| fun s -> Real (float_of_string s)
  | _ -> integer >>| fun i -> Integer i
        
  
let array size elt_reader =
  let rec loop i acc =
    if i = size then
      return (Array.of_list (List.rev acc))
    else
      elt_reader >>= fun elt ->
      loop (i + 1) (elt :: acc)
  in
  loop 0 []

let chunk start_offset end_offset =
  let len = end_offset - start_offset in
  let r = Bytes.create len in
  let rec loop i =
    if i = len then return (Bytes.unsafe_to_string r)
    else
      read_byte >>= fun c ->
      Bytes.set r i c ;
      loop (i + 1)
  in
  loop 0

let index elt_reader =
  card16 >>= fun count ->
  if count = 0 then return []
  else
    card8 >>= fun offSize ->
    let offset_reader =
      match offSize with
      | 1 -> card8
      | 2 -> card16
      | 3 -> card24
      | 4 -> card32
      | _ -> error "invalid offSize"
    in
    array (count + 1) offset_reader >>= fun offset ->
    let rec loop i acc =
      if i = count then
        return (List.rev acc)
      else
        elt_reader offset.(i) offset.(i + 1) >>= fun s ->
        loop (i + 1) (s :: acc)
    in
    loop 0 []

let name_index = index chunk

let header =
  card8 >>= fun major ->
  card8 >>= fun minor ->
  card8 >>= fun hdrSize ->
  card8 >>| fun offSize ->
  { major ; minor ; hdrSize ; offSize }

let sid = read_uint16
  
let _top_dict_entry : top_dict_entry Reader_monad.t =
  read_u8 >>= fun b0 ->
  match b0 with
  | 0 -> sid >>| fun i -> `Version i
  | 1 -> sid >>| fun i -> `Notice i
  | 2 -> sid >>| fun i -> `FullName i
  | 3 -> sid >>| fun i -> `FamilyName i
  | 4 -> sid >>| fun i -> `Weight i
  | 12 -> (
    read_u8 >>= fun b1 ->
    match b1 with
    | 0 -> sid >>| fun i -> `Copyright i
    | 1 -> boolean >>| fun b -> `isFixedPitch b
    | 2 -> number >>| fun n -> `ItalicAngle n
    | 3 -> number >>| fun n -> `UnderlinePosition n
    | 4 -> number >>| fun n -> `UnderlineThickness n
    | 5 -> number >>| fun n -> `PaintType n
    | 6 -> number >>| fun n -> `CharstringType n
    | _ -> error "invalid top dict operator code (second)"
  )
  | n -> errorf "invalid top dict operator code %d" n

let top_dict_operator =
  read_u8 >>= fun b0 ->
  match b0 with
  | 0 -> return `Version
  | 1 -> return `Notice
  | 2 -> return `FullName
  | 3 -> return `FamilyName
  | 4 -> return `Weight
  | 5 -> return `FontBBox
  | 12 -> (
    read_u8 >>= fun b1 ->
    match b1 with
    | 0 -> return `Copyright
    | 1 -> return `isFixedPitch
    | 2 -> return `ItalicAngle
    | 3 -> return `UnderlinePosition
    | 4 -> return `UnderlineThickness
    | 5 -> return `PaintType
    | 6 -> return `CharstringType
    | 7 -> return `FontMatrix
    | 8 -> return `StrokeWidth
    | 20 -> return `SyntheticBase
    | 21 -> return `Postscript
    | 22 -> return `BaseFontName
    | 23 -> return `BaseFontBlend
    | 30 -> return `ROS
    | 31 -> return `CIDFontVersion
    | 32 -> return `CIDFontRevision
    | 33 -> return `CIDFontType
    | 34 -> return `CIDCount
    | 35 -> return `UIDBase
    | 36 -> return `FDArray
    | 37 -> return `FDSelect
    | 38 -> return `FontName
    | n -> errorf "invalid top dict operator code 12 %d" n
  )
  | 13 -> return `UniqueID
  | 14 -> return `XUID
  | 15 -> return `charset
  | 16 -> return `Encoding
  | 17 -> return `CharStrings
  | 18 -> return `Private
  | n -> errorf "invalid top dict operator code %d" n
  
let top_dict_token =
  peek_byte >>= fun b0 ->
  match b0 with
  | '\000'..'\021' -> 
     top_dict_operator >>| fun op -> `Operator op
  | '\028' | '\029' | '\032'..'\254' ->
     integer >>| fun i -> `Integer i
  | '\030' ->
     real >>| fun f -> `Real f
  | '\022'..'\027' | '\031' | '\255' ->
     error "invalid top dict token"

let top_dict _start_offset end_offset =
  let rec loop acc =
    tell >>= fun i ->
    if i >= end_offset then return acc
    else
      top_dict_token >>= fun t ->
      loop (t :: acc)
  in
  loop []

let top_dict_index = index top_dict

let parse s =
  from_bytes s (
      header >>= fun h ->
      seek h.hdrSize >>= fun () ->
      name_index >>= fun ni ->
      top_dict_index >>= fun tdi ->
      return (h, ni, tdi)
    )
