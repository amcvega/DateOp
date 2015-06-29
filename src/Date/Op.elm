module Date.Op(..) where
--module Date.Op(format, formatWithDict, stdTokens) where

{-| This module can be used for easier interoperability between string and date object.

# Format
@docs format, formatWithDict

# Parse
@docs parse, parseWithDict

# Types
@docs Token, TokenParser, TokenFormater, TokenParser, TokenRegex, TokenDict

# Constants
@docs baseTokens, extTokens, stdTokens

# Localize
@docs localize

--}

import Date.TimeStamp as TimeStamp exposing (emptyTimeStampFragment)

import Basics exposing (..)
import Maybe exposing(andThen, withDefault, map)
import Date exposing(year, month, day, dayOfWeek, hour, minute, second, millisecond, Date)
import String
import List exposing((::),head, tail, drop)
import Regex
import Result
import Dict

{-| A token is a Symbol or group of Symbols that signify a formatted time to be inserted or parsed
--}
type alias Token = String

{-| A TokenParser is a function that takes a String and returns a TimeStampFragment.
--}
type alias TokenParser = String -> TimeStamp.TimeStampFragment

{-| A TokenFormatter is a function that formats one aspect of a date.
--}
type alias TokenFormater = Date -> String

{-| This is just an alias of a String. It signifies a pattern that matches an aspect of a date in a string.
--}
type alias TokenRegex = String

{-| A TokenDict is a special Dictionary that contains all the information and functions to format and parse dates.
--}
type alias TokenDict = Dict.Dict Token (TokenFormater, Maybe (TokenRegex, TokenParser))

escapeString : String
escapeString = "/"
    

{-| This is a standard TokenDict. With this object it is possible to format and parse most strings.
It does not contain any localization information. Use localize for that. The tokens are based on the 
format specified for the date() function in PHP. Characters can be escape by prefixing them with '/'.

For more information check [here.](http://php.net/manual/en/function.date.php)

implements from date(): d, j, m, Y, G, H, i, s, u
not implemented from date(): D, j, l, S, z, W, F, M, t, L, o, B
--}
baseTokens : TokenDict
baseTokens = 
    let justInt = String.toInt >> Result.toMaybe
    in Dict.fromList
    [   ( "d"
        ,   ( day >> toString >> String.padLeft 2 '0'
            , Just ( "[0-9]{2}", \p -> {emptyTimeStampFragment | day <- justInt p} )
            )
        )
    ,   ( "j"
        ,   ( day >> toString
            , Just ( "[0-9]{1,2}" , \p -> {emptyTimeStampFragment | day <- justInt p} )
            )
        )
    ,   ( "m"
        ,   ( month >> TimeStamp.fromMonth >> toString >> String.padLeft 2 '0'
            , Just ( "[0-9]{2}" , \p -> {emptyTimeStampFragment | month <- justInt p} )
            )
        )
    ,   ( "n"
        ,   ( month >> TimeStamp.fromMonth >> toString
            , Just ( "[0-9]{1,2}" , \p -> {emptyTimeStampFragment | month <- justInt p} )
            )
        )
    ,   ( "Y"
        ,   ( year >> toString
            , Just ( "[0-9]{4}" , \p -> {emptyTimeStampFragment | year <- justInt p} )
            )
        )
    ,   ( "G"
        ,   ( hour >> toString
            , Just ( "[0-9]{1,2}" , \p -> {emptyTimeStampFragment | hour <- justInt p} )
            )
        )
    ,   ( "H"
        ,   ( hour >>toString >> String.padLeft 2 '0'
            , Just ( "[0-9]{2}" , \p -> {emptyTimeStampFragment | hour <- justInt p} )
            )
        )
    ,   ( "i"
        ,   ( minute >>toString >> String.padLeft 2 '0'
            , Just ( "[0-9]{2}" , \p -> {emptyTimeStampFragment | minute <- justInt p} )
            )
        )
    ,   ( "s"
        ,   ( second  >>toString >> String.padLeft 2 '0'
            , Just ( "[0-9]{2}" , \p -> {emptyTimeStampFragment | second <- justInt p} )
            )
        )
    ,   ( "u"
        ,   ( millisecond  >>toString >> String.padLeft 3 '0'
            , Just ( "[0-9]{3}" , \p -> {emptyTimeStampFragment | millisecond <- justInt p} )
            )
        )
    ]
    
{-| Another TokenDict to extend the base with patterns that won't directly map to a TimeStamp. These tokens can not be used with the parser.

implements: a, A, w, N, g, h, y
--}
extTokens : TokenDict
extTokens = 
    let
        weekN = TimeStamp.fromWeekDay >> (+) 1
        weekW = \d -> case weekN d of
            7->0
            x->x
    in Dict.fromList
    [   ( "a"
        ,   ( \dt->if hour dt <13 then "am" else "pm"
            , Nothing
            )
        )
    ,   ( "A"
        ,   ( \dt->if hour dt <13 then "AM" else "PM"
            , Nothing
            )
        )
    ,   ( "w"
        ,   ( dayOfWeek >> TimeStamp.fromWeekDay >> toString
            , Nothing
            )
        )
    ,   ( "N"
        ,   ( dayOfWeek >> TimeStamp.fromWeekDay >> toString
            , Nothing
            )
        )
    ,   ( "g"
        ,   ( hour >> (%) 12 >> toString
            , Nothing
            )
        )
    ,   ( "h"
        ,   ( hour >> (%) 12 >> toString >> String.padLeft 2 '0'
            , Nothing
            )
        )
    ,   ( "y"
        ,   ( year >> toString >> String.right 2
            , Nothing
            )
        )
    ]
    
indexOf : compareable -> List compareable -> Int -> Maybe Int
indexOf needle (x::xs) current = if | x==needle -> Just current
                                    | x/=needle && (not <| List.isEmpty xs) -> indexOf needle xs (current+1)
                                    | otherwise -> Nothing

{-| Function to add localized content to a TokenDict. The first argument should map a Date.Month to a short representation.
The second argument should map to a long representation. The third should map days of the week to Strings. The last argument 
contains the TokenDict to be localized. This is commonly stdTokens. The package provides some localizations in the Locale
modules.

implements: F,M,D
--}
localize : (Date.Month -> String) -> ( Date.Month -> String ) -> (Date.Day -> String) -> TokenDict -> TokenDict
localize shortM longM dow base =
    let
        monthList = [Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun, Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec]
        shortMs = List.map shortM monthList
        longMs = List.map longM monthList
    in extendTokenDict base <| Dict.fromList
        [   ( "F" -- long month
            ,   ( month >> longM
                , Just ( String.join "|" longMs , \p -> {emptyTimeStampFragment | month <- indexOf p longMs 0} )
                )
            )
        ,   ( "M" -- short month
            ,   ( month >> shortM
                , Just ( String.join "|" shortMs , \p -> {emptyTimeStampFragment | month <- indexOf p shortMs 0} )
                )
            )
        ,   ( "D" -- short month
            ,   ( dayOfWeek >> dow
                , Nothing
                )
            )
        ]
    
{-| This function can be used to extend a TokenDict with user-defined tokens.
--}
extendTokenDict : TokenDict -> TokenDict -> TokenDict
extendTokenDict base extend = Dict.union extend base

stdTokens : TokenDict
stdTokens = extendTokenDict baseTokens extTokens

{-| This creates a pattern for a regular expression to be used in formatting and parsing.
--}
createRegexString : TokenDict -> String
createRegexString tdict = 
    let 
        t=Dict.keys tdict
        match=String.join "|" t
        prefix=String.concat t
    in
        String.concat ["(^|"++escapeString++"|[^", prefix,"]*)","(",match,")"]

{-| Format takes a string and a date and returns a formatted date as a string. The string has to contain tokens as specified in stdToken.

    formatISO = format "Y-m-dTH:i:s"
    --formatISO (Date.fromTime 0) results in 1970-1-1T01:00:00 
    
To escape a character prefix it with a '/'.
--}
format : String -> Date -> String
format = formatWithDict stdTokens

{-| This is the format function with the ability to specify the TokenDict. This can be useful to extend the functionality or apply localization.
-}
formatWithDict : TokenDict -> String -> Date -> String
formatWithDict tdict s d = Regex.replace Regex.All (createRegexString tdict |> Regex.regex) (formatToken tdict d) s

{-| Used to format a found token in a string. It looks for the token in a TokenDict and applies its TokenFormater function.
If it notices a prefix of '/' it will just return the matched token wihthout the prefix.
-}
formatToken : TokenDict -> Date -> Regex.Match -> String
formatToken tokendict d {submatches} =
    let
        (prefix::symbol::_) = List.map (withDefault "") submatches
        transform = case Dict.get symbol tokendict of
            Just (format, _)->format
            Nothing->\dt->""
    in if prefix==escapeString then symbol else prefix ++ transform d
    
{-| Replaces tokens in a pattern with a TokenRegex. It also returns a list of TokenParsers for the used tokens.
Only works with tokens from baseTokens.
--}
transformPattern : TokenDict -> String -> (String, List TokenParser)
transformPattern tdict pattern = 
    let
        format = createRegexString tdict |> Regex.regex -- create a RegexPattern from the TokenDict
        string' = Regex.replace Regex.All format (transformToken tdict) pattern -- replace the found tokens with their corresponding RegexPattern from the TokenDict
        tokens = Regex.find Regex.All format pattern |> List.map .submatches |> List.map (List.map (withDefault "")) |> List.map valid |> List.concat
        lookup = \token -> Dict.get token tdict |> \l -> case l of -- lookup a token and return a parser if it is available
            Just (_, Just (_, parser)) -> [parser]
            _ -> []
        valid = \[prefix, second] -> if prefix==escapeString then [] else lookup second -- filter which are prefixed or invalid
    in (string', tokens)

{-| Replaces the match with either a Regex pattern or the escaped symbol.
--}
transformToken : TokenDict -> Regex.Match -> String
transformToken tokendict {submatches} =
    let 
        (prefix::symbol::_) = List.map (withDefault "") submatches
        transform = case Dict.get symbol tokendict of
            Just (_, parseable) -> parseable
            Nothing -> Nothing
    in case transform of 
        Just (format,_) -> if (prefix==escapeString) then symbol else prefix ++ "(" ++ format ++ ")"
        Nothing -> prefix ++ symbol

{-| Parses a string according to the given pattern and maybe returns a date.

    parse "Y-m-d" "2015-06-16"

This Returns a Maybe Date with the year, month and day set to the information in the string. The time will be zeroed out.

    parse "m" "03"
    
This will parse only the month. Note however that the year will be set to 1970 and the day to the first of the month.

WARNING: If the string parses correct but contains out of range values the date may be set to the same value as Date.fromTime 0
--}
parse : String -> String -> Maybe Date
parse = parseWithDict baseTokens

{-| This function much like formatWithDict takes a TokenDict to parse a string to a date.
--}
parseWithDict : TokenDict -> String -> String -> Maybe Date
parseWithDict tdict pattern dstring =
    let
        (pattern', parsers) = transformPattern tdict pattern --transform the pattern and get the corresponding parsers
        raw = case Regex.find (Regex.AtMost 1) (Regex.regex pattern') dstring of --check if the pattern matches the given string and extract the submatches
            [{submatches}] -> Just submatches -- submatches should line up with the parsers, bad things happen if they dont
            _ -> Nothing -- if there is no match propagate a Nothing
        results = map (List.map (withDefault "")) raw -- extract the found submatches
        parsed = map (List.map2 (<|) parsers) results -- parse the submatches
        merged = map (List.foldl TimeStamp.union TimeStamp.emptyTimeStampFragment) parsed -- fold the found TimeStampFragments into each other
        tupled = map TimeStamp.fromFragment merged -- convert the TimeStampFragment to a TimeStamp
        finaldate = map TimeStamp.toDate tupled -- convert it to a complete Date
    in finaldate