module Date.TimeStamp(..) where

{-|
Utility functions used by Date.Op but may be of some use.

# Utils
@docs fromMonth, fromWeekDay

# Convert
@docs toIsoDate, toDate

# Build TimeStamp
@docs TimeStamp, fromDate, fromFragment, stdTimeStamp, fromFragmentWithTimeStamp

# Build TimeFragment
@docs TimeStampFragment, union, emptyTimeStampFragment

--}

import Date
import String
import Result
import Maybe exposing (oneOf, withDefault)

{-| This type is used to hold the information gathered by the parser and can be easily converted to a Date.
--}
type alias TimeStamp =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }

{-| This type is used to describe zero or more aspects of a TimeStamp.
--}
type alias TimeStampFragment =
    { year : Maybe Int
    , month : Maybe Int
    , day : Maybe Int
    , hour : Maybe Int
    , minute : Maybe Int
    , second : Maybe Int
    , millisecond : Maybe Int
    }

{-| A standard TimeStamp used to fill empty aspects of a TimeStampFragment at conversion.
--}
stdTimeStamp : TimeStamp
stdTimeStamp = TimeStamp 1970 1 1 0 0 0 0

emptyTimeStamp : TimeStamp
emptyTimeStamp = TimeStamp 0 0 0 0 0 0 0

{-| Creates a empty fragment
--}
emptyTimeStampFragment : TimeStampFragment
emptyTimeStampFragment = TimeStampFragment Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

{-| Creates a TimeStamp from a TimeStampFragment. If the fragment does not 
describe an aspect the information from the provided TimeStamp is used instead.
--}
fromFragmentWithTimeStamp : TimeStamp -> TimeStampFragment -> TimeStamp
fromFragmentWithTimeStamp std tsf =
    { year = withDefault std.year tsf.year
    , month = withDefault std.month tsf.month
    , day = withDefault std.day tsf.day
    , hour = withDefault std.hour tsf.hour
    , minute = withDefault std.minute tsf.minute
    , second = withDefault std.second tsf.second
    , millisecond = withDefault std.millisecond tsf.millisecond
    }
    
{-| Creates a TimeStamp from a TimeStampFragment. If the fragment does not 
describe an aspect the information from stdTimeStamp is used instead.
--}
fromFragment : TimeStampFragment -> TimeStamp
fromFragment tsf = fromFragmentWithTimeStamp stdTimeStamp tsf

{-| Creates a TimeStamp from a date.
--}
fromDate : Date.Date -> TimeStamp
fromDate d =
    { year = Date.year d
    , month = Date.month d |> fromMonth
    , day = Date.day d
    , hour = Date.hour d
    , minute = Date.minute d
    , second = Date.second d
    , millisecond = Date.millisecond d
    }

{-| Used by to toDate to convert to a date.
--}
toIsoDate : TimeStamp -> String
toIsoDate ts =
    toString ts.year ++ "-" ++
    ( toString ts.month |>String.padLeft 2 '0' ) ++ "-" ++
    ( toString ts.day |>String.padLeft 2 '0' ) ++ "T" ++
    ( toString ts.hour |>String.padLeft 2 '0' ) ++ ":" ++
    ( toString ts.minute |>String.padLeft 2 '0' ) ++":" ++
    ( toString ts.second |> String.padLeft 2 '0')

--replace ASAP with a better function
{-| This is kind of an ugly hack to guarantee that a tuple gets converted to a date as the tuple constructor of Date is not available yet.
--}
toDate:TimeStamp -> Date.Date
toDate ts = toIsoDate ts |> Date.fromString |> Result.toMaybe |> Maybe.withDefault (Date.fromTime 0)

{-| Converts a Month used in Date to an Int.
--}
fromMonth : Date.Month -> Int
fromMonth m = case m of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12

{-| Converts a weekday used in Date to an Int. The range is 0 to 6 and starts with Monday.
--}
frowWeekDay : Date.Day -> Int
fromWeekDay d=case d of
    Date.Mon ->0
    Date.Tue ->1
    Date.Wed ->2
    Date.Thu ->3
    Date.Fri ->4
    Date.Sat ->5
    Date.Sun ->6

{-| Combines two fragments. Values from the base(first argument) are given precedence.
--}
union : TimeStampFragment -> TimeStampFragment -> TimeStampFragment
union base extend = TimeStampFragment
    ( oneOf [base.year, extend.year] )
    ( oneOf [base.month, extend.month] )
    ( oneOf [base.day, extend.day] )
    ( oneOf [base.hour, extend.hour] )
    ( oneOf [base.minute, extend.minute] )
    ( oneOf [base.second, extend.second] )
    ( oneOf [base.millisecond, extend.millisecond] )