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

import Native.DateOp

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
stdTimeStamp = TimeStamp 1970 0 0 0 0 0 0

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

{-| Convert a TimeStamp to a Date.
--}
toDate:TimeStamp -> Date.Date
toDate ts = Native.DateOp.fromMoment ts.year ts.month ts.day ts.hour ts.minute ts.second ts.millisecond

{-| Converts a Month used in Date to an Int.
--}
fromMonth : Date.Month -> Int
fromMonth m = case m of
    Date.Jan -> 0
    Date.Feb -> 1
    Date.Mar -> 2
    Date.Apr -> 3
    Date.May -> 4
    Date.Jun -> 5
    Date.Jul -> 6
    Date.Aug -> 7
    Date.Sep -> 8
    Date.Oct -> 9
    Date.Nov -> 10
    Date.Dec -> 11

{-| Converts a weekday used in Date to an Int. The range is 0 to 6 and starts with Monday.
--}
fromWeekDay : Date.Day -> Int
fromWeekDay d=case d of
    Date.Sun ->0
    Date.Mon ->1
    Date.Tue ->2
    Date.Wed ->3
    Date.Thu ->4
    Date.Fri ->5
    Date.Sat ->6

alter : (Int -> Int -> Int) -> TimeStamp -> TimeStamp -> TimeStamp
alter op ts1 ts2 = TimeStamp
    (op ts1.year ts2.year)
    (op ts1.month ts2.month)
    (op ts1.day ts2.day)
    (op ts1.hour ts2.hour)
    (op ts1.minute ts2.minute)
    (op ts1.second ts2.second)
    (op ts1.millisecond ts2.millisecond)
    
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