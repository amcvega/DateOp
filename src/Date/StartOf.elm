module Date.StartOf(year, month, week, day, hour, minute, second) where
{-| This module provides functions to easily calculate the start of a period of time.
--}

import Date exposing (Date)
import Date.TimeStamp as TimeStamp exposing(stdTimeStamp)

year : Date->Date
year d = TimeStamp.toDate <| { stdTimeStamp | year <- Date.year d }

month : Date->Date
month d = TimeStamp.toDate <|
    { stdTimeStamp 
    | year <- Date.year d
    , month <- Date.month d |> TimeStamp.fromMonth
    }

week : Date->Date
week d = TimeStamp.toDate <|
    { stdTimeStamp
    | year <- Date.year d
    , month <- Date.month d |> TimeStamp.fromMonth
    , day <- Date.day d - (Date.dayOfWeek d |> TimeStamp.fromWeekDay)
    }

day : Date->Date
day d = TimeStamp.toDate <|
    { stdTimeStamp
    | year <- Date.year d
    , month <- Date.month d |> TimeStamp.fromMonth
    , day <- Date.day d
    }

hour : Date->Date
hour d = TimeStamp.toDate <|
    { stdTimeStamp 
    | year <- Date.year d
    , month <- Date.month d |> TimeStamp.fromMonth
    , day <- Date.day d
    , hour <- Date.hour d
    }

minute : Date->Date
minute d = TimeStamp.toDate <|
    { stdTimeStamp 
    | year <- Date.year d
    , month <- Date.month d |> TimeStamp.fromMonth
    , day <- Date.day d
    , hour <- Date.hour d
    , minute <- Date.minute d
    }

second : Date->Date
second d = TimeStamp.toDate <|
    { stdTimeStamp 
    | year <- Date.year d
    , month <- Date.month d |> TimeStamp.fromMonth
    , day <- Date.day d
    , hour <- Date.hour d
    , minute <- Date.minute d
    , second <- Date.second d
    }
