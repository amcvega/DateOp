module Date.Add (..) where
{-| Methods to alter dates.

# General
@docs timestamp

# Specific
@ docs year, month, day, hour, minute, millisecond

-}

import Date

import Date.TimeStamp

empty : Date.TimeStamp.TimeStamp
empty = Date.TimeStamp.emptyTimeStamp

timestamp : Date.Date -> Date.TimeStamp.TimeStamp -> Date.Date
timestamp date ts = Date.TimeStamp.fromDate date |> Date.TimeStamp.alter (+) ts |> Date.TimeStamp.toDate

year : Int -> Date.Date -> Date.Date
year u d = { empty | year <- u} |> timestamp d

month : Int -> Date.Date -> Date.Date
month u d = { empty | month <- u} |> timestamp d

day : Int -> Date.Date -> Date.Date
day u d = { empty | day <- u} |> timestamp d

hour : Int -> Date.Date -> Date.Date
hour u d = { empty | hour <- u} |> timestamp d

minute : Int -> Date.Date -> Date.Date
minute u d = { empty | minute <- u} |> timestamp d

second : Int -> Date.Date -> Date.Date
second u d = { empty | second <- u} |> timestamp d

millisecond : Int -> Date.Date -> Date.Date
millisecond u d = { empty | millisecond <- u} |> timestamp d