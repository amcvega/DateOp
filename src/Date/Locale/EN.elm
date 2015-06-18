module Date.Locale.NL(localize) where

import Date
import Date.Op

long : Date.Month -> String
long m = case m of
    Date.Jan -> "January"
    Date.Feb -> "February"
    Date.Mar -> "March"
    Date.Apr -> "April"
    Date.May -> "May"
    Date.Jun -> "June"
    Date.Jul -> "July"
    Date.Aug -> "August"
    Date.Sep -> "September"
    Date.Oct -> "October"
    Date.Nov -> "November"
    Date.Dec -> "December"
    
short : Date.Month -> String
short m = case m of
    Date.Jan -> "Jan"
    Date.Feb -> "Feb"
    Date.Mar -> "Mar"
    Date.Apr -> "Apr"
    Date.May -> "May"
    Date.Jun -> "Jun"
    Date.Jul -> "Jul"
    Date.Aug -> "Aug"
    Date.Sep -> "Sep"
    Date.Oct -> "Oct"
    Date.Nov -> "Nov"
    Date.Dec -> "Dec"
    
day : Date.Day -> String
day d = case d of
    Date.Mon -> "Monday"
    Date.Tue -> "Tuesday"
    Date.Wed -> "Wednesday"
    Date.Thu -> "Thursday"
    Date.Fri -> "Friday"
    Date.Sat -> "Saturday"
    Date.Sun -> "Sunday"

localize : Date.Op.TokenDict -> Date.Op.TokenDict
localize = Date.Op.localize short long day