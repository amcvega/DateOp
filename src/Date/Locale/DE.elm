module Date.Locale.NL(localize) where

import Date
import Date.Op

long : Date.Month -> String
long m = case m of
    Date.Jan -> "Januar"
    Date.Feb -> "Februar"
    Date.Mar -> "März" 
    Date.Apr -> "April"
    Date.May -> "Mai"   
    Date.Jun -> "Juni"
    Date.Jul -> "Juli"
    Date.Aug -> "August" 
    Date.Sep -> "September"
    Date.Oct -> "October"
    Date.Nov -> "November"  
    Date.Dec -> "Dezember"
    
short : Date.Month -> String
short m = case m of
    Date.Jan -> "Jan"
    Date.Feb -> "Feb"
    Date.Mar -> "Mrz"
    Date.Apr -> "Apr"
    Date.May -> "Mai"
    Date.Jun -> "Jun"
    Date.Jul -> "Jul"
    Date.Aug -> "Aug"
    Date.Sep -> "Sep"
    Date.Oct -> "Oct"
    Date.Nov -> "Nov"
    Date.Dec -> "Dez"
    
day : Date.Day -> String
day d = case d of
    Date.Mon -> "Montag"
    Date.Tue -> "Dienstag"
    Date.Wed -> "Mittwoch"
    Date.Thu -> "Donnerstag"
    Date.Fri -> "Freitag"
    Date.Sat -> "Samstag"
    Date.Sun -> "Sonntag"

localize : Date.Op.TokenDict -> Date.Op.TokenDict
localize = Date.Op.localize short long day