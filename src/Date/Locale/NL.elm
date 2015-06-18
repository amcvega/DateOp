module Date.Locale.NL(localize) where

import Date
import Date.Op

long : Date.Month -> String
long m = case m of
    Date.Jan -> "Januari"
    Date.Feb -> "Februari"
    Date.Mar -> "Maart" 
    Date.Apr -> "April"
    Date.May -> "Mei"   
    Date.Jun -> "Juni"
    Date.Jul -> "Juli"
    Date.Aug -> "August" 
    Date.Sep -> "September"
    Date.Oct -> "October"
    Date.Nov -> "November"  
    Date.Dec -> "December"
    
short : Date.Month -> String
short m = case m of
    Date.Jan -> "Jan"
    Date.Feb -> "Feb"
    Date.Mar -> "Mrt"
    Date.Apr -> "Apr"
    Date.May -> "Mei"
    Date.Jun -> "Jun"
    Date.Jul -> "Jul"
    Date.Aug -> "Aug"
    Date.Sep -> "Sep"
    Date.Oct -> "Oct"
    Date.Nov -> "Nov"
    Date.Dec -> "Dec"
    
day : Date.Day -> String
day d = case d of
    Date.Mon -> "Ma"
    Date.Tue -> "Di"
    Date.Wed -> "Wo"
    Date.Thu -> "Do"
    Date.Fri -> "Vr"
    Date.Sat -> "Za"
    Date.Sun -> "Zo"

localize : Date.Op.TokenDict -> Date.Op.TokenDict
localize = Date.Op.localize short long day