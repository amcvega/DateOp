Elm.Native.DateOp = {};
Elm.Native.DateOp.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.DateOp = localRuntime.Native.DateOp || {};
	if (localRuntime.Native.DateOp.values)
	{
		return localRuntime.Native.DateOp.values;
	}

	var Result = Elm.Result.make(localRuntime);
	
	function constructDate(y,m,d,h,mi,s,ms)
	{
		var date = new window.Date(y,m,d,h,mi,s,ms);
		if (y>=0 && y<100)
		{
			date.setFullYear(y); //force the correct year - see: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Date#Example:_Two_digit_years_map_to_1900_-_1999
		}
		return date;
	}

	function constructLocalDate(y,m,d,h,mi,s,ms)
	{
		var date = constructDate(y,m,d,h,mi,s,ms)
		return new Date( Date.UTC(y,m,d,h,mi,s,ms) )
	}
	
	return localRuntime.Native.DateOp.values = {
		fromMoment: F7(constructDate),
		fromLocalMoment: F7(constructLocalDate)
	};

};
