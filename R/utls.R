# For yearly data, Second must be 1
rondate <- setClass("rondate",
                        slots = c(First = "numeric", Second = "numeric", Frequency = "numeric"),
                        prototype = list(First = 1, Second = 1, Frequency = 1))
						
setMethod("initialize", "rondate", function(.Object, first,second,frequency)
    {
	if(frequency==1)
	{
	     second=1
		}
	if (first<0 || second<0 || frequency<0)
	stop("Negative values for arguments are not valid.")
	.Object@First=first
	.Object@Second=second
	.Object@Frequency=frequency
	return(.Object)
	})
						
						
setMethod("print", signature(x = "rondate"),function(x)
{
    if (x@Frequency == 1)
    {
        cat(x@First)
    }
    else if (x@Frequency == 4)
    {
        cat(x@First, "q", x@Second, sep = "")
    }
    else if (x@Frequency == 12)
    {
        cat(x@First, "m", x@Second, sep = "")
    }
    else
        {
        cat(x@First, ":", x@Second, "(Frequency=,", x@Frequency, ")", sep = "")
    }
})


setGeneric("Convert2RoNDate", function(object) { standardGeneric("Convert2RoNDate") })
## a string must contain "q" or "m" as indicators of quarterly or monthly data; e.g., 1900q3 or 2001m5
setMethod("Convert2RoNDate", signature(object = "character"), function(object) {
                          tryCatch(
                        {
							object = tolower(object)
							if (grepl(object, "q"))
                            {
								d = strsplit(object, "q")[[1]]
                                if (length(d) == 2)
                                {
                                    year = as.numeric(d[1])
                                    quartermonth = as.numeric(d[2])
                                    return(rondate(year, quartermonth, 4))
                                }
                            }

							if (grepl(object, "m"))
                            {
								d = strsplit(object, "m")[[1]]
                                if (length(d) == 2)
                                {
                                    year = as.numeric(d[1])
                                    quartermonth = as.numeric(d[2])
                                    return(rondate(year, quartermonth, 12))
                                }
                            }

							year = as.numeric(object)
                            return(rondate(year, 0, 1))
                        }, warning = function(war)
                        {
                            pring(war)
                            stop("I could not convert the string to a valid date.")
                        }, error = function(err)
                        {
                            pring(err)
                            stop("I could not convert the string to a valid date.")
                        })
                      }
                      )

 


setGeneric("GetOlderRoNDate",function(date1,date2) { standardGeneric("GetOlderRoNDate") })
setMethod("GetOlderRoNDate", signature(date1="rondate",date2="rondate"),function(date1,date2)
{
if (date1@Frequency != date2@Frequency){
stop("Comparison of 'rondate's with different frequencies are not supported.")}

if (date1@First < date2@First)
        return(date1)

    if (date1@First > date2@First)
        return(date2)

    if (date1@Second < date2@Second)
        return(date1)

    if (date1@Second  > date2@Second )
        return(date2)

    return(date1) # they are equal
})

setGeneric("GetNewerRoNDate", function(date1, date2) { standardGeneric("GetNewerRoNDate") })
setMethod("GetNewerRoNDate", signature(date1 = "rondate", date2 = "rondate"), function(date1, date2) {
	if (date1@Frequency != date2@Frequency) {
		stop("Comparison of 'rondate's with different frequencies are not supported.")
	}

	if (date1@First < date2@First)
		return(date2)

	if (date1@First > date2@First)
		return(date1)

	if (date1@Second < date2@Second)
		return(date2)

	if (date1@Second > date2@Second)
		return(date1)

	return(date1) # they are equal
})

setGeneric(name = "GetIntervalBetweenRoNDates", def = function(date1,date2) { standardGeneric("GetIntervalBetweenRoNDates") })
setMethod("GetIntervalBetweenRoNDates", signature(date1 = "rondate",date2 = "rondate"),function(date1,date2)
{
if (date1@Frequency != date2@Frequency){
stop("Calculating Interval between two 'RonDate's with different frequencies are not supported.")}

if (date1@Frequency == 1)
    {
        return(abs(date1@First - date2@First))
    }	
	olddate = GetOlderRoNDate(date1, date2)
	newdate = GetNewerRoNDate(date1, date2)
    return(frequency * (newdate@First - olddate@First- 1) + newdate@Second + (frequency - olddate@Second))
})

setGeneric("GetNextRoNDates", function(date,count) { standardGeneric("GetNextRoNDates") })
# count can be negative in which a previous date is returned.
setMethod("GetNextRoNDates", signature(date = "rondate",count="numeric"),
definition=function(date,count)
{
if (count == 0)
    {
        return(date)
    }
    if (date@Frequency == 1)
    {
        return(rondate(date@First+count,1,date@Frequency))
    }

    if (count > 0)
    {
        r = count %% date@Frequency
        y = count %/% date@Frequency

        if (date@Second + r > date@Frequency)
        {
		return(rondate(date1@First+y+1,date@Second+r-date@Frequency,date@Frequency))
        }
        else
            {
					return(rondate(date1@First+y,date@Second+r,date@Frequency))
        }
    }
    else if (count < 0)
    {
        r = -count %% date@Frequency
        y = -count %/% date@Frequency

        if (date@Second - r < 0)
        {
            return(c(date@First - y - 1, date1@Frequency - r + date@Second))
        }
        else
            {
            return(c(date@First - y, date@Second - r))
        }
    }
})


