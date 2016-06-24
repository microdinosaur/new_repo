#' A Time Vector Function
#' 
#' This function allows for the creation of a vector between two dates for TS data handling.
#' @param Starting Date, Ending Date, Time Split, Time Zone (defaults to EST)
#' @keywords time-series
#' @export
#' @examples 
#' #Example Dates
#' x <- ISOdatetime(2014,01,01,01,00,00)
#' y <- ISOdatetime(2016,06,24,18,00,00)
#' 
#' #Vector by Month
#' tsVec(x,y,"month")
#' 
#' #Vector by Week
#' tsVec(x,y,"week","EST")
#' 
#' #Vector by Day
#' tsVec(x,y,"day","EST")
#' 
#' #Vector by Hour
#' tsVec(x,y,"hour","GMT")
#' tsVec()
#'
#' 
#'   
tsVec <- function(x, y, BY = c("month", "week","day", "hour"), TZ = "EST"){
  
  #Pull Starting Date Info  
  yearS <- lubridate::year(x)
  monthS <- lubridate::month(x)
  dayS <- lubridate::day(x)
  hourS <- lubridate::hour(x)
  minS <- lubridate::minute(x)
  secS <- lubridate::second(x)
  
  #Pull Ending Date Info
  yearE <- lubridate::year(y)
  monthE <- lubridate::month(y)
  dayE <- lubridate::day(y)
  hourE <- lubridate::hour(y)
  minE <- lubridate::minute(y)
  secE <- lubridate::second(y)
    
  if (BY == "month"){
    
    a <- seq.POSIXt(ISOdatetime(yearS,monthS,dayS,hourS,minS,secS, tz = TZ),
                 ISOdatetime(yearE,monthE,dayE,hourE,minE,secE, tz = TZ), by = BY)
    
  } else if (BY == "week"){
    
    a <- seq.POSIXt(ISOdatetime(yearS,monthS,dayS,hourS,minS,secS, tz = TZ),
               ISOdatetime(yearE,monthE,dayE,hourE,minE,secE, tz = TZ), by = BY)
    
  } else if (BY == "day"){
    
    a <- seq.POSIXt(ISOdatetime(yearS,monthS,dayS,hourS,minS,secS, tz = TZ),
               ISOdatetime(yearE,monthE,dayE,hourE,minE,secE, tz = TZ), by = BY)
    
  } else if (BY == "hour"){
    
    a <- seq.POSIXt(ISOdatetime(yearS,monthS,dayS,hourS,minS,secS, tz = TZ),
               ISOdatetime(yearE,monthE,dayE,hourE,minE,secE, tz = TZ), by = BY)
    
  }
    return <- a
    print(return)
}