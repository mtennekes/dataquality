#' Date difference
#'
#' Function that returns the difference between two dates in various formats
#'
#' @param date1 first date
#' @param date2 second date
#' @param format format
#' @param unit unit
#' @return description of the returned object
#' @export
#' @example ../examples/dateDiff.R
dateDiff <- function(date1, date2 = Sys.Date(), format="%d-%m-%Y", unit="days")
{
  ##Check which difference to return
  correctUnit = FALSE
  diff <- 0
  
  ##Calculate days
  if(unit == "days") {
    correctUnit = TRUE
    ##calculate days 
    diff <- as.double(as.Date(date2, format) - as.Date(date1, format))
    ##show result
    cat("Time difference of", diff, "days\n")
  }
  
  ##Calculate weeks
  if(unit == "weeks") {
    correctUnit = TRUE
    ##calculate weeks
    diff <- as.double(difftime(as.Date(date2, format), as.Date(date1, format), units="weeks"))
    ##show result
    cat("Time difference of", diff, "weeks\n")
  }
  
  ##Calculate months and years
  if(unit == "months" || unit == "years") {
    correctUnit = TRUE
    ##Determine difference years and months
    lt <- as.POSIXlt(c(as.Date(date1, format), as.Date(date2, format)))
    years <- lt$year[2] - lt$year[1]
    months <- lt$mon[2] - lt$mon[1]     
  
    ##for years
    if(unit == "years") {
      ##Get years
      diff <- years
      ##show result
      cat("Time difference of", diff, "years\n")
    } else { ## for months
      ##include years difference if needed
      if(years > 0) { months <- months + (years*12) }
      ##Get months
      diff <- months
      ##show result
      cat("Time difference of", diff, "months\n")
    }   
  } 
 
  ##Check if diff has been calculated
  if(!correctUnit) cat("Incorrect unit provided, use 'days', 'weeks', 'months' or 'years'") 
  
  ##return a single number
  invisible(diff)
}

dayDiff <- function(date1, date2 = Sys.Date(), format="%d-%m-%Y")
{
  ##Calculate differences in days
  days <- as.double(as.Date(date2, format) - as.Date(date1, format))
  
  ##show result
  cat("Time difference of", days, "days\n")
  
  ##return a single number
  invisible(days)
}

weekDiff <- function(date1, date2 = Sys.Date(), format="%d-%m-%Y")
{
  ##Calculate differences in weeks
  weeks <- as.double(difftime(as.Date(date2, format), as.Date(date1, format), units="weeks"))
  
  ##show result
  cat("Time difference of", weeks, "weeks\n")
  
  ##return a single number  
  invisible(weeks)
}

monthDiff <- function(date1, date2 = Sys.Date(), format="%d-%m-%Y")
{
  ##Calculate difference
  lt <- as.POSIXlt(c(as.Date(date1, format), as.Date(date2, format)))
  years <- lt$year[2] - lt$year[1]
  months <- lt$mon[2] - lt$mon[1]     
  
  ##Include year differences
  if(years > 0) { months <- months + (years*12) }
     
  ##show result
  cat("Time difference of", months, "months\n")
  
  ##Return number
  invisible(months)
}

yearDiff <- function(date1, date2 = Sys.Date(), format="%d-%m-%Y")
{
  ##Calculate difference
  lt <- as.POSIXlt(c(as.Date(date1, format), as.Date(date2, format)))
  years <- lt$year[2] - lt$year[1]     
  
  ##show result
  cat("Time difference of", years, "years\n")
  
  ##Return number
  invisible(years) 
}
