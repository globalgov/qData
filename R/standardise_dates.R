#' Standardises a wide range of of date inputs
#'
#' The function standardises a wide range of date inputs parsed through it.
#' It accepts date inputs in different formats, incomplete dates,
#' historical dates and future dates. It also creates nested
#' vectors of dates for vague date inputs, ambiguous and ranged dates,
#' into a range of dates.
#' @param ... One (ymd) or three (yyyy, mm, dd) variables
#' @details The function seeks to convert a wide range of dates into
#' dates so that these can be meaningfully used for analysis.
#' There are several limitations of other date wrangling packages
#' and/or functions for dealing with incomplete dates, dates with
#' different or inconsistent formats or historical dates for which
#' `standardise_dates()` can be used.
#' It also converts  ambiguous and ranged dates into a
#' range of dates. The function allows only for dmy or ymd
#' date formats at present, since mdy may introduce errors.
#' @return Nested vector of POSIXct dates that includes a range of dates
#' @importFrom stringr str_detect str_split
#' @importFrom lubridate as_date dmy ymd
#' @examples
#' dates_comparison <- tibble::tribble(~Example, ~OriginalDate,
#' "A normal date", "2010-01-01",
#' "A historical date", "1712-01-01",
#' "A really historical date", "712-01-01",
#' "A very historical date", "012-01-01",
#' "A clearly future date", "9999-12-31",
#' "A not so clearly future date", "2599-12-31")
#' dates_comparison %>% dplyr::mutate(
#' lubridate = lubridate::as_date(OriginalDate),
#' anytime = anytime::anydate(OriginalDate),
#' qData = qData::standardise_dates(OriginalDate)
#' ) %>% print(n = 25)
#' @export
standardise_dates <- standardize_dates <- function(...){
  
  # Step one: if necessary, join multiple columns
  dates <- parse_date_input(...)
  
  # Step two: standardise inputs
  dates <- standardise_date_input(dates)
  
  # Step three: correct date formats and sizes
  dates <- correct_date_format(dates)
  
  # Step four: identifying negative dates and maintaining negative values
  dates <- treat_historical_dates(dates)
  
  # Step five: Spliting dates in two collumns
  ranged_dates <- replicate(1, dates)
  #ranged_dates <- purrr::map(dates, as.character)
  
  # Step six: insert range on incomplete year only dates for ranged dates only
  ranged_dates <- treat_incomplete_dates(ranged_dates)
  ranged_dates <- treat_range_dates(ranged_dates)
  
  # Step seven: removing ranges and completing dates for date column
  dates <- remove_range(dates)
  dates <- complete_dates(dates)
  
  # Step eight: treating future dates
  dates <- treat_future_dates(dates)
  ranged_dates <- treat_future_ranges(ranged_dates)
  
  dates <- lubridate::as_date(dates)
  
  invisible(cbind(dates, ranged_dates))
  
  dates
  
  invisible(ranged_dates)
}

# Helper functions for standardise_dates()
parse_date_input <- function(...){
  dots <- list(...)
  if(length(dots)==1){
    dots <- do.call(as.character, dots)
    dates <- unlist(dots)
  } else if (length(dots)==3){
    dots <- purrr::map(dots, as.character)
    dates <- unlist(purrr::pmap_chr(dots, paste, sep = "-"))
  } else stop("Either you need to pass standardise_dates() one variable (i.e. 'yyyy-mm-dd' or three (yyyy, mm, dd).")
  dates
}

standardise_date_input <- function(dates){
  
  as_bc_dates <- function(dates) {
    dates <- stringr::str_remove_all(dates, "(bc|BC|Bc|bC)") # remove before christ letters
    dates <- paste0("-", dates) # adds a negative sign to date
    dates
  }
  
  dates <- stringr::str_replace_all (dates, "^NA$", "") # makes NAs will not create errors
  dates <- stringr::str_remove_all(dates, "(0000-00-00|00-00-0000|00-00-00)") # standardising null dates
  dates <- stringr::str_replace_all(dates, "\\.", "-") # standardising separaters
  dates <- stringr::str_replace_all(dates, "\\/", "-") # standardising separaters
  dates <- stringr::str_replace_all(dates, "-([:digit:])-", "-0\\1-") # standardising months
  dates <- stringr::str_replace_all(dates, "-([:digit:])$", "-0\\1") # standardising days
  dates <- stringr::str_replace_all(dates, "^([:digit:])-", "0\\1-") # standardising days 2
  dates <- stringr::str_replace_all(dates, "-00|-\\?\\?|-NA", "") # standardising ambiguities
  dates <- stringr::str_replace_all(dates, "_", ":") # standardising ranges
  dates <- stringr::str_remove_all(dates, "(ad|AD|Ad|aD)") # remove after christ
  dates <- ifelse(stringr::str_detect(dates, "(bc|BC|Bc|bC)"), as_bc_dates(dates), dates) # replacing BC for corresponding negative dates
  dates <- stringr::str_trim(dates, side = "both") # removes trailing white spaces
}

correct_date_format <- function(dates){
  
  incomp_dates <- function(dates) {
    
    thresh <- as.numeric(substr(Sys.Date(),1,4))
    x <- matrix(as.numeric(unlist(strsplit(dates, "-"))), ncol=3, byrow = T)
    ypos <- which(apply(x, 2, function(y) any(y>31) | any(nchar(y)==4))) # identify year
    if(length(ypos)!=1) ypos <- 3
    dpos <- setdiff(which(apply(x, 2, function(y) any(y>12) & all(y<32))), ypos) # identify day
    if(length(dpos)==0) dpos <- setdiff(c(1,3), ypos)
    mpos <- setdiff(setdiff(1:3, ypos), dpos) # identify month
    
    if(nrow(x)==1){
      y <- ifelse(nchar(x[ypos])>2,
                  ifelse(x[ypos] > thresh & x[ypos]!="9999",
                         paste0("19", x[ypos] %% 100),
                         formatC(x[ypos], width = 4, flag = "0")), # if 4, keep or correct
                  ifelse(x[ypos] > (thresh %% 100),
                         paste0("19", formatC(x[ypos], width = 2, flag = "0")), # if more than now, likely last century
                         paste0("20", formatC(x[ypos], width = 2, flag = "0")))) # if less than now, likely this century
      m <- formatC(x[mpos], width = 2, flag = "0")
      if(m=="00") m <- "01"
      d <- formatC(x[dpos], width = 2, flag = "0")
      if(d=="00") d <- "01"
      out <- paste(y,m,d,sep = "-")
    } else {
      out <- apply(x, 1, function(x){
        y <- ifelse(nchar(x[ypos])>2,
                    ifelse(x[ypos] > thresh & x[ypos]!="9999",
                           paste0("19",x[ypos] %% 100),
                           formatC(x[ypos], width = 4, flag = "0")), # if 4, keep or correct
                    ifelse(as.numeric(x[ypos]) > (thresh %% 100),
                           paste0("19", formatC(x[ypos], width = 2, flag = "0")), # if more than now, likely last century
                           paste0("20", formatC(x[ypos], width = 2, flag = "0")))) # if less than now, likely this century
        m <- formatC(as.numeric(x[mpos]), width = 2, flag = "0")
        if(m=="00") m <- "01"
        d <- formatC(as.numeric(x[dpos]), width = 2, flag = "0")
        if(d=="00") d <- "01"
        paste(y,m,d,sep = "-")
      })
    }
    dates <- out
  }
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$"), as.character(as.Date(dates,"%d-%m-%Y")), dates) # Correct date order if need
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$"), as.character(as.Date(dates,"%d-%m-%Y")), dates) # Correct date order and size 
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$"), as.character(as.Date(dates,"%d-%m-%Y")), dates) # Correct date order and size
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$"), as.character(as.Date(dates,"%d-%m-%Y")), dates) # Correct date order and size
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{3}-[:digit:]{2}-[:digit:]{2}$"), paste0("0", dates), dates) # correct year size if missing 0 before year
  dates <- ifelse(stringr::str_detect(dates, "^[:alpha:]{3}\\s[:digit:]{2}\\,\\s[:digit:]{4}$"), as.character(as.Date(dates, "%b %d, %Y" )), dates) # Correct format
  dates <- ifelse(stringr::str_detect(dates, "^[:alpha:]{3}\\s[:digit:]{1}\\,\\s[:digit:]{4}$"), as.character(as.Date(dates, "%b %d, %Y" )), dates) # Correct format
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{1}-[:digit:]{1}-[:digit:]{2}$"), incomp_dates(dates), dates) # for incomplete dates with 4 digits only
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{2}-[:digit:]{1}-[:digit:]{2}$"), incomp_dates(dates), dates) # for incomplete dates with 5 digits only
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{1}-[:digit:]{2}-[:digit:]{2}$"), incomp_dates(dates), dates) # for incomplete dates with 5 digits only
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{2}-[:digit:]{2}-[:digit:]{2}$"), incomp_dates(dates), dates) # for incomplete dates with 6 digits only
  dates <- ifelse(stringr::str_detect(dates, "0000-00-00"), NA, dates)
}

treat_historical_dates <- function(dates){
  
  neg_dates_comp <- function(dates) {
    if(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")) { # for negative ymd dates
      ndate <- as.numeric(lubridate::ymd(dates)) 
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
    } else if(stringr::str_detect(dates, "^-[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$")) {
      nd <- lubridate::dmy(dates)
      ndate <- as.numeric(lubridate::as_date(nd))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
    } else {
      dates
    }
  }
  
  neg_dates <- function(dates) {
    if(stringr::str_detect(dates, "^-[:digit:]{4}$")){ # negative dates with 4 digit year only
      nd <- paste0(dates, "-01-01") 
      ndate <- as.numeric(lubridate::as_date(nd))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
    } else if(stringr::str_detect(dates, "^-[:digit:]{3}$")){ # negative dates with 3 digit year only
      ndate <- stringr::str_replace(dates, "-", "0") 
      ndate <- paste0(ndate, "-01-01") 
      ndate <- as.numeric(lubridate::as_date(ndate))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
    } else if(stringr::str_detect(dates, "^-[:digit:]{2}$")){ # negative dates with 2 digit year only
      ndate <- stringr::str_replace(dates, "-", "00") 
      ndate <- paste0(ndate, "-01-01") 
      ndate <- as.numeric(lubridate::as_date(ndate))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
    } else {
      dates
    } 
  }
  
  dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), neg_dates_comp(dates), dates) # negative ymd dates
  dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$"), neg_dates_comp(dates), dates) # negative dates in dmy format
  # todo: expand on negative date formats taken
  dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{2}$"), neg_dates(dates), dates) # negative 2 years only dates
  dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{3}$"), neg_dates(dates), dates) # negative 3 years only dates
  dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}$"), neg_dates(dates), dates) # negative year only dates
}

treat_future_dates <- function(dates){
  fut_dates <- function(dates) {
    ifelse(dates > Sys.Date() + lubridate::years(25), "9999-12-31", dates)
  }
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), fut_dates(dates), dates) # stadardises how future dates are reported
}

complete_dates <- function(dates) {
  
  dates <- as.character(dates)
  
  dates <- sapply(dates, function(d){
    if(is.na(d)) {
      d <- d
    } else if(stringr::str_detect(d, "^[:digit:]{4}$")){ # 4 digit year only
      d <- paste0(d, "-01-01")
    } else if(stringr::str_detect(d, "^[:digit:]{3}$")){ # 3 digit year only
      d <- paste0("0", d, "-01-01")
    } else if(stringr::str_detect(d, "^[:digit:]{2}$")){ # 2 digit year only
      d <- paste0("00", d, "-01-01")
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}$")){ # month only
      d <- paste0(d, "-01")
    } else d <- d
    d
  }) 
  unname(dates)
}

remove_range <- function(dates) {
  
  dates <- lapply(dates, function(d){
    
    if(is.na(d)) {
      d <- d
    } else if(stringr::str_detect(d, "^[:digit:]{4}:[:digit:]{4}$")){ # remove year range
      brackets <- stringr::str_split(d, ":")
      d <- paste0(brackets[[1]][1], "-01-01")
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}:[:digit:]{2}$")){ # remove month range
      brackets <- stringr::str_split(d, ":")
      d <- paste0(brackets[[1]][1], "-01")
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{2}$")){ # remove day range
      brackets <- stringr::str_split(d, ":")
      d <- paste0(brackets[[1]][1])
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")){ # remove correct range format
      brackets <- stringr::str_split(d, ":")
      d <- paste0(brackets[[1]][1])
    } else d <- d
    d
  })
  unlist(dates)
}


treat_incomplete_dates <- function(ranged_dates){
  
  ranged_dates <- sapply(ranged_dates, function(d){
    if(is.na(d)) {
      d <- d
    } else if(stringr::str_detect(d, "^[:digit:]{4}$")){ # 4 digit year only
      d <- paste0(d, "-01-01:", d, "-12-31")
    } else if(stringr::str_detect(d, "^[:digit:]{3}$")){ # 3 digit year only
      d <- paste0("0", d, "-01-01:", "0", d, "-12-31")
    } else if(stringr::str_detect(d, "^[:digit:]{2}$")){ # 2 digit year only
      d <- paste0("00", d, "-01-01:","00", d, "-12-31")
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}$")){ # month only
      start <- paste0(d, "-01")
      d <- paste0(start, ":", d, "-", days_in_month(month(ymd(start))))
    } else d <- d
    d
  }) 
  unname(ranged_dates)
}

treat_range_dates <- function(ranged_dates){
  
  ranged_dates <- lapply(ranged_dates, function(d){
    
    if(is.na(d)) {
      d <- d
    } else if (stringr::str_detect(d, "^[:digit:]{4}:[:digit:]{4}$")){ # year range
      brackets <- stringr::str_split(d, ":")
      start <- paste0(brackets[[1]][1], "-01-01")
      finish <- paste0(brackets[[1]][2], "-12-31")
      d <- paste0(start, ":", finish)
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}:[:digit:]{2}$")){ # month range
      brackets <- stringr::str_split(d, ":")
      start <- paste0(brackets[[1]][1], "-01")
      finish <- paste(stringr::str_split(start, "-")[[1]][1],
                      brackets[[1]][2],
                      lubridate::days_in_month(as.numeric(brackets[[1]][2])),
                      sep = "-")
      d <- paste0(start, ":", finish)
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{2}$")){ # day range
      brackets <- stringr::str_split(d, ":")
      start <- brackets[[1]][1]
      finish <- paste(stringr::str_split(start, "-")[[1]][1],
                      stringr::str_split(start, "-")[[1]][2],
                      brackets[[1]][2],
                      sep = "-")
      d <- paste0(start, ":", finish)
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")){ # correct range format
      d <- d
    } else d <- d
    d
  })
  unlist(ranged_dates)
}

treat_future_ranges <- function(ranged_dates){
  fut_ranges <- function(ranged_dates) {
    ifelse(ranged_dates > Sys.Date() + lubridate::years(25), paste0(ranged_dates, ":9999-12-31"), ranged_dates)
  }
  ranged_dates <- ifelse(stringr::str_detect(ranged_dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), fut_ranges(ranged_dates), ranged_dates) # stadardises how future dates are reported
}

#' Resetting century of future events
#'
#' Resets the century of (unlikely) future events
#' @param dates First variable to be used, required.
#' Can be a character or date variable.
#' @param sep Year from which to make a cut-off and return the last century.
#' By default this is the system date, but can be specified.
#' @return A vector the same length as \code{dates}
#' @details This function will use the system date as the cut-off
#' for identifying past or future events.
#' It is geared towards numeric dates (i.e. "12") not named dates (i.e. "Dec"),
#' though these dates should be correctly parsed regardless of order or separator.
#' It will return any datestamps of "9999-12-31",
#' which may be interpreted as unknown/future date, as is.
#' Please note that different types of separators in the same vector,
#' for example c("2004-12-12","12/12/2004", "12.12.2004"),
#' may confuse the algorithm.
#' In other words, this function is built for internally consistent codes.
#' @examples
#' \dontrun{
#' recent(head(tfd_agree$Sign))
#' }
#' @export
recent <- function(dates, sep = NULL){
  .Deprecated("qData::standardise_dates")
}

#' Resorting and filtering dates
#'
#' Resorting and filtering dates
#' @param data a dataframe
#' @param vars a character vector identifying columns in the dataframe to sequence
#' @param unity a string identifying how multiple entries may be glued together.
#' By default, tidyr::unite() glues using the underscore "_".
#' @return a dataframe/columns
#' @import lubridate
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' data <- data.frame(Sign = c("2000-01-01", "2001-01-01", "2001-01-01_2000-01-01", "2000-01-01", NA),
#'                    Force = c("2001-01-01", "2000-01-01", "2001-01-01", NA, "2001-01-01"))
#' resequence(data, c("Sign","Force"))
#' }
#' @export
resequence <- function(data, vars, unity = "_"){
  
  len <- length(vars)
  
  out <- apply(data[,vars], 1, function(x) {
    dates <- sort(unlist(strsplit(unique(na.omit(x)),unity)))
    
    if (length(dates) < len){
      dates <- interleave(dates, which(is.na(x)))
    }
    
    if (length(dates) > len){
      if (sum((!grepl("-01-01", dates))*1)>=len) dates <- dates[!grepl("-01-01",dates)]
      if (sum((!grepl("9999", dates))*1)>=len) dates <- dates[!grepl("9999",dates)]
      
      dmax <- max(lubridate::as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))])))
      dmax <- which(lubridate::as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))]))==as.duration(dmax))
      dates <- dates[c(1,dmax+1)]
    }
    
    dates
  })
  
  t(out)
  
}

