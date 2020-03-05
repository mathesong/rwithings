#' Get measures data
#'
#' @param token Your token obtained using `withings_auth()`
#' @param meastype Which measure number should be obtained (see API)
#' @param category Which category number should be obtained (see API)
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param offset Offset
#' @param lastupdate Last update (see API)
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getmeas(token, meastype = 1, category=1, "2018-01-01", "2018-10-18")
#' }
getmeas <- function(token, meastype, category, startdate=NULL, enddate=NULL,
                    offset=NULL, lastupdate=NULL, tz=Sys.timezone()) {

  if( class(startdate) == "character" ) { startdate = as.Date(startdate) }
  if( class(enddate) == "character" ) { enddate = as.Date(enddate) }
  if( class(lastupdate) == "character" ) { lastupdate = as.Date(lastupdate) }

  if( class(startdate) == "Date" ) { startdate = as.POSIXct(startdate, tz=tz) }
  if( class(enddate) == "Date" ) { enddate = as.POSIXct(enddate, tz=tz) }
  if( class(lastupdate) == "Date" ) { lastupdate = as.POSIXct(lastupdate, tz=tz) }

  if( sum(class(startdate) == "POSIXct")>=1 ) { startdate = as.numeric(startdate) }
  if( sum(class(enddate) == "POSIXct")>=1 ) { enddate = as.numeric(enddate) }
  if( sum(class(lastupdate) == "POSIXct")>=1 ) { lastupdate = as.numeric(lastupdate) }

  if(!is.null(startdate) & !is.null(enddate) & !is.null(lastupdate)) {
    stop("Enter both a startdate and enddate, or a lastupdate, or nothing")
  }

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate, or a lastupdate, or nothing")
  }

  req <- httr::GET("https://wbsapi.withings.net/measure?action=getmeas",
             query=list(access_token=token$credentials$access_token,
                        meastype = meastype,
                        category = category,
                        startdate = startdate,
                        enddate = enddate,
                        offset = offset,
                        lastupdate = lastupdate))
  httr::stop_for_status(req)
  out <- httr::content(req)

  out <- jsonlite::fromJSON(out)

  if(out$status==0) {

    out$body$updatetime = as.POSIXct(out$body$updatetime, tz=tz, origin="1970-01-01")
    out$body$measuregrps <- jsonlite::flatten(out$body$measuregrps)
    out$body$measuregrps$date <- as.POSIXct(out$body$measuregrps$date, tz=tz, origin="1970-01-01")

    measures <- do.call("rbind", out$body$measuregrps$measures)

    out$body$measuregrps <- dplyr::select(out$body$measuregrps, -"measures")

    out$body$measuregrps <- dplyr::bind_cols(out$body$measuregrps, measures)

  }

  return(out)

}




#' Get sleep data
#'
#' @param token Your token obtained using `withings_auth()`
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getsleep(token, "2018-10-16", "2018-10-18")
#' }
getsleep <- function(token, startdate=NULL, enddate=NULL, tz="") {

  if( class(startdate) == "character" ) { startdate = as.Date(startdate) }
  if( class(enddate) == "character" ) { enddate = as.Date(enddate) }

  if( class(startdate) == "Date" ) { startdate = as.POSIXct(startdate, tz=tz) }
  if( class(enddate) == "Date" ) { enddate = as.POSIXct(enddate, tz=tz) }

  if( sum(class(startdate) == "POSIXct")>=1 ) { startdate = as.numeric(startdate) }
  if( sum(class(enddate) == "POSIXct")>=1 ) { enddate = as.numeric(enddate) }

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/sleep?action=get",
             query=list(access_token=token$credentials$access_token,
                        startdate = startdate,
                        enddate = enddate))
  httr::stop_for_status(req)

  out <- httr::content(req)

  out <- jsonlite::fromJSON(out)

  if(out$status==0) {

    out$body$series <- jsonlite::flatten(out$body$series)

    out$body$series$startdate <- as.POSIXct(out$body$series$startdate, tz=tz, origin="1970-01-01")
    out$body$series$enddate <- as.POSIXct(out$body$series$enddate, tz=tz, origin="1970-01-01")
    out$body$series <- dplyr::arrange(out$body$series, startdate)

  }

  return(out)

}





#' Get sleep summary data
#'
#' @param token Your token obtained using `withings_auth()`
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getsleepsummary(token, "2018-10-16", "2018-10-18")
#' }
#'
getsleepsummary <- function(token, startdate=NULL, enddate=NULL, tz="") {

  if( class(startdate) == "character" ) { startdate = as.Date(startdate) }
  if( class(enddate) == "character" ) { enddate = as.Date(enddate) }

  if( sum(class(startdate) == "POSIXct")>=1 ) { startdate = as.Date(startdate) }
  if( sum(class(enddate) == "POSIXct")>=1 ) { enddate = as.Date(enddate) }

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/sleep?action=getsummary",
                   query=list(access_token=token$credentials$access_token,
                              startdateymd = startdate,
                              enddateymd = enddate))
  httr::stop_for_status(req)
  out <- httr::content(req)

  out <- jsonlite::fromJSON(out)

  if(out$status==0) {



    out$body$series <- jsonlite::flatten(out$body$series)

    out$body$series$startdate <- as.POSIXct(out$body$series$startdate, tz=tz, origin="1970-01-01")
    out$body$series$enddate <- as.POSIXct(out$body$series$enddate, tz=tz, origin="1970-01-01")
    out$body$series <- dplyr::arrange(out$body$series, startdate)

  }

  return(out)

}


#' Get activity data
#'
#' @param token Your token obtained using `withings_auth()`
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param offset Offset
#' @param lastupdate Last update (see API)
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getactivity(token, "2018-10-16", "2018-10-18")
#' }
#'
getactivity <- function(token, startdate=NULL, enddate=NULL, offset=NULL, lastupdate=NULL, tz="") {

  if( class(startdate) == "character" ) { startdate = as.Date(startdate) }
  if( class(enddate) == "character" ) { enddate = as.Date(enddate) }
  if( class(lastupdate) == "character" ) { lastupdate = as.Date(lastupdate) }

  if( sum(class(startdate) == "POSIXct")>=1 ) { startdate = as.Date(startdate) }
  if( sum(class(enddate) == "POSIXct")>=1 ) { enddate = as.Date(enddate) }
  if( sum(class(lastupdate) == "POSIXct")>=1 ) { lastupdate = as.Date(lastupdate) }

  if(is.null(startdate) & is.null(enddate) & is.null(lastupdate)) {
    stop("Enter both a startdate and enddate, or a lastupdate")
  }

  if(!is.null(startdate) & !is.null(enddate) & !is.null(lastupdate)) {
    stop("Enter both a startdate and enddate, or a lastupdate")
  }

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate, or a lastupdate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/measure?action=getactivity",
             query=list(access_token=token$credentials$access_token,
                        startdateymd = startdate,
                        enddateymd = enddate,
                        offset = offset,
                        lastupdate = lastupdate))
  httr::stop_for_status(req)

  out <- httr::content(req)
  out <- jsonlite::fromJSON(out)

  if(out$status==0) {

    out$body$activities <- jsonlite::flatten(out$body$activities)

  }

  return(out)

}


#' Get intraday activity data
#'
#' @param token Your token obtained using `withings_auth()`
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getintradayactivity(token, "2018-10-16", "2018-10-18")
#' }
getintradayactivity <- function(token, startdate=NULL, enddate=NULL, tz="") {

  if( class(startdate) == "character" ) { startdate = as.Date(startdate) }
  if( class(enddate) == "character" ) { enddate = as.Date(enddate) }

  if( class(startdate) == "Date" ) { startdate = as.POSIXct(startdate, tz=tz) }
  if( class(enddate) == "Date" ) { enddate = as.POSIXct(enddate, tz=tz) }

  if( sum(class(startdate) == "POSIXct")>=1 ) { startdate = as.numeric(startdate) }
  if( sum(class(enddate) == "POSIXct")>=1 ) { enddate = as.numeric(enddate) }

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/measure?action=getintradayactivity",
             query=list(access_token=token$credentials$access_token,
                        startdate = startdate,
                        enddate = enddate))
  httr::stop_for_status(req)

  out <- httr::content(req)
  out <- jsonlite::fromJSON(out)

  if(out$status==0) {

    out$body$series <- lapply(out$body$series,
                              function(x) x[!sapply(x, is.null)])

    out$body$series <- dplyr::bind_rows( out$body$series, .id="id")

  }

  return(out)

}




#' Get workouts data
#'
#' @param token Your token obtained using `withings_auth()`
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param offset Offset
#' @param lastupdate Last update (see API)
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getworkouts(token, "2018-10-16", "2018-10-18")
#' }
getworkouts <- function(token, startdate=NULL, enddate=NULL, offset=NULL, lastupdate=NULL, tz="") {

  # measures <- getmeas(token, meastype = 1, category=1, "2018-01-01", "2018-10-18")

  if( class(startdate) == "character" ) { startdate = as.Date(startdate) }
  if( class(enddate) == "character" ) { enddate = as.Date(enddate) }
  if( class(lastupdate) == "character" ) { lastupdate = as.Date(lastupdate) }

  if( sum(class(startdate) == "POSIXct")>=1 ) { startdate = as.Date(startdate) }
  if( sum(class(enddate) == "POSIXct")>=1 ) { enddate = as.Date(enddate) }
  if( sum(class(lastupdate) == "POSIXct")>=1 ) { lastupdate = as.Date(lastupdate) }

  if(!is.null(startdate) & !is.null(enddate) & !is.null(lastupdate)) {
    stop("Enter both a startdate and enddate, or a lastupdate, or nothing")
  }

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate, or a lastupdate, or nothing")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/measure?action=getworkouts",
             query=list(access_token=token$credentials$access_token,
                        startdateymd = startdate,
                        enddateymd = enddate,
                        offset = offset,
                        lastupdate = lastupdate))
  httr::stop_for_status(req)

  out <- httr::content(req)
  out <- jsonlite::fromJSON(out)

  if(out$status==0 & length(out$body$series) > 0) {

    out$body$series <- jsonlite::flatten(out$body$series)

    out$body$series$startdate <- as.POSIXct(out$body$series$startdate, tz=tz, origin="1970-01-01")
    out$body$series$enddate <- as.POSIXct(out$body$series$enddate, tz=tz, origin="1970-01-01")
    out$body$series$data.device_startdate <- as.POSIXct(out$body$series$data.device_startdate, tz=tz, origin="1970-01-01")
    out$body$series$data.device_enddate <- as.POSIXct(out$body$series$data.device_enddate, tz=tz, origin="1970-01-01")

  }

  return(out)

}
