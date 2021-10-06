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
#' getmeas(token, meastype = 1, category=1, "2019-01-01", "2019-10-18")
#' }
getmeas <- function(token, meastype, category, startdate=NULL, enddate=NULL,
                    offset=NULL, lastupdate=NULL, tz="") {

  startdate <- convert_date(startdate, "numeric", tz)
  enddate <- convert_date(enddate, "numeric", tz)
  lastupdate <- convert_date(lastupdate, "numeric", tz)

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

  out <- httr::content(req, as = "text", encoding = "utf-8")

  out <- jsonlite::fromJSON(out)

  if(out$status==0) {

    out$body$updatetime = as.POSIXct(out$body$updatetime, tz=tz, origin="1970-01-01")

    if( length(out$body$measuregrps) > 1) {
      out$body$measuregrps <- jsonlite::flatten(out$body$measuregrps)
      out$body$measuregrps$date <- as.POSIXct(out$body$measuregrps$date, tz=tz, origin="1970-01-01")

      measures <- dplyr::bind_rows(out$body$measuregrps$measures)

      out$body$measuregrps <- dplyr::select(out$body$measuregrps, -"measures")

      out$body$measuregrps <- dplyr::bind_cols(out$body$measuregrps, measures)
    }
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

  startdate <- convert_date(startdate, "numeric", tz)
  enddate <- convert_date(enddate, "numeric", tz)

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/sleep?action=get",
             query=list(access_token=token$credentials$access_token,
                        startdate = startdate,
                        enddate = enddate))

  httr::stop_for_status(req)

  out <- httr::content(req, as = "text", encoding = "utf-8")

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

  startdate <- convert_date(startdate, "Date", tz)
  enddate <- convert_date(enddate, "Date", tz)

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/sleep?action=getsummary",
                   query=list(access_token=token$credentials$access_token,
                              startdateymd = startdate,
                              enddateymd = enddate))
  httr::stop_for_status(req)

  out <- httr::content(req, as = "text", encoding = "utf-8")

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

  startdate <- convert_date(startdate, "Date", tz)
  enddate <- convert_date(enddate, "Date", tz)
  lastupdate <- convert_date(lastupdate, "Date", tz)

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

  out <- httr::content(req, as = "text", encoding = "utf-8")

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
#' getintradayactivity(token, "2018-10-16", "2018-11-18")
#' }
getintradayactivity <- function(token, startdate=NULL, enddate=NULL, tz="") {

  startdate <- convert_date(startdate, "numeric", tz)
  enddate <- convert_date(enddate, "numeric", tz)

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET("https://wbsapi.withings.net/v2/measure?action=getintradayactivity",
             query=list(access_token=token$credentials$access_token,
                        startdate = startdate,
                        enddate = enddate))
  httr::stop_for_status(req)

  out <- httr::content(req, as = "text", encoding = "utf-8")

  out <- jsonlite::fromJSON(out)

  if(out$status==0) {

    out$body$series <- lapply(out$body$series,
                              function(x) x[!sapply(x, is.null)])

    out$body$series <- dplyr::bind_rows( out$body$series, .id="id")

    out$body$series$startdate <- as.POSIXct(as.numeric(out$body$series$id),
                                            tz="", origin="1970-01-01")

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
#' getworkouts(token, "2018-10-16", "2018-11-18")
#' }
getworkouts <- function(token, startdate=NULL, enddate=NULL, offset=NULL,
                        lastupdate=NULL, tz="") {

  startdate <- convert_date(startdate, "Date", tz)
  enddate <- convert_date(enddate, "Date", tz)
  lastupdate <- convert_date(lastupdate, "Date", tz)

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

  out <- httr::content(req, as = "text", encoding = "utf-8")

  out <- jsonlite::fromJSON(out)

  if(out$status==0 & length(out$body$series) > 0) {

    out$body$series <- jsonlite::flatten(out$body$series)

    out$body$series$startdate <- as.POSIXct(out$body$series$startdate,
                                            tz=tz, origin="1970-01-01")
    out$body$series$enddate <- as.POSIXct(out$body$series$enddate,
                                          tz=tz, origin="1970-01-01")

  }

  return(out)

}

#' Get list of ECG recordings and Afib classification in the chosen period of
#' time
#'
#' @param token Your token obtained using `withings_auth()`
#' @param startdate Start date in character, date or POSIXct format
#' @param enddate End date in character, date or POSIXct format
#' @param offset Offset
#' @param tz Time zone
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getheartlist(token, "2018-10-16", "2018-10-18")
#' }
#'
getheartlist<- function(token, startdate=NULL, enddate=NULL, offset=NULL, tz="") {

  startdate <- convert_date(startdate, "Date", tz)
  enddate <- convert_date(enddate, "Date", tz)

  if(is.null(startdate) != is.null(enddate)) {
    stop("Enter both a startdate and enddate")
  }

  req <- httr::GET(url = "https://wbsapi.withings.net/v2/heart",
                    query=list(access_token=token$credentials$access_token,
                        action="list",
                        startdateymd = startdate,
                        enddateymd = enddate,
                        offset = offset))

  httr::stop_for_status(req)

  out <- httr::content(req, as = "text", encoding = "utf-8")

  out <- jsonlite::fromJSON(out)

  if(out$status==0 & length(out$body$series) > 0) {
    out$body$series <- jsonlite::flatten(out$body$series)
    out$body$series$timestamp<- as.POSIXct(out$body$series$timestamp,
                                           tz=tz, origin="1970-01-01")
  }

  return(out)
}


#' Get the full data set of the ECG recordings in micro-volt (μV).
#'
#' @param token Your token obtained using `withings_auth()`
#' @param signalid ID of a signal. Use `getheartlist()` to get one.
#'
#' @return A list containing the status (status) and the data (body)
#' @export
#'
#' @examples
#' \dontrun{
#' getheart(token, 123456)
#' }
#'
getheart<- function(token, signalid) {

  req <- httr::GET(url = "https://wbsapi.withings.net/v2/heart",
                    query=list(access_token=token$credentials$access_token,
                        action="get",
                        signalid=signalid))

  httr::stop_for_status(req)
  out <- httr::content(req, as = "text", encoding = "utf-8")
  out <- jsonlite::fromJSON(out)
  return(out)
}
