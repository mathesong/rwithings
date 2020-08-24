#' Fix CSV files with start, duration and value
#'
#' @param start String start value
#' @param dur String duration value
#' @param value String value value
#'
#' @return A tibble with each observation on a new line
#' @export
#'
#' @examples
#' \dontrun{
#' startdurval(Sys.time(),
#'             "29,60,60,60", "23,9,9,35" )
#' }
startdurval <- function(start, dur, value) {

  durations <- as.numeric(strsplit(dur, ",")[[1]])
  values <- as.numeric(strsplit(value, ",")[[1]])
  starts <- c(start, start + cumsum(durations[-length(durations)]))

  tibble::tibble(start = starts,
                 duration=durations,
                 value=values)
}

#' Read CSV including start duration value
#'
#' This function reads the raw input data files which can be downloaded by
#' requesting one's data from the Withings Health Mate site.
#'
#' @param filename CSV filename
#' @param tz Time zone
#'
#' @return A tibble including the munged values in tidy format
#' @export
#'
#' @examples
#' \dontrun{
#' read_csv_startdurval('raw_tracker_hr.csv')
#' }
#'
read_csv_startdurval <- function(filename, tz=Sys.timezone()) {

  dat <- readr::read_csv(filename, col_types = "ccc") %>%
    dplyr::mutate(
      duration = stringr::str_replace_all(duration, "\\[|\\]", ""),
      value = stringr::str_replace_all(value, "\\[|\\]", ""),
      start = lubridate::as_datetime(start, tz=tz)) %>%
    dplyr::group_by(start) %>%
    dplyr::mutate(minidat = purrr::pmap(
      .l = list(start, duration, value),
      ~startdurval(start, duration, value))) %>%
    dplyr::ungroup() %>%
    dplyr::select(minidat) %>%
    tidyr::unnest() %>%
    dplyr::mutate(date = as.Date(start),
           end = start + duration,
           mid = start + duration/2)

  return(dat)

}


convert_date <- function(date, output_type = c("numeric", "Date"),
                         tz="") {

  output_type <- match.arg(output_type, c("numeric", "Date"))

  # Always convert from character to date
  if( any(class(date) == "character" )) { date = as.Date(date) }

  if( output_type == "numeric" ) {

    # If to numeric, date --> POSIXct --> numeric

    if( any(class(date) == "Date" )) { date = as.POSIXct(date, tz=tz,
                                                         origin="1970-01-01") }
    if( any(class(date) == "POSIXct")) { date = as.numeric(date) }

  } else if( output_type == "Date" ) {

    # If to date, POSIXct --> Date

    if( any(class(date) == "POSIXct")) { date = as.Date(date) }

  }

  return(date)
}
