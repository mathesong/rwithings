#' Authenticate with the Withings API
#'
#' This function performs the OAuth dance to get an OAuth2.0 token
#'
#' @param client_id Your client ID
#' @param client_secret Your Client Secret
#' @param scope Which information you would like to access
#' @param authorize_url The authorize URL (you shouldn't need to touch this)
#' @param access_url The access URL (you shouldn't need to touch this)
#'
#' @return An access token, or refreshing the access token.
#' @export
#'
#' @examples
#' \dontrun{
#' client_id <- 12345
#' client_secret <- "hunter2"
#' token <- withings_auth(client_id, client_secret)
#' }
withings_auth <- function(client_id, client_secret,
                          scope = c("user.metrics", "user.info", "user.activity"),
                          authorize_url = "https://account.withings.com/oauth2_user/authorize2",
                          access_url = "https://account.withings.com/oauth2/token") {

  if(length(scope) > 1) {
    scope <- paste(scope, collapse=",")
  }

  withings <- httr::oauth_endpoint(request = NULL,
                             authorize = authorize_url,
                             access = access_url)

  withingsapp <- httr::oauth_app("withings",
                           key = client_id,
                           secret = client_secret)

  token <- httr::oauth2.0_token(endpoint=withings,
                 app = withingsapp,
                 scope = scope)

  return(token)

}

#' Refresh token
#'
#' @param token Your token obtained using `withings_auth()`
#'
#' @return A refreshed access token
#' @export
#'
#' @examples
#' \dontrun{
#' client_id <- 12345
#' client_secret <- "hunter2"
#' token <- withings_auth(client_id, client_secret)
#'
#' token <- withings_authrefresh(token)
#' }
withings_authrefresh <- function(token) {
  token$refresh()
}

