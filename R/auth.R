#' Extract authorization code from OAuth redirect URL
#'
#' @param url The full redirect URL containing the authorization code
#' @return The authorization code or NULL if not found
#' @keywords internal
extract_code_from_url <- function(url) {
  # Try to extract code parameter from URL
  if(grepl("code=", url)) {
    # Use regex to extract the code parameter
    matches <- regmatches(url, regexec("code=([^&]+)", url))
    if(length(matches[[1]]) >= 2) {
      return(matches[[1]][2])
    }
  }
  return(NULL)
}

#' Authenticate with the Withings API (Automatic with Manual Fallback)
#'
#' This function attempts automatic OAuth2.0 authentication and falls back to 
#' manual authentication with proper Withings nonce-based signing if needed.
#' This is the recommended authentication method.
#'
#' @param client_id Your client ID from Withings Developer Dashboard
#' @param client_secret Your Client Secret from Withings Developer Dashboard
#' @param scope Which information you would like to access
#' @param redirect_uri Your redirect URI (must match what's configured in your Withings app)
#' @param authorize_url The authorize URL (you shouldn't need to touch this)
#' @param access_url The access URL (you shouldn't need to touch this)
#'
#' @return A token object that can be used with other rwithings functions
#' @export
#'
#' @examples
#' \dontrun{
#' client_id <- 12345
#' client_secret <- "hunter2"
#' 
#' # Automatic authentication (tries automatic, falls back to manual if needed)
#' token <- withings_auth(client_id, client_secret)
#' 
#' # Use the token to get data
#' sleep_data <- getsleepsummary(token, "2023-01-01", "2023-01-31")
#' }
withings_auth <- function(client_id, client_secret,
                          scope = c("user.metrics", "user.info", "user.activity"),
                          redirect_uri = "http://localhost:1410/",
                          authorize_url = "https://account.withings.com/oauth2_user/authorize2",
                          access_url = "https://wbsapi.withings.net/v2/oauth2") {

  if(length(scope) > 1) {
    scope <- paste(scope, collapse=",")
  }

  withings <- httr::oauth_endpoint(request = NULL,
                             authorize = authorize_url,
                             access = access_url)

  withingsapp <- httr::oauth_app("withings",
                           key = client_id,
                           secret = client_secret,
                           redirect_uri = redirect_uri)

  token <- httr::oauth2.0_token(endpoint=withings,
                 app = withingsapp,
                 scope = scope,
                 use_oob = FALSE,
                 cache = FALSE)

  # Check if the token has proper credentials
  if(!is.null(token$credentials) && 
     (is.null(token$credentials$access_token) || 
      (!is.null(token$credentials$status) && token$credentials$status != 0))) {
    
    message("\nStandard OAuth flow failed. Withings requires special authentication.")
    message("Falling back to manual authentication with proper nonce handling...")
    
    # Fall back to manual authentication
    return(withings_auth_manual(client_id, client_secret, scope, redirect_uri))
  }

  return(token)

}

#' Manual Withings authentication with improved UX
#'
#' This function provides manual OAuth2 authentication with proper Withings 
#' nonce-based signing and automatic code extraction from URLs. Use this if you 
#' prefer manual control over the authentication process, or if withings_auth() 
#' automatic fallback doesn't work for your setup.
#'
#' @param client_id Your client ID from Withings Developer Dashboard
#' @param client_secret Your Client Secret from Withings Developer Dashboard
#' @param scope Which information you would like to access
#' @param redirect_uri Your redirect URI (default: localhost with automatic code extraction)
#'
#' @return A token object that can be used with other rwithings functions
#' @export
#'
#' @details
#' This function will:
#' 1. Display an authorization URL for you to visit
#' 2. After authorization, ask you to paste the redirect URL  
#' 3. Automatically extract the authorization code from the URL
#' 4. Handle the proper Withings OAuth flow with nonce generation and signing
#' 5. Return a working token object
#'
#' @examples
#' \dontrun{
#' client_id <- 12345
#' client_secret <- "hunter2"
#' 
#' # Manual authentication with automatic code extraction
#' token <- withings_auth_manual(client_id, client_secret)
#' 
#' # Use with custom redirect URI
#' token <- withings_auth_manual(client_id, client_secret, 
#'                              redirect_uri = "https://yoursite.com/callback")
#' }
withings_auth_manual <- function(client_id, client_secret,
                                 scope = c("user.metrics", "user.info", "user.activity"),
                                 redirect_uri = NULL) {
  
  # If no redirect URI provided, use localhost with automatic code extraction
  if(is.null(redirect_uri)) {
    message("Using localhost:1410 with automatic code extraction...")
    message("If you have a different redirect URI configured in your Withings Developer Dashboard,")
    message("you can specify it by calling: withings_auth_manual(client_id, client_secret, redirect_uri = 'your_uri')")
    
    redirect_uri <- "http://localhost:1410/"
  }
  
  if(length(scope) > 1) {
    scope <- paste(scope, collapse=",")
  }
  
  # Step 1: Create authorization URL
  auth_url <- paste0(
    "https://account.withings.com/oauth2_user/authorize2",
    "?response_type=code",
    "&client_id=", client_id,
    "&state=", sample(1000:9999, 1),
    "&scope=", utils::URLencode(scope, reserved = TRUE),
    "&redirect_uri=", utils::URLencode(redirect_uri, reserved = TRUE)
  )
  
  message("Please visit the following URL to authorize the application:")
  message(auth_url)
  
  # Step 2: Get authorization code from user
  if(grepl("localhost|127\\.0\\.0\\.1", redirect_uri)) {
    message("\nAfter authorization, you will be redirected to localhost.")
    message("The page won't load, but that's OK! Just copy the ENTIRE URL from your browser.")
    message("Example: http://localhost:1410/?code=abc123&state=xyz")
    
    full_url <- readline(prompt = "Paste the full redirect URL here: ")
    
    # Extract code from URL
    auth_code <- extract_code_from_url(full_url)
    if(is.null(auth_code)) {
      message("Could not extract code from URL. Please enter it manually:")
      auth_code <- readline(prompt = "Enter just the authorization code: ")
    } else {
      message(paste("Extracted authorization code:", auth_code))
    }
    
  } else {
    message("\nAfter authorization, copy the authorization code from the redirect page.")
    auth_code <- readline(prompt = "Enter the authorization code: ")
  }
  
  # Step 3a: Get a nonce from Withings first
  timestamp <- as.integer(Sys.time())
  
  # Create signature for nonce request: action,client_id,timestamp
  nonce_sig_string <- paste("getnonce", client_id, timestamp, sep = ",")
  nonce_signature <- tolower(digest::hmac(client_secret, nonce_sig_string, algo = "sha256"))
  
  # Request nonce from Withings
  nonce_response <- httr::POST(
    url = "https://wbsapi.withings.net/v2/signature",
    body = list(
      action = "getnonce",
      client_id = client_id,
      timestamp = timestamp,
      signature = nonce_signature
    ),
    encode = "form"
  )
  
  httr::stop_for_status(nonce_response)
  nonce_content <- httr::content(nonce_response, as = "text", encoding = "utf-8")
  nonce_data <- jsonlite::fromJSON(nonce_content)
  
  if(nonce_data$status != 0) {
    stop("Failed to get nonce. Error: ", nonce_data$error)
  }
  
  nonce <- nonce_data$body$nonce
  
  # Step 3b: Exchange code for tokens using the nonce
  # Create signature for token request: action,client_id,nonce (sorted alphabetically)
  token_sig_string <- paste("requesttoken", client_id, nonce, sep = ",")
  token_signature <- tolower(digest::hmac(client_secret, token_sig_string, algo = "sha256"))
  
  message("Debug: Nonce=", nonce, ", Token Signature=", token_signature)
  
  token_response <- httr::POST(
    url = "https://wbsapi.withings.net/v2/oauth2",
    body = list(
      action = "requesttoken",
      grant_type = "authorization_code",
      client_id = client_id,
      client_secret = client_secret,
      code = auth_code,
      redirect_uri = redirect_uri,
      nonce = nonce,
      signature = token_signature
    ),
    encode = "form"
  )
  
  httr::stop_for_status(token_response)
  
  token_content <- httr::content(token_response, as = "text", encoding = "utf-8")
  token_data <- jsonlite::fromJSON(token_content)
  
  # Check Withings API status
  if(token_data$status != 0) {
    stop("Failed to obtain access token. Error: ", token_data$error)
  }
  
  # Withings API response format (has 'body' field)
  if(is.null(token_data$body$access_token)) {
    stop("Failed to obtain access token. Response: ", token_content)
  }
  
  # Create token object compatible with existing functions
  token <- list(
    credentials = list(
      access_token = token_data$body$access_token,
      refresh_token = token_data$body$refresh_token,
      expires_in = if(is.null(token_data$body$expires_in)) 10800 else token_data$body$expires_in
    ),
    refresh = function() {
      withings_refresh_token(client_id, client_secret, token_data$body$refresh_token)
    }
  )
  
  class(token) <- "withings_token"
  return(token)
}

#' Refresh Withings access token
#'
#' @param client_id Your client ID
#' @param client_secret Your Client Secret  
#' @param refresh_token Your refresh token
#'
#' @return A refreshed token object
#' @export
#'
#' @examples
#' \dontrun{
#' refreshed_token <- withings_refresh_token(client_id, client_secret, refresh_token)
#' }
withings_refresh_token <- function(client_id, client_secret, refresh_token) {
  
  # Step 1: Get a nonce from Withings first
  timestamp <- as.integer(Sys.time())
  
  # Create signature for nonce request: action,client_id,timestamp
  nonce_sig_string <- paste("getnonce", client_id, timestamp, sep = ",")
  nonce_signature <- tolower(digest::hmac(client_secret, nonce_sig_string, algo = "sha256"))
  
  # Request nonce from Withings
  nonce_response <- httr::POST(
    url = "https://wbsapi.withings.net/v2/signature",
    body = list(
      action = "getnonce",
      client_id = client_id,
      timestamp = timestamp,
      signature = nonce_signature
    ),
    encode = "form"
  )
  
  httr::stop_for_status(nonce_response)
  nonce_content <- httr::content(nonce_response, as = "text", encoding = "utf-8")
  nonce_data <- jsonlite::fromJSON(nonce_content)
  
  if(nonce_data$status != 0) {
    stop("Failed to get nonce for refresh. Error: ", nonce_data$error)
  }
  
  nonce <- nonce_data$body$nonce
  
  # Step 2: Create signed request for token refresh using the nonce
  # Create signature for refresh request: action,client_id,nonce
  refresh_sig_string <- paste("requesttoken", client_id, nonce, sep = ",")
  signature <- tolower(digest::hmac(client_secret, refresh_sig_string, algo = "sha256"))
  
  refresh_response <- httr::POST(
    url = "https://wbsapi.withings.net/v2/oauth2",
    body = list(
      action = "requesttoken",
      grant_type = "refresh_token",
      client_id = client_id,
      client_secret = client_secret,
      refresh_token = refresh_token,
      nonce = nonce,
      signature = signature
    ),
    encode = "form"
  )
  
  httr::stop_for_status(refresh_response)
  
  refresh_content <- httr::content(refresh_response, as = "text", encoding = "utf-8")
  refresh_data <- jsonlite::fromJSON(refresh_content)
  
  # Check Withings API status
  if(refresh_data$status != 0) {
    stop("Failed to refresh access token. Error: ", refresh_data$error)
  }
  
  # Withings API response format (has 'body' field)
  if(is.null(refresh_data$body$access_token)) {
    stop("Failed to refresh access token. Response: ", refresh_content)
  }
  
  # Create new token object
  token <- list(
    credentials = list(
      access_token = refresh_data$body$access_token,
      refresh_token = refresh_data$body$refresh_token,
      expires_in = if(is.null(refresh_data$body$expires_in)) 10800 else refresh_data$body$expires_in
    ),
    refresh = function() {
      withings_refresh_token(client_id, client_secret, refresh_data$body$refresh_token)
    }
  )
  
  class(token) <- "withings_token"
  return(token)
}

#' Refresh token (legacy function for backward compatibility)
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
  if(inherits(token, "withings_token") && !is.null(token$refresh)) {
    return(token$refresh())
  } else if(!is.null(token$refresh)) {
    return(token$refresh())
  } else {
    stop("Token object does not support refresh. Please re-authenticate.")
  }
}

