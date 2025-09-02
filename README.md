# rwithings

The goal of rwithings is to enable data extraction from the [Withings Health Mate API](https://developer.withings.com/) into R. You will need to follow the instructions on the linked developer's guide to create a Withings account and a Withings developer app.

**Note: This package has been updated (2024) to fix authentication issues with the current Withings API.**

## Installation

You can install the development version of rwithings with the following:

``` r
devtools::install_github("mathesong/rwithings")
```

## Prerequisites

Before being able to use this package, and the Withings Health Mate API, you will need to create a Withings account and a Withings developer app. Instructions are provided in the [developer's guide](https://developer.withings.com/).

Note: when setting up an app, the Callback URL should be set to http://localhost:1410/. This is not supposed to be allowed, but it does seems to work, and I haven't been able to make it work using another address. If anyone has any input on this, please do make an issue.


## Examples

### API Functions

The functions of the package basically correspond to the same commands listed in the [developer's guide](https://developer.withings.com/api-reference/), with similar input arguments. Below are some examples.

#### Authentication (Automatic with Fallback)

The package now uses an improved authentication system that tries automatic OAuth first, then falls back to manual authentication with better user experience:

``` r
client_id <- 12345
client_secret <- "hunter2"

# Recommended: Automatic authentication with smart fallback
token <- withings_auth(client_id, client_secret)

# Use the token to get your health data
sleep_data <- getsleepsummary(token, "2023-01-01", "2023-01-31")
measurements <- getmeas(token, meastype = 1, category = 1, "2023-01-01", "2023-01-31")
activity_data <- getactivity(token, "2023-01-01", "2023-01-31")
```

#### What happens during authentication:

1. **First**: Attempts automatic browser-based OAuth
2. **If that fails**: Automatically switches to manual mode with:
   - Clear instructions for getting the authorization code
   - Automatic code extraction from URLs (no more manual parsing!)
   - Proper Withings API nonce-based signing

#### Manual Authentication (If Needed)

If you prefer manual control or need to use a custom redirect URI:

``` r
# Manual authentication with automatic code extraction
token <- withings_auth_manual(client_id, client_secret)

# Or with custom redirect URI (must be configured in your Withings app)
token <- withings_auth_manual(client_id, client_secret, 
                             redirect_uri = "https://yoursite.com/callback")
```

In the case of entering in start and end dates, there are conversions in place so that you can enter it as a character vector in YYYY-MM-DD format to make quick queries more easy.


### CSV Functions

The API used not to have functions for extracting heart rate data.  This data could only be obtained by following [these instructions](https://support.withings.com/hc/en-us/articles/201491377-Health-Mate-Online-Dashboard-Exporting-my-data) to obtain a csv file. The data arrive in a format which is a bit messy to work with, so I also included some functions for munging the raw data. Most of the raw data is structured according to start, duration and value columns, so for these csv files, the command to read them works as follows:

``` r
hr_data <- read_csv_startdurval('raw_tracker_hr.csv')
```

The API has been expanded to include HR data now as intraday activity, which can be extracted using the `getintradayactivity()` function.  Tthe CSV munging command remains in place regardless as it's still useful for processing large amounts of data at once without running into API request limits.
