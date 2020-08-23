# rwithings

The goal of rwithings is to enable data extraction from the [Withings Health Mate API](http://developer.withings.com/oauth2/) into R. You will need to follow the instructions on the linked developer's guide to create a Withings account and a Withings developer app.

This package is still quite young, so there could very well be some remaining issues.

## Installation

You can install the development version of rwithings with the following:

``` r
devtools::install_github("mathesong/rwithings")
```

## Prerequisites

Before being able to use this package, and the Withings Health Mate API, you will need to create a Withings account and a Withings developer app. Instructions are provided in the [developer's guide](http://developer.withings.com/oauth2/).

Note: when setting up an app, the Callback URL should be set to http://localhost:1410/. This is not supposed to be allowed, but it seems to work, and I am also not sure how to make it work using another address. If anyone has any input on this, I'd really appreciate it.


## Examples

### API Functions

The functions of the package basically correspond to the same commands listed in the [developer's guide](http://developer.withings.com/oauth2/), with similar input arguments. Below are some examples.

``` r
client_id <- 12345
client_secret <- "hunter2"

token <- withings_auth(client_id, client_secret)

getmeas(token, meastype = 1, category=1, "2018-01-01", "2018-10-18")
getsleep(token, "2018-10-16", "2018-10-18")
getsleepsummary(token, "2018-10-16", "2018-10-18")
```
In the case of entering in start and end dates, I've put in place conversions so that you can enter it as a character vector in YYYY-MM-DD format to make quick queries more easy.


### CSV Functions

The API used not to have functions for extracting heart rate data.  This data could only be obtained by following [these instructions](https://support.withings.com/hc/en-us/articles/201491377-Health-Mate-Online-Dashboard-Exporting-my-data) to obtain a csv file. The data arrive in a format which is a bit messy to work with, so I also included some functions for munging the raw data. Most of the raw data is structured according to start, duration and value columns, so for these csv files, the command to read them works as follows:

``` r
hr_data <- read_csv_startdurval('raw_tracker_hr.csv')
```

The API has been expanded to include HR data now as intraday activity, which can be extracted using the `getintradayactivity()` function.  But I've left the CSV munging command in place regardless as it's still useful for linking heart rate with exact times, or for processing large amounts of data at once.
