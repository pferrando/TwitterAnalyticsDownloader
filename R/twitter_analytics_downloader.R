#' @title Tweet Activity
#'
#' @description
#' Downloads the Tweet activity report from Twitter Analytics.
#'
#' @param username String specifying the phone, email or username of a Twitter account.
#' @param password String specifying the password of the Twitter account.
#' @param start_date String specifying the start date of the period to download in format "\%Y-\%m-\%d".
#' @param end_date String specifying the end date of the period to download in format "\%Y-\%m-\%d".
#' @param download_directory String specifying the directory where the downloaded files will be saved. Current directory is set by default.
#'
#' @details
#' This function downloads one or more csv files containing detailed data of the given
#' account's tweets: number of impressions, engagement rate, number of retweets,
#' link clicks, etc.
#'
#' Twitter Analytics restricts the maximum possible date range to a 91-day window.
#' If the selected date range is longer than 91 days, this function splits the download
#' into several downloads.
#'
#' @examples
#' \dontrun{
#' twitter_analytics_downloader("username", "password", "2018-01-01", "2018-04-01", "C:/Users/ABCD/Downloads")}
twitter_analytics_downloader <- function(username, password, start_date, end_date, download_directory=getwd()){
  ### Twitter Analytics report downloader using Selenium browser interaction
  ## Start a selenium server and a chrome browser
  open_browser(download_directory)
  ## Navigate to tweets tab of Twitter Analytics (require login)
  navigate_to_analytics_twitter(username)
  login(username, password)
  ## Download report/s (for a period longer than 91 days, it splits the
  ## download into several downloads)
  # Compute date ranges of 91 days
  date_ranges <- split_date_range(start_date, end_date)
  # For each batch, select dates and export data
  for(dates in date_ranges){
    # Select date range in calendar
    select_date_range(dates[1], dates[2])
    # Export data
    export_data()
  }
  ## Close browser
  remDr$close()
}

# ------------------------------------------------------------------
open_browser <- function(download_directory){
  ### Open a Chrome browser
  ## Choose Chrome options (e.g., download directory)
  eCaps <- list(
    chromeOptions =
      list(prefs = list(
        "profile.default_content_settings.popups" = 0L,
        "download.prompt_for_download" = FALSE,
        "download.default_directory" = download_directory
      )
      )
  )
  ## Initialize a Chrome browser with the chosen options
  rD <<- rsDriver(extraCapabilities = eCaps)
  remDr <<- rD$client
}

# ------------------------------------------------------------------
navigate_to_analytics_twitter <- function(username){
  ### Navigate to the tweets tab of Twitter Analytics
  remDr$navigate(paste0("https://analytics.twitter.com/user/",username,"/tweets"))
}

# ------------------------------------------------------------------
login <- function(username_or_email, password){
  ### Login on Twitter
  ## Populate username or email field
  webElem <- remDr$findElement("xpath","//input[@class='js-username-field email-input js-initial-focus']")
  webElem$sendKeysToElement(list(username_or_email))
  ## Populate password field
  webElem <- remDr$findElement("xpath","//input[@class='js-password-field']")
  webElem$sendKeysToElement(list(password))
  ## Click Log in button
  webElem <- remDr$findElement("xpath","//button[@class='submit EdgeButton EdgeButton--primary EdgeButtom--medium']")
  webElem$clickElement()
  Sys.sleep(5)
}

# ------------------------------------------------------------------
split_date_range <- function(start_date, end_date){
  ### Split date range into 91-day periods
  ## Compute number of batches required
  num_batches <<- ceiling(as.numeric(as.Date(end_date)-as.Date(start_date))/90)
  ## Compute start date and end date for each batch
  date_ranges <- lapply(1:num_batches,
                        function(i, start_date, end_date){
                          c(max(as.Date(start_date), as.Date(end_date) - 91*i + 1),
                            as.Date(end_date) - 91*(i-1))
                        }, start_date, end_date)
  return(date_ranges)
}

# ------------------------------------------------------------------
export_data <- function(){
  ### Download Twitter Analytics report for selected dates
  # Find Export Data button
  webElem <- remDr$findElement("xpath","//button[@class='btn btn-default ladda-button']")
  # Click button to download csv
  webElem$clickElement()
  Sys.sleep(10)
}
