TwitterAnalyticsDownloader
==========================

[Twitter Analytics](https://analytics.twitter.com) provides detailed data of a twitter account's tweets: number of impressions, engagement rate, number of retweets, link clicks, etc. However, the website restricts the maximum possible date range to a 91-day window.

This R script downloads the tweet activity data from Twitter Analytics for *any* given period. If the selected date range is longer than 91 days, the download is split into several downloads.

### Prerequisites

The only requirement is having the `RSelenium` package installed. You can install it by running:

    install.packages("RSelenium")

### Installing

To install the script you will need the `devtools`package:

    install.packages("devtools")
    library(devtools)

Then you can install the script directly from GitHub by running:

    install_github("pferrando/TwitterAnalyticsDownloader")

Example
-------

The main function is `twitter_analytics_downloader`, which will download one or more csv files containing tweets activity.

You will have to provide:

-   The phone, email or username of a Twitter account.
-   The password of the Twitter account.
-   The start date of the period to download in format "%Y-%m-%d".
-   The end date of the period to download in format "%Y-%m-%d".
-   The directory where the downloaded files will be saved. Current directory is set by default.

You can download the Twitter Analytics report/s using the following code:

    library(TwitterAnalyticsDownloader)

    twitter_analytics_downloader("your-user-name", "your-password", "2018-01-01", "2018-04-01")
