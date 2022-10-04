####################################################
# BAN432, Coding in class 7. Sept. 2022, Lecture 3
####################################################
#install.packages("jsonlite")
install.packages("fromJSON")
require(jsonlite)
require(dplyr)
# 1. Wikipedia
# Define the parts of the url
endpoint        <- "https://en.wikipedia.org/w/api.php"
type.of.action  <- "query"
page.title      <- "Enron%20scandal" # space has to be escaped with '%20'
property        <- "contributors" # or try: "extracts"
response.format <- "json"
# combine the elements of the URL
search.url <- paste0(endpoint,
                     "?action=",type.of.action,
                     "&titles=",page.title,
                     "&prop=",property,
                     "&format=",response.format)
# inspect
search.url
# Get a list of the users that contributed to the page
wiki.data <- fromJSON(search.url)
contributors <- wiki.data$query$pages$`11954274`$contributors
# Text - Download the text of the article and make a frequency list of the
#        words
property <- "extracts"
search.url <- paste0(endpoint,
                     "?action=",type.of.action,
                     "&titles=",page.title,
                     "&prop=",property,
                     "&format=",response.format)
wiki.data.text <- fromJSON(search.url)
# Workflow for the frequency list
# (1.1) read the html document
# (1.2) split the text into words
# (1.3) make the frequency list
# (1.4) sort it in decreasing order
require(xml2)
require(rvest)
require(tidytext)
freq <- wiki.data.text$query$pages$`11954274`$extract %>%
  read_html() %>%
  html_text(trim = T) %>%
  as_tibble() %>%
  unnest_tokens(input  = "value",      # input column in data frame
                output = "tokens",     # name of the new output column
                token  = "words") %>%  # unit of segmentation (character, words, 
  etc)
count(tokens, sort = T)
# >>>
# Alternative with base-R:
# freq <- wiki.data.text$query$pages$`11954274`$extract %>%  
# gsub("<.+?>", "",.) %>%
# strsplit(x=.,split = " ") %>%
# unlist() %>%
# table() %>%
# sort(decreasing = T)

# 2. EDGAR
# Documentation for the EDGAR API can be found here:
# https://www.sec.gov/edgar/sec-api-documentation
#
# Goal: retrieve all annual reports from Apple from the past 10 years. For this,
# we use the "submissions" endpoint of the EDGAR API which lists all submissions
# that were made by a company. The endpoint is available under this url:
# https://data.sec.gov/submissions/CIK##########.json
# CIK for Apple is 0000320193 (you have to look that up)
#
# WORKFLOW:
# (2.1) The object that is returned by the API is in json format, so we use
# "fromJSON()" from the jsonlite package to retrive the data. The URl is
# https://data.sec.gov/submissions/CIK0000320193.json
# (2.2) Extract the data we need from the response object
# (2.3) Create urls to the annual reports and download them
# (2.1)
raw <- fromJSON("https://data.sec.gov/submissions/CIK0000320193.json")
# (2.2)
# Have a look at the "raw" object. Under "filings" there is a list with the
# "recent" (= last 1000 filings) filings. We transform that into a tibble...
raw$filings$recent %>%
  as_tibble() %>%
  # ... and keep only those rows that contain annual reports
  filter(form=="10-K") -> df
# If you want to retrieve older filings, you find the file name of an additional
# json-file under "filings/files"
# (2.3)
# The urls to the 10-K filings are composed this way:
#  - a base url: https://www.sec.gov/Archives/edgar/data/
#  - the CIK: 0000320193 (Apple in this case)
#  - the accession nr. (without hyphens)
#  - the file name
# This is how the url for the first 10-K in df looks like:
# https://www.sec.gov/Archives/edgar/data/0000320193/000032019320000096/aapl-20200926.htmsource("~/Dropbox/Applied data text analysis for business and finance/version 2022/Lecture_03/mail_address.R")
for(i in 1:nrow(df)){
  base.url <- "https://www.sec.gov/Archives/edgar/data/"
  
  cik <- "0000320193"
  
  accession <- df$accessionNumber[i] %>%
    gsub("-", "",.)
  
  file.name <- df$primaryDocument[i]
  
  url <- paste0(base.url, cik, "/", accession, "/", file.name)
  
  download.file(url,
                destfile = paste0("downloads/", file.name),
                headers = c("User-Agent"=.your_mail_address)) # Add your email 
  address
}
list.files("downloads/")
# 4. TWITTER
# If you do not have an Twitter account and rtweet installed, you use 
# the provided query.results, which is a data frame that is returned
# by the "search_tweets()" function from the package "rtweet".
# Set some variables before running the script
require(rtweet)
query <- "#lufthansa" # Put your query string here.
number.of.tweets <- 500
# Max. number is 18000.
# This is the query. Only results from the past 7 days are returned
# (limit in the Twitter-API)
rtweet_user()
auth_setup_default()
# If you are not able to run rtweet, you find "query.results" in the Rdata file
# for today's lecture.
query.results <- search_tweets(q=query, 
                               n=number.of.tweets)
# Inspect the data frame "query.results". From the environment we see that
# it has 500 rows (observations) and 90 variables (columns). To get an
# impression, we subset the data frame: we select the first 5 rows and
# the first 7 columns.
query.results[1:5,1:7]
colnames(query.results)
# What language are the tweets written in?
query.results %>%
  count(lang, sort=T)
# The language of the tweets can be spezified:
# query <- "#lufthansa AND lang:en"
# We filter the data frame and keep only rows where lang=="en".
query.results %>%
  filter(lang=="en") -> query.results.en
# TASK 1: Which apps were used to post the tweets? The app is listed in the
#         column "source". Which are the most frequent ones?
query.results.en %>%
  count(source, sort = T) -> app
gsub("<.+?>", "",app$source) -> app$source

# TASK 2: What is the time range in which the tweets were posted
range(query.results.en$created_at)
weekdays(query.results.en$created_at[1]) 

# TASK 3: Get tweets that refer to the Apple stock
query <- "$AAPL"
query.results.aple <- search_tweets(q=query, 
                                    n = number.of.tweets)