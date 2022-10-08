#######################################################
#  BAN432, lecture 11, coding in class 7 October 2022 #
#######################################################
# INTRODUCTION TO SENTIMENT ANALYSIS
load(file = "data_for_lecture_11_2022.Rdata")
#install.packages("sentimentr")
require(sentimentr)
require(dplyr)
#install.packages("lexicon")
require(lexicon)
require(stringr)
#install.packages("reshape2")
require(reshape2)
require(ggplot2)
require(rvest)
# Exercise 1
# Inspect the sentiment word list
dict <- hash_sentiment_jockers_rinker
#dict <- lexicon::hash_sentiment_loughran_mcdonald
head(dict)
# Which words are found in the dictionary?
text <- "I really didn't like the food. It was awful."
text %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .) %>%
  strsplit(., split = " ") %>%
  unlist() -> tokens
dict %>%
  filter(x %in% tokens)
extract_sentiment_terms(text,polarity_dt = hash_sentiment_jockers_rinker)
# Get to know the sentiment() and sentiment_by() functions from package
# "sentimentr"
text.pos <- "I really liked the food. It was delicious."
text.neg <- "I really did not like the food. It was awful."
# sentiment(): calculates sentence-level sentiment
sentiment(text.pos)
sentiment(text.neg)
# sentiment_by(): calculates text-level sentiment
sentiment_by(text.pos)
sentiment_by(text.neg)
#>>>>
# Exercise 2: Twitter data
# Stock prices and sentiment 
# Sentiment analysis based on daily mean sentiment scores
# 1: Get relevant tweets
#    There are many lists on the internet containing the X most important
#    Twitter users with a finance or investment profile to follow. We choose
#    3 sources to get relevant Twitter accounts:
# (1) Scrape Forbes website:
url <- "https://www.forbes.com/sites/alapshah/2017/11/16/the-100-best-twitter-accounts-for-finance/"
read_html(url) %>%
  html_table(header = T) %>%
  .[[1]] %>%
  pull('Twitter Handle')-> user.names.forbes # 100 user names
# (2) Marketwatch: Members of the list "Finance Twitter"
#     https://www.marketwatch.com/story/finance-twitter-the-50-most-important-people-for-investors-to-follow-2018-12-13
#     We use the function lists_members() from the package rtweet. You find the
#     list in your environment when you load today's data file.
#     NOTE: You can not run this code unless you have a Twitter account and rtweet
#     is configured. Find instructions on how to set up rtweet in the section "Usage": 
#     https://cran.r-project.org/web/packages/rtweet/readme/README.html
#install.packages("rtweet")  
require(rtweet)
list.members <- lists_members(slug = "finance-twitter", #命名
                              owner_user = "MarketWatch") #所有者
# user.names.marketwatch is included in today's data file
list.members %>%
  pull(screen_name) -> user.names.marketwatch
# (3) Investopedia
url <- "https://www.investopedia.com/financial-edge/0712/10-twitter-feeds-investors-should-follow.aspx"
read_html(url) %>%
  html_nodes("h2") %>%
  html_text2() %>%
  gsub(".+@(.+)", "\\1",.) %>%
  .[2:11] -> user.names.investopedia
# Concatenate the usernames into one vector and unique
user.names <- c(user.names.forbes,
                user.names.marketwatch,
                user.names.investopedia) %>%
  unique()
rm(user.names.forbes,user.names.marketwatch,user.names.investopedia)
# The following line works only if you are able to run rtweet.
# active.user.names is included in today's data file.
# Check if the users are active
lookup_users(user.names) %>% #取得資料 
  pull(screen_name) -> active.user.names #pull():取得向量
user.names[!user.names %in% active.user.names]
#--- DO NOT RUN THIS PART IN CLASS, YOU CAN TRY  IT AT HOME --- #
# The Twitter API has limits of how many requests an application can send in a 
# given time frame. We choose to split the vector with user names into 2 vectors
# with 120 names as a maximum. Each of the new vectors is processed individually
# with a pause of 15 min in between. This is done to bypass rate limits.
# max <- 120
# idx <- seq_along(active.user.names)
# user.names.list <- split(active.user.names, ceiling(idx/max))
# 
# tl1 <- get_timelines(user.names.list[[1]], n = 200)
# 
# # wait for 15 min to reset the rate limit
# tl2 <- get_timelines(user.names.list[[2]], n = 200)
# 
# tmls.all <- rbind(tl1,tl2)
# Reduce data to only contain tweets from 2022. 
tmls.all %>%
  filter(created_at >= "2022-01-01") -> tweets.2022
# Add the sentiment statistics for each tweet to tweets.2022
tweets.2022$full_text %>%
  get_sentences() %>%
  sentiment_by(polarity_dt = hash_sentiment_loughran_mcdonald) %>%
  .$ave_sentiment -> tweets.2022$sentimentLM
# Add a column "Date" that will be used to calculate daily mean sentiment scores. #分數
tweets.2022$Date <- as.Date(tweets.2022$created_at)
# TSLA
# Let's have a look at the sentiment scores for TSLA in 2022.
tweets.2022 %>%
  filter(grepl("(TSLA)|(Tesla)", full_text, ignore.case = T)) -> tsla
# Calculate the daily mean sentiment for TSLA: group by date and
# then summarise.
tsla %>%
  group_by(Date) %>% #用時間組成群組
  summarise(mean.LM = mean(sentimentLM)) -> tsla.daily.sentiment #算個平均值
# Next, we want to plot the mean daily sentiment scores and the closing stock
# prices in one plot to the if there is a relation between those variables.
# (1) Join the two data frames tsla.daily.sentiment and tsla.stock.prices
# (2) Select the relevant columns (Date, mean.LM, Close)
# (3) Prepare the data frame for plotting by "melting" it by the variable "date"
# The melt function is explained here: https://rpubs.com/guester/192565
tsla.data <- full_join(x = tsla.daily.sentiment,
                       y = stock.prices$tsla) %>%
  select(Date, mean.LM, Close) %>%
  melt(., id.var = "Date")
# Plot
ggplot() +
  geom_line(data = tsla.data,
            aes(x = Date,
                y = value,
                colour = variable)) +
  facet_grid(variable ~ ., scales = "free_y") + 
  labs(color='Variables') +
  ggtitle("TSLA: Daily sentiment and stock price")
# AAPL
# Do the same with Apple tweets and stock prices
tweets.2022 %>%
  filter(grepl("(AAPL)|Apple", full_text, ignore.case = T)) -> aapl
# Calculate the daily mean sentiment for AAPL: group by date and
# then summarise.
aapl %>%
  group_by(Date) %>%
  summarise(mean.LM = mean(sentimentLM)) -> aapl.daily.sentiment
# Join sentiment and stock price df
aapl.data <- full_join(x = aapl.daily.sentiment,
                       y = stock.prices$aapl) %>%
  select(Date, mean.LM, Close) %>%
  melt(., id.var = "Date")
# Plot
ggplot() +
  geom_line(data = aapl.data,
            aes(x = Date,
                y = value,
                colour = variable)) +
  facet_grid(variable ~ ., scales = "free_y") + 
  labs(color='Variables') +
  ggtitle("AAPL: Daily sentiment and stock price")

