######################################################
#  BAN432, lecture 5, coding in class 16. Sept. 2022
######################################################
require(tidytext)
require(dplyr)
require(readr)
require(wordcloud)
require(stopwords)
setwd("")
load("Data/data_for_lecture_05.Rdata")
######################################################
# TASK 1:
# How are frequencies distributed in a corpus of natural language?
# (1) Load the Brown corpus into a tibble with just one column and one row.
#     We need this format for further processing.
raw.text <- tibble(text = read_file("Data/brown.txt"))
str(raw.text)
# (2) Split the text into smaller units. This is called "tokenization". We split
# into words using the "unnest_tokens()" function from the package "tidytext".
df <- unnest_tokens(tbl = raw.text,  # name of the data frame with the raw text
                    input = text,    # name of the input column in raw.text
                    output = word,   # name of the column that holds the words in 
                    the new data frame
                    to_lower = T)    # convert words to lower case
# Per default R only shows the 10 first rows of a tibble. If you want to see
# more rows, you have to use print() and specify the number of rows to print.
print(df, n=50)
# (3) Make a frequency table and sort it. We use count() from "dplyr" package.
brown.freq <- count(x = df,
                    word,
                    sort = T)
print(brown.freq, n=50)
# before plotting: how many types and tokens are in the Brown corpus?
# types:
nrow(brown.freq)
# tokens:
sum(brown.freq$n)
# Plot
plot(brown.freq$n,
     type = "l",            # "l" means: line chart
     xlab = "rank",         # x-axis label
     ylab = "frequencies",  # y-axis label
     main = "Brown corpus: frequencies") # main heading
# Problem: We don't see anything!
# Plot the frequencies with log
plot(brown.freq$n,
     type = "l",
     xlab = "rank",
     ylab = "frequencies",
     main = "Brown corpus: frequencies",
     log = "x")
##########################################
# TASK 2:
# Plot Wiki and Earning calls frequencies
load("Data/data_for_lecture_05.Rdata")
# Wiki
plot(wiki.freq$n,
     type = "l",
     xlab = "rank",
     ylab = "frequencies",
     main = "Wiki corpus: frequencies",
     log = "x")
# Calls:
plot(earning.calls.freq$n,
     type = "l",
     xlab = "rank",
     ylab = "frequencies",
     main = "Earning calls corpus: frequencies",
     log = "x")
lines(earning.calls.freq$n,
      type = "l",
      log = "x")
lines(wiki.freq$n,
      type = "l",
      log = "x")
# >>>>>>>>>>>>>>
#####################################
# TASK 3:
# Finding words with low frequencies
# (8) finding hapax legomena (= words that occur one time)
hapax <- brown.freq %>%
  filter(n==1)





# >>>>>>>>>>>>>>
####################################
# TASK 4:
# Make wordclouds together in class
####################################
# (1) Wordcloud with stopwords present. Consider only the 100 most frequent words.
# In case you get warnings or errors when creating the wordclouds, you can try
# to print them into a file
png("earning.calls.png", width=12, height=8, units="in", res=300)
wordcloud(words = earning.calls.freq$word[1:100],
          freq = earning.calls.freq$n[1:100])
dev.off()
# Now we remove the stopwords using a stopword list that is included
# in the package "stopwords".
# Filter out the stopwords with %in% operator. This keeps the rows in
# earning.calls.freq where "word" is *not* in stopword.list
earning.calls.freq %>%
  filter(!word %in% stopwords()) -> calls.no.stopwords
# In case you get warnings or errors when creating the wordclouds, you can try
# to print them into a file
png("earning.calls.no.sw.png", width=12, height=8, units="in", res=300)
wordcloud(words = calls.no.stopwords$word[1:100] ,
          freq = calls.no.stopwords$n[1:100])
dev.off()
# Sentence splitting
sentences <- unnest_sentences(raw.text,
                              sentences,
                              text)
sentences
# Check if the tokenizer recognizes abbreviations or if it splits at every full 
stop
x <- tibble(text = "This is one sentence. In this sentence the word dr. occurs.")
unnest_sentences(x,
                 sentences,
                 text)
# bi-grams (sequences of two consecutive words)
bigrams.df <- unnest_ngrams(raw.text, # the input data frame
                            bigrams,  # column name in the new data frame
                            text,     # type of input data
                            n=2)      # type of n-gram (2 = bi-gram)
bigrams.df
freq.bigrams <- count(bigrams.df, # the input data frame
                      bigrams,    # the column to count
                      sort =T)    # sort in decreasing order
freq.bigrams
# Extra:
require(tidyr)
# word clouds with bigrams
freq.bigrams %>%
  separate(bigrams,             # input column             
           into = c("w1","w2"), # separate the bi-grams into word 1 and words 2
           sep = " ",
           remove = F) %>%      # do not remove input column from output df
  # remove the rows where both w1 and w2 is not a stopword
  filter(!w1 %in% stopwords() & !w2 %in% stopwords()) -> x
wordcloud(word = x$bigrams[1:100], freq = x$n[1:100])