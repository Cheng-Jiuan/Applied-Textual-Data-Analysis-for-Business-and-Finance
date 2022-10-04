######################################################
#  BAN432, lecture 6, coding in class 21 Sept. 2022
######################################################
require(tidytext)
require(readr)
require(dplyr)
require(udpipe)
require(wordcloud)
require(tm)
setwd("")
load("data/data_for_lecture_06.Rdata")
# Example from sildes:
# load a sample txt file in a vector
txt <- readLines("data/text_file_in_iso-latin-1.txt")
txt
# Convert to UTF-8 encoding
iconv(txt, from="LATIN1", to = "UTF-8")
# The package "readr" provides a function "guess_encoding()"
guess_encoding("data/text_file_in_iso-latin-1.txt")
iconv(txt, from = "LATIN1", to = "UTF-8")
###########################
# TASK 2:
# Term extraction.
# Find all types in the "Business description" corpus that do not occur in the
# British National Corpus (BNC):
# bnc.freq: frequency list of the BNC
# item1.freq: frequency list of a corpus of "Item 1" section from 10-Ks
###########################
head(bnc.freq)
head(item.1.freq)
# We merge the two data frames "bnc.freq" and "item.1.freq" that contain
# the frequency lists of the BNC corpus and a frequency list based on the
# business descriptions in the vector "section.1.business".
# Both data frames have a column "word" that we use for merging:
# We use "full_join()" from the dplyr package.
full_join(x = bnc.freq,
          y = item.1.freq,
          by = "word") -> merged.frequencies

head(merged.frequencies)  

# For encoding issues in some non-English versions of Windows
# we published a help page on Canvas. You find it in the Files section.
# Give more readable column names
colnames(merged.frequencies) <- c("word", "bnc", "item.1")
# Inspect the frequencies for the most frequent words in the BNC corpus
merged.frequencies %>%
  arrange(desc(bnc)) %>%
  head()
# (a)  One definition of a keyword could be, that it only appears in the 
# specialized 'item.1' corpus and not in the general language corpus:
# Filter out those types that are only present in "item.1"
# i.e. their frequency is 'NA' in "bnc". We use the function is.na()
# that returns TRUE if a value is NA.
merged.frequencies %>%
  filter(is.na(bnc)) -> term.candidates

head(term.candidates, n=20)
# A lot of noise (hapax legomena, words that appear only once). We add the 
condition,
# that frequency >= 100 in the "item.1" frequency list
merged.frequencies %>%
  filter(is.na(bnc) & item.1 >= 100) -> term.candidates
# Inspect term.candidates 
print(term.candidates, n=50)
# Some observations:
# (1) Why is "website" among the terms?
# (2) "frb" = Federal Reserve Board of Governors
# (3) "ferc" = Federal Energy Regulatory Commission
# (4) "cfpb" = Consumer Financial Protection Bureau
# >>>>>>>>>>>>>>>>>>>
#############################
# TASK 3:
# POS-tagging with package "udpipe". For the preprocessing, we need a tagger 
# that is included in the package "udpipe". The tagger needs a language model.
# After installing the package "udpipe", download the English language model
# (if not downloaded previously, you only have to download it once) ...
udpipe_download_model("english")
# ... and load it
tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
# Use the function "udpipe_annotate()" to create a data frame with one word
# per row, and (among others) do the part-of-speech tagging and lemmatization.
# We choose the first document in the "section.1.business" vector 
pos.test <- udpipe_annotate(tagger, x = section.1.business[1]) %>%
  as_tibble() %>%
  select(doc_id, token, lemma, xpos, upos)
# Task 1: Make word clouds based on the first 5 business descriptions.
#         Consider only nouns.
# POS-tag the first 5 business descriptions (this can take a minute).
# We don't run the next line in class, but you find 'sec.1' in today's data file.
sec.1 <- udpipe_annotate(tagger, x=section.1.business[1:5]) %>%
  as_tibble()
# Extract only nouns
sec.1 %>%
  filter(grepl("^N.+", xpos))-> nouns.df
head(nouns.df, n=50)
# Create wordclouds of the business descriptions
# We use a for-loop. For each document, create a wordcloud.
docs <- nouns.df$doc_id %>%
  unique()
for(d in docs){
  nouns.df %>%
    filter(doc_id == d) %>%
    count(lemma, sort = T) -> wordcloud.input
  
  png(filename = paste0(d,".png"),
      units = "cm",
      width = 20,
      height = 20,res = 300)
  wordcloud(word = wordcloud.input$lemma,
            freq = wordcloud.input$n)
  dev.off()
}
##################################
# TASK 4
# Create a Document Term Matrix
##################################
# (1) load the tm package:
require(tm)
# (2) read the text data
text.source <- VectorSource(section.1.business)
# (3) compile a corpus of texts
corpus <- Corpus(text.source)
corpus
# (4) Create a DTM
dtm <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = T,
                                         removeNumbers = T,
                                         stopwords = T,
                                         tolower = T))
dtm.matrix <- as.matrix(dtm)
dtm.matrix[1:5,1:5]