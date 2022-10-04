setwd("")
load("firm_dataset.Rdata")
#install.packages("quanteda") #好像無法載這個套件
require(quanteda)
require(dplyr)
#install.packages("tidytext")
require(tidytext)
#install.packages("wordcloud")
#install.packages("RColorBrewer")
require(wordcloud) 
#install.packages("stopwords")
require(stopwords)
# Step 1: Create a KWIC-table (Key Word In Context):
# For this, we use the function "kwic()" that is included in the package
# "quanteda". The function produces a kwic-object that can easily be converted
# to a tibble. But first, we tokenize the the business  descriptions in 
# "section.1.business" using the function "tokens()" in "quanteda".
toks <- tokens(section.1.business)
# Set document-level variables: For each business description,  register the
# industry the company belongs to. You find that information in  the raw.data
# data frame.
docvars(x = toks, field = "industry") <-  raw.data$industry.fama.french.49 #創造一個變數field = "industry"
docvars(toks)
# This returns a "tokens" object, basically a list with 500 vectors containing
# the tokens of each business description. The toks object is, inturn, input
# for the kwic() function.
kw.env <- kwic(x = toks,                # input object
                 pattern = "environment.*",          # search pattern #.*+任何字母（都會被抓出來）
                 valuetype = "regex",        # what is the pattern, e.g. a regex
               window =  "4",          # how many words to the left and the right are displayed
               case_insensitive =  T # is the search case sensitive or not
)
# For a later analysis, we want to know where in the text the search term appears.
# The approach is to assign a number in the range between 0 and 100, where
# 0 is the first word in a text, and 100 the last word. To calculate this
# position index, we first need to find the total number of words (tokens) in
# each document. The function "ntoken()" does this.
nt <- ntoken(section.1.business)
head(nt)
names(nt)
# Make a tibble where one column is called "docname" (the same as in kw.env) ... #創造兩個變數
nt.df <- tibble(docname= names(nt), #docname
                ntoken = nt) #幾個
# ... and join it with kw.env
kw.env <- left_join(kw.env, 
                    nt.df) #把他們加在一起
# Now we can add a column "position.index" 
kw.env$position.index <- kw.env$to/(kw.env$ntoken*0.01) #創一個新的變數   #這裡看不是很懂？？？



# Step 2: Find the most frequent words for the right context and visualize them
# in a wordcloud. Approach:
#  (1) Convert the kwic object to a tibble (now it's a data frame)
#  (2) Select the column "post" and tokenize it with unnest_tokens()
#  (3) Remove stopwords
#  (4) Make a frequency list using count()
kw.env %>%
  as_tibble() %>%                                 # (1)把它變成tibble格式(tokens fountion 需要)
  select(post) %>%                                # (2)挑選出post
  unnest_tokens(input = post,
                output = tokens,
                to_lower = T) %>% #unnest_tokens 拆解單詞，把符号转换为小写字符，这是为了更方便与其它数据集比较或合并
  filter(!tokens %in% stopwords()) %>%            # (3) stopwords把不需要刪掉    ##%in%檢查他是否在這個vector內
  count(tokens, sort = T) -> right.context.freq   # (4) 統計post裡面的字、排序

# Make a wordcloud
filename <- "wordcloud1.png"
png(filename, 2000, 2000)
wordcloud(words = right.context.freq$tokens[1:100],
          freq  = right.context.freq$n[1:100],
          scale = c(20,3))
dev.off()

# Remove some of the words from 'right.context.freq' that obviously don't have
# anything to do with natural environment. （無關的資料）
custom.stopwords <- c("laws","regulations","safety", "health", 
                      "agency","compliance", "liabilities", "liability", "may", "including")

#做相同的事情
kw.env %>%
 as_tibble() %>%
 select(post) %>%
 unnest_tokens(input = post,
               output = tokens,
               to_lower = T) %>%
 filter(!tokens %in% stopwords()) %>%
 filter(!tokens %in% custom.stopwords) %>%
 count(tokens, sort = T) -> right.context.freq

# Make another wordcloud, now with custom stopwords removed （去除無關的資料後再次呈現）
filename <- "wordcloud2.png"
png(filename, 2000, 2000)
wordcloud(words = right.context.freq$tokens[1:100],
         freq  = right.context.freq$n[1:100],
         scale = c(20,3))
dev.off()

# In the second wordcloud, "protection" appears as the most frequent token.
# We create another kwic with the phrase "environmental protection" to see
# what the companies write about that topic.
kw.ep <- kwic(x = toks,
             pattern = phrase("environmental protection"),
             window = 4,
             case_insensitive = T)
# Mostly references to a government agency, not environment protection efforts
# of the companies.
# Next, let's see where in the documents the search pattern "environment.*"
# occurs. We have already created an index (0-100: 0=first word,100=last word)
# First, we make intervals. "Cut" divides the range of x into intervals and
# codes the values in x according to which interval they fall... 
intervals <- cut(x = kw.env$position.index, breaks = 10) # try 100 breaks #嘗試切割
levels(intervals)
# ... and then we plot how many mentions of "environment.*" there are in 
# each interval:
plot(intervals,
     xlab = "Intervals: 0 = first word, 100 = last word",
     ylab = "Number of occurences",
     main = "Occurences of 'environment.*' within the documents") #畫圖表
# How to interpret the plot?!
# Hypothesis: "environment" is part of a phrase that contains references to 
# law regulations. These phrases occur more often towards the end of a document.


#----------#
# Appendix #
#----------#
# What industries do the companies belong to that mention  "environment.*" in
# their business descriptions?
# The function tokens_select(), selects tokens that match a regex from a tokens
# object. 
tokens_select(toks,
              "environment.*",
              valuetype = "regex") %>%
  sapply(., function(x){
    length(x)
  }) %>%
  as_tibble() -> freq.env
# Register which industry the documents belong to
freq.env$industry <- raw.data$industry.fama.french.49
# Group the tibble by "industry" and calculate the mean freq. of "environment.*
# per industry
freq.env %>%
 group_by(industry) %>%
 summarize(sum(value)/n()) -> input.plot #組成群組、做加總
colnames(input.plot) <- c("industry", "average_freq")

require(ggplot2)
ggplot(data = input.plot) +
 geom_bar(aes(x = reorder(industry, -average_freq),
              y = average_freq),
          stat = "identity") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 labs(x = "Industry (Fama/French)",
      y = "Mean frequencies of 'environment.*'",
      title = "Aggregated mean frequencies of 'environment.*' for different industries",
      subtitle = "Data: Section 'Item 1' from 500 10-Ks")


