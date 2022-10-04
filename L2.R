#getwd() 找到她在哪裡

#ls() #可以看到列表中的data是什麼

#require() #library() 載入套件

#class() #看他是什麼類型

#martix 矩正 

#cbind 綁兩種類似的綁在一起

================================
require(stringr)
require(dplyr)
require(readr)

grep("retail", my.strings, ignore.case = T) #第二個item是
grepl("retail", my.strings, ignore.case = T)

# grep and grepl can be used for subsetting vectors (and data frames)
my.strings[grep("retail", my.strings, ignore.case = T)]
my.strings[2] #寫法跟上面一樣

# Ex 2: Does the character vector my.strings contain the character sequence "bank"?
grep("bank", my.strings, ignore.case = T)

# Ex 3: Does the character vector my.strings contain the character sequence "unlike" at the beginning of a line?
grep("^unlike", my.strings, ignore.case = T) #^指的意思為字母為該字樣在開頭前面
grepl("^unlike", my.strings, ignore.case = T)
grep("its.$", my.strings, ignore.case = T) #$指的意思為字母為該字樣在結尾前面

# Ex 4: Does the character vector my.strings contain digits?
grepl("[[:digit:]]", my.strings) #找到數值

# Ex 5: Which digits appear in my.strings?
require(stringr)
str_extract_all(my.strings, "[[:digit:]]") #數值為何

# Working tasks:
# (1) Does the vector "my.strings" contain the character sequence "the"?
grep(" the ",my.strings) #yes #\\bthe\\b \\b等同於空格

# (2) Does the vector "my.strings" contain hyphenated words?
grep("-",my.strings)  #yes
grep("[[:alpha:]]"+-+"[[:alpha:]]",my.strings) #這個寫法比較好

# (3) Does the vector contain words that meet the following  criteria:
#  - starts with an "re" (upper or lower case)
#  - has "l" (as in "lemon") as the sixth character
grep("\\bre[[:alpha:]]{3}l", my.strings, ignore.case = T) #\\b避免前免有其他的文字影響
grep("re[[:alpha:]]{3}l", my.strings, ignore.case = T) #re開頭 #l結尾（第六個字母） #{3}中間會再多3個字母 #:alpha:中間有些文字
grep("re[[:alpha:]]{3}l.*", my.strings, ignore.case = T) #.*用於後面還有東西


# (4) Does the vector contain words that end with "cial"?
grep("cial\\b", my.strings) #yes->commercial
grep("[[:alpha:]]+cial\\b", my.strings) #也可以這樣寫

my.strings[1]
my.strings[2]
my.strings[3]

# Substitution with gsub and backreferencing
# General gsub synatx:
# gsub(pattern, repacement, string)
# Substitute "An" with "The" in my.strings[1]
gsub("An", "The", my.strings[1]) #第一句中用 "The "代替 "An"


# Remove the first part of the sentence in my.strings[2] until the comma.
gsub(".+, (.+)", "\\1", my.strings[2]) #\\1 replace -> .+, 
#.+意指為前面的東西, #(.+)要被留著
# "\\1" is the content of the 1st bracket
gsub(".+," ,".+", "\\1", my.strings[2]) #好像可以這樣改

# Example: In my.strings[2], extract all text until the word
# 
my.strings[2]
# Greedy matching: all text until the last occurrence
# of "banks" is matched.
gsub("(.+banks).+", "\\1",my.strings[2]) #最後一個bank後面都不要了
# Ungreedy matching (with quantifier "?"): text until
# the 1st occurrence of "banks is matched.
gsub("(.+?banks).+", "\\1",my.strings[2]) #?代表重複字母後都不要


# For-loops
day <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
# Alt. 1: "d" takes the values in day
for(d in day){
  print(paste("Today is", d))
}
# Alt. 2: "i" is an index
for(i in 1:length(day)){
  print(paste("Today is", day[i]))
}

# lapply/sapply
day <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
result <- sapply(X = day,
                 FUN = function(x){
                   paste("Today is", x) %>%
                     print()
                 }
)

# if-statement
weekday <- "Friday"
if(weekday == "Wednesday" | weekday == "Friday") {
  print("Today there is a lecture in BAN432.")
} else {
  print("There is no lecture in BAN432 today.")
}

# Create a function
myfunction.sum <- function(arg1, arg2){
  y<- arg1+arg2
  return(y)
}
x1 <- 1
x2 <- 2
myfunction.sum(x1, x2)

# Reading tsv-files and manipulating data frames
require(readr)
# Reading a tsv-file. If you are not sure how the data is 
separated, open
# in a text editor or the console of your operating system.
df <- read_tsv("tab_separated_values.tsv")
# Tasks:

# If you don't know the dplyr verbs `filter, select, pull, count`,
# you can use the help function `?dplyr::filter` etc.
# 1. Filter the data frame and keep only the words in doc2.
df %>%
  filter(doc_id == "doc2")

# 2. Select the columns token and upos.
df %>%
  select(upos,token)

# 3. Pull the column lemma.
df %>%
  poll(lemma) #找到每個字
  
# 4. Count how many times a token (word) appears
df %>%
  count(token, sort = T)
  

  
