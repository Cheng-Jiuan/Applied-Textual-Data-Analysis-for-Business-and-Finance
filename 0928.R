#package and load data
load("~/Desktop/出國交換/NHH/文字分析/firm_dataset.Rdata")
require(tibble)
require(dplyr)

# u.s. state names
# 可能遇到的問題：不同的文字、縮寫、文字的寫法不同、拼法不同...問題

us.states <- datasets::state.name  #看一下他們的名字
us.states <- gsub("\\s", ".", us.states) #去除空格及其他詞彙變成.
us.states <- tolower(us.states) #將文本改為大小寫
section.1.business <- tolower(section.1.business)

# 目標 raw.data$state.count #目標要創建一個新的state.count

i <- 43
j <- 481
raw.data$state.count <- 0


#2 a bit most inffefficient approach 
system.time ( #告訴會用多少時間

# overall file（拆解所有公司）
for (j in 1:500){
  # over all state（拆解一間公司有多少次的州）
  for (i in seq_along(us.states)){
    # overwrite/backfile
    raw.data$state.count[j] <- raw.data$state.count[j] #拿前面的值 + 
      grepl(us.states[i],section.1.business[j]) 
  }
}

)


grepl(us.states[i],
      section.1.business[j]) #後來老師把j刪除了

# 3 efficient approach----
sapply(us.states,function(us.states){
  grepl(us.states.select,
        section.1.business)
}) %>%
  rowsum() -> raw.data$state.count


Q2. grouping
raw.data%>%
  mutate(stateCountQuintiles = )%>%
  group_by(stateCountQuintiles)%>%
  summarise(average.state.count )


#quantile 分成不同的層級(%)，並且可以知道目前的分佈為何


