
require(tidyverse)



# data
load("/Users/maximilianrohrer/Dropbox/course work/Text course NHH/Applied text data analysis for business and finance/version 2021/Lecture_08/firm_dataset.Rdata")


# Vector of 52 states
us.states <- datasets::state.name

# loop through all unique state names

# Count States
raw.data$state.count <- 0
for(us.state.iter in us.states){
  raw.data$state.count <- 
    raw.data$state.count + 
    grepl(pattern = us.state.iter, 
          x = section.1.business, 
          ignore.case = T)
}

# Apply approach
sapply(us.states, grepl, x = section.1.business) %>% 
  rowSums() -> raw.data$state.count

# histogram
hist(raw.data$state.count, breaks = 50)

# Average size per 5 equally sized bins
break.points <- quantile(raw.data$state.count, 
                          c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Aggregate using cut and aggregate
raw.data$state.count.quintile <- cut(raw.data$state.count, 
                                     breaks = break.points, 
                                     include.lowest = T) 
aggregate(raw.data$market.value, 
          by = list(raw.data$state.count.quintile), 
          median)

# Aggregate using tidy
raw.data %>% 
  group_by(state.count.quintile) %>% 
  summarise(MedianSize = median(market.value))

# how to count how often a given name appears (indiv assignment 3)
require("stringr")
str_count(string = section.1.business, 
          pattern = "Virginia")

# Remember
# -talk about group size
# -show individual assignment 3
