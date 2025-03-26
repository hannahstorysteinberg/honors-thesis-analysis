library(readr)
library(dplyr)
res <- read_csv(file = "results.csv",col_names = FALSE)
df <- res
head(df)
colnames(df) <- LETTERS[1:24] # like the google sheet
head(df)
list_of_names <- as.vector(df[which(df[,"J"] == "name"),"K"])[[1]]
list_of_names
length(list_of_names)
length(unique(list_of_names)) # 1 unique name
list_of_names[duplicated(list_of_names)]
# philip naef is dup, but it seems to be fine bc he got a different order of words/sentences


df <- filter(df, !grepl("^#",A)) # remove all rows that start with a hashtag which are all the comments
head(df)
df <- select(df,-c(B,C,D,E,G)) # remove unnessesary columns

# assign each result time number to the name as an id
times <- unique(df[,"A"])[[1]]
length(times)
length(list_of_names)
cbind(list_of_names,times)
