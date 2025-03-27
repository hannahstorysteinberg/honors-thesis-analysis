library(readr)
library(dplyr)
library(stringr)
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
key <- setNames(list_of_names,times)

df$A <- str_replace_all(df$A, key)
df <- filter(df,!(F == "instructions" | F == "secondpage")) # they all consented and have native english so can remove that part
df <- select(df,-X)
colnames(df) <- c("Name","Phase","TypeOption","Question","TypeChoice","Data","Timing","Num","Item","Group","isCrit","isLeg","isSent","isMain","sentNum","text","author","fileName")
head(df)

# SPLIT INTO DEMOGRAPHICS AND MAIN STUDY
df_main <- filter(df,!(Phase == "demographics"))
df_demo <- filter(df,Phase == "demographics")



# LEGIBILITY OF EACH PHOTO
#make negative if supposed to be ill? 
head(df_main)
leg <- data.frame(photo = unique(df$fileName), legibility = 0, number = 0) # go through and add all the legibility scores and one to number, then divide legibility by number
for (i in 1:nrow(df_main)) { # go through the rows
  if (df$Question == "leg") { # if the question is about legibility
    photoName <- (df$fileName)[i] # get the row photo name
    index <- which(leg$photo = photoName) # figure out which row in leg is that photo
    leg[i,2] <- leg[i,2] + (df$Data)[i] # add the legibility scores
    leg[i,3] <- leg[i,3] + 1 # add to the total number
  }
}
leg <- mutate(leg, avgLeg = )




# CONFIDENCE FOR EACH PHOTO



# LEGIBILITY OF EACH SENTENCE


# CONFIDENCE FOR EACH SENTENCE


# PERCENTAGE LITERAL WORD PER SENTENCE (while having context correct)



# PERCENTAGE NONLITERAL WORD PER SENTENCE (while having context correct)


# DEMOGRAPHICS
df_demo <- select(df_demo,c("Name","Phase","TypeOption","Question","TypeChoice","Data"))
head(df_demo)