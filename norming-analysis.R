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
df_main <- filter(df,Phase == "main")
df_demo <- filter(df,Phase == "demographics")
df_gender <- filter(df,Phase == "extra")


# LEGIBILITY OF EACH PHOTO
#make negative if supposed to be ill? 

head(df_main)
uniqueNames <- unique(df_main$fileName)
isLeg <- ifelse(str_detect(leg$photo,"leg"),"leg","ill")
leg <- data.frame(photo = uniqueNames, isLeg = isLeg,legibility = 0)
for (i in 1:nrow(leg)) { # go through the rows
  curFileName <- leg$photo[i]
  leg$legibility[i] <- mean(as.numeric(df_main$Data[which(df_main$fileName == curFileName & df_main$Question == "leg" & !is.na(df_main$Data))]))
}
leg$legibility <- round(leg$legibility,2)
leg
mean(filter(leg,isLeg == "leg")$legibility) # avg legibility of legible sentences
mean(filter(leg,isLeg == "ill")$legibility) # avg legibility of illegible sentences



# CONFIDENCE FOR EACH PHOTO
conf <- data.frame(photo=uniqueNames,isLeg = isLeg, confidence = 0)
for (i in 1:nrow(conf)) { # go through the rows
  curFileName <- conf$photo[i]
  conf$confidence[i] <- mean(as.numeric(df_main$Data[which(df_main$fileName == curFileName & df_main$Question == "conf" & !is.na(df_main$Data))]))
}
conf$confidence <- round(conf$confidence,2)
conf
mean(filter(conf,isLeg == "leg")$confidence) # avg confidence of legible sentences
mean(filter(conf,isLeg == "ill")$confidence) # avg confidence of illegible sentences



# LEGIBILITY OF EACH SENTENCE




# CONFIDENCE FOR EACH SENTENCE







# PERCENTAGE LITERAL WORD PER SENTENCE (while having context correct)



# PERCENTAGE NONLITERAL WORD PER SENTENCE (while having context correct)


# GENDER STUFF
head(df_gender)


# DEMOGRAPHICS
df_demo <- select(df_demo,c("Name","Phase","TypeOption","Question","TypeChoice","Data"))
head(df_demo)