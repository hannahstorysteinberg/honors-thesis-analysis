library(readr)
library(dplyr)
library(stringr)
res <- read_csv(file = "results.csv",col_names = FALSE) # kendra black was removed prior to this csv
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
isLeg <- ifelse(str_detect(uniqueNames,"leg"),"leg","ill")
leg_photo <- data.frame(photo = uniqueNames, isLeg = isLeg,legibility = 0)
for (i in 1:nrow(leg_photo)) { # go through the rows
  curFileName <- leg_photo$photo[i]
  leg_photo$legibility[i] <- mean(as.numeric(df_main$Data[which(df_main$fileName == curFileName & df_main$Question == "leg" & !is.na(df_main$Data))]))
}
leg_photo$legibility <- round(leg_photo$legibility,2)
leg_photo
mean(filter(leg_photo,isLeg == "leg")$legibility) # avg legibility of legible sentences
mean(filter(leg_photo,isLeg == "ill")$legibility) # avg legibility of illegible sentences



# CONFIDENCE FOR EACH PHOTO
conf_photo <- data.frame(photo=uniqueNames,isLeg = isLeg, confidence = 0)
for (i in 1:nrow(conf_photo)) { # go through the rows
  curFileName <- conf_photo$photo[i]
  conf_photo$confidence[i] <- mean(as.numeric(df_main$Data[which(df_main$fileName == curFileName & df_main$Question == "conf" & !is.na(df_main$Data))]))
}
conf_photo$confidence <- round(conf_photo$confidence,2)
conf_photo
mean(filter(conf_photo,isLeg == "leg")$confidence) # avg confidence of legible sentences
mean(filter(conf_photo,isLeg == "ill")$confidence) # avg confidence of illegible sentences



# LEGIBILITY OF EACH SENTENCE OVERALL
# using sentNum, but need to remove the first underscore
df_main$sentNum <- as.numeric(gsub("_","",df_main$sentNum)) # remove the first character with is an underscore
leg_sent <- data.frame(sentence=1:80,legibility=0)
for (i in 1:nrow(leg_sent)) {
  leg_sent$legibility[i] <- mean(as.numeric(df_main$Data[which(df_main$sentNum == i & df_main$Question == "leg" & !is.na(df_main$Data))]))
}
leg_sent$legibility <- round(leg_sent$legibility,2)
leg_sent
mean(filter(leg_sent,sentence > 20)$legibility) # avg legibility of fillers, which are all supposed to be legible


## CRITICAL SPLIT BY LEGIBILITY
leg_sent_crit <- data.frame(sentence=1:20,legibility_leg=0,legibility_ill=0)
for (i in 1:nrow(leg_sent_crit)) {
  leg_sent_crit$legibility_leg[i] <- mean(as.numeric(df_main$Data[which(df_main$sentNum == i & df_main$Question == "leg" & !is.na(df_main$Data) & df_main$isLeg == "leg")]))
  leg_sent_crit$legibility_ill[i] <- mean(as.numeric(df_main$Data[which(df_main$sentNum == i & df_main$Question == "leg" & !is.na(df_main$Data) & df_main$isLeg == "ill")]))
  
}
leg_sent_crit$legibility_leg <- round(leg_sent_crit$legibility_leg,2)
leg_sent_crit$legibility_ill <- round(leg_sent_crit$legibility_ill,2)
leg_sent_crit <- mutate(leg_sent_crit,difference = legibility_leg - legibility_ill)
leg_sent_crit



# CONFIDENCE FOR EACH SENTENCE OVERALL
conf_sent <- data.frame(sentence=1:80,confidence=0)
for (i in 1:nrow(conf_sent)) {
  conf_sent$confidence[i] <- mean(as.numeric(df_main$Data[which(df_main$sentNum == i & df_main$Question == "conf" & !is.na(df_main$Data))]))
}
conf_sent$confidence <- round(conf_sent$confidence,2)
conf_sent
mean(filter(conf_sent,sentence > 20)$confidence) # avg confidence of fillers, which are all supposed to be legible


## CRITICAL SPLIT BY LEGIBILITY
conf_sent_crit <- data.frame(sentence=1:20,confidence_leg=0,confidence_ill=0)
for (i in 1:nrow(conf_sent_crit)) {
  conf_sent_crit$confidence_leg[i] <- mean(as.numeric(df_main$Data[which(df_main$sentNum == i & df_main$Question == "conf" & !is.na(df_main$Data) & df_main$isLeg == "leg")]))
  conf_sent_crit$confidence_ill[i] <- mean(as.numeric(df_main$Data[which(df_main$sentNum == i & df_main$Question == "conf" & !is.na(df_main$Data) & df_main$isLeg == "ill")]))
  
}
conf_sent_crit$confidence_leg <- round(conf_sent_crit$confidence_leg,2)
conf_sent_crit$confidence_ill <- round(conf_sent_crit$confidence_ill,2)
conf_sent_crit <- mutate(conf_sent_crit,difference = confidence_leg - confidence_ill)
conf_sent_crit


# PERCENTAGE FILLER CORRECT BY SENTENCE
df_main$Data <- gsub("%2C","",df_main$Data) # replace %2C (commas) with nothing because they are not important
df_main$Data <- gsub("\\.","",df_main$Data) # remove periods because they are not important
df_main$Data <- gsub("’","",df_main$Data) # remove weird apostrophes because they mess everything up
df_main$Data <- gsub("'","",df_main$Data) # remove normal apostrophes because they mess everything up
df_main$Data <- tolower(df_main$Data) # make all same case

# do the same for text column
df_main$text <- gsub("%2C","",df_main$text) # replace %2C (commas) with nothing because they are not important
df_main$text <- gsub("\\.","",df_main$text) # remove periods because they are not important
df_main$text <- gsub("’","",df_main$text) # remove weird apostrophes because they mess everything up
df_main$text <- gsub("'","",df_main$text) # remove apostrophes because they mess everything up
df_main$text <- tolower(df_main$text) # make all same case


df_main_fill <- filter(df_main,isCrit == "fill")
df_main_fill_q1 <- filter(df_main_fill,Question == "q1")
mean(df_main_fill_q1$Data == df_main_fill_q1$text) # overall accuracy


perc_fill <- data.frame(sentence=21:80,percentage=0)
for (i in 1:nrow(perc_fill)) {
  curSentNum <- i + 20 # fillers start at 21
  perc_fill$percentage[i] <- mean(df_main_fill_q1$Data[which(df_main_fill_q1$sentNum == curSentNum)] == df_main_fill_q1$text[which(df_main_fill_q1$sentNum == curSentNum)])
}
perc_fill




# PER SENTENCE PERCENTAGE LITERAL WORD, NON LITERAL WORD, AND OTHER WORD, WHILE HAVING CONTEXT CORRECT, ALL SPLIT BY LEGIBILITY
df_main_crit <- filter(df_main, isCrit == "crit")
df_main_crit_sent <- filter(df_main_crit, isSent == "sent")
df_main_crit_word <- filter(df_main_crit, isSent == "word")



crit_words_implaus <- c("banks","smell","gold","desk","map","saved","ball","hit","naps","run","blamed","ties","liver","rain","halted","warm","water","hear","dining","meat")
crit_words_plaus <- c("barks","swell","mold","disk","mop","sawed","mall","lit","nags","sun","flamed","tics","liner","gain","halved","warn","wafer","near","pining","moat")




# PER CRITICAL WORD, PERCENTAGE CORRECT SPLIT BY LEGIBILITY
df_main_crit_word_q1 <- filter(df_main_crit_word,Question == "q1")
head(df_main_crit_word_q1)
leg_photo_word <- filter(leg_photo,grepl("*.word*.",photo))
leg_photo_word # per photo

perc_crit_word <- data.frame(word = 1:20,percentage_leg = 0, percentage_ill = 0)
for (i in 1:nrow(perc_crit_word)) {
  perc_crit_word$percentage[i] <- mean(df_main_crit_word_q1$Data[which(df_main_crit_word_q1$sentNum == i)] == df_main_crit_word_q1$text[which(df_main_crit_word_q1$sentNum == i)])
}
perc_crit_word





# GENDER STUFF
head(df_gender)


# DEMOGRAPHICS
df_demo <- select(df_demo,c("Name","Phase","TypeOption","Question","TypeChoice","Data"))
head(df_demo)