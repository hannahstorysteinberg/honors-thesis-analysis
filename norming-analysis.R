library(readr)
library(dplyr)
library(stringr)
res <- read_csv(file = "results.csv",col_names = FALSE) # kendra black and second philip naef was removed prior to this csv
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




# PER SENTENCE PERCENTAGE LITERAL WORD, NON LITERAL WORD, AND OTHER WORD, ALL SPLIT BY LEGIBILITY 
# (WHILE HAVING CONTEXT CORRECT???)



df_main_crit <- filter(df_main, isCrit == "crit")
df_main_crit_sent <- filter(df_main_crit, isSent == "sent")
df_main_crit_word <- filter(df_main_crit, isSent == "word")



crit_words_implaus <- c("banks","smell","gold","desk","map","saved","ball","hit","naps","run","blamed","ties","liver","rain","halted","warm","water","hear","dining","meat")
crit_words_plaus <- c("barks","swell","mold","disk","mop","sawed","mall","lit","nags","sun","flamed","tics","liner","gain","halved","warn","wafer","near","pining","moat")

df_main_crit_sent_q1 <- filter(df_main_crit_sent, Question == "q1")

#legible
df_main_crit_sent_q1_leg <- filter(df_main_crit_sent_q1, isLeg == "leg")

perc_crit_sent_leg <- data.frame(sentence = 1:20, percentage_literal = 0, percentage_nonliteral = 0, wrong = 0)
for (i in 1:nrow(perc_crit_sent_leg)) {
  cur_crit_word_implaus <- crit_words_implaus[i]
  cur_crit_word_plaus <- crit_words_plaus[i]
  curSentNum <- i
  cur_df <- filter(df_main_crit_sent_q1_leg, sentNum == curSentNum)
  
  perc_crit_sent_leg$percentage_literal[i] <- mean(grepl(cur_crit_word_implaus,cur_df$Data)) # literal, implausible word
  perc_crit_sent_leg$percentage_nonliteral[i] <- mean(grepl(cur_crit_word_plaus,cur_df$Data)) # non literal, plausible word
  perc_crit_sent_leg$wrong[i] <- 1 - mean(grepl(cur_crit_word_implaus,cur_df$Data)) - mean(grepl(cur_crit_word_plaus,cur_df$Data)) # any other word
}
round(perc_crit_sent_leg,2)


#illegible
df_main_crit_sent_q1_ill <- filter(df_main_crit_sent_q1, isLeg == "ill")

perc_crit_sent_ill <- data.frame(sentence = 1:20, percentage_literal = 0, percentage_nonliteral = 0, wrong = 0)
for (i in 1:nrow(perc_crit_sent_ill)) {
  cur_crit_word_implaus <- crit_words_implaus[i]
  cur_crit_word_plaus <- crit_words_plaus[i]
  curSentNum <- i
  cur_df <- filter(df_main_crit_sent_q1_ill, sentNum == curSentNum)
  
  perc_crit_sent_ill$percentage_literal[i] <- mean(grepl(cur_crit_word_implaus,cur_df$Data)) # literal, implausible word
  perc_crit_sent_ill$percentage_nonliteral[i] <- mean(grepl(cur_crit_word_plaus,cur_df$Data)) # non literal, plausible word
  perc_crit_sent_ill$wrong[i] <- 1 - mean(grepl(cur_crit_word_implaus,cur_df$Data)) - mean(grepl(cur_crit_word_plaus,cur_df$Data)) # any other word
}
round(perc_crit_sent_ill,2)


# PERCENTAGE CONTEXT CORRECT
# idea: match each word then *. in between, so need to match all words but can have an extra word inbetween which is the critical one
# so check if every word in text is in data, then determine about the critical word if plaus, implaus, or other
df_main_crit_q1 <- filter(df_main_crit,Question == "q1" & isSent == "sent")
df_main_crit_q1$text <- gsub(" ii","",df_main_crit_q1$text) # the pictures actually dont have the ii

perc_context <- data.frame(sentence = 1:20, percentage = 0)
for(i in 1:nrow(perc_context)) {
  cur_crit_word_implaus <- crit_words_implaus[i]
  curSentNum <- i
  cur_df <- filter(df_main_crit_q1, sentNum == curSentNum)
  cur_Sent <- cur_df$text[1]
  splitted <- strsplit(cur_Sent, cur_crit_word_implaus)[[1]]
  before <- splitted[1]
  after <- splitted[2]
  if (is.na(after)) {
    after <- ""
  }
  
  perc_context$percentage[i] <- mean(grepl(before,cur_df$Data) & grepl(after,cur_df$Data))
}
round(perc_context,2)


# PER CRITICAL WORD, PERCENTAGE CORRECT SPLIT BY LEGIBILITY
df_main_crit_word_q1 <- filter(df_main_crit_word,Question == "q1")
head(df_main_crit_word_q1)
leg_photo_word <- filter(leg_photo,grepl("*.word*.",photo))
leg_photo_word # per photo

perc_crit_word <- data.frame(word = 1:20,percentage_leg = 0, percentage_ill = 0)
for (i in 1:nrow(perc_crit_word)) {
  perc_crit_word$percentage_leg[i] <- mean(df_main_crit_word_q1$Data[which(df_main_crit_word_q1$sentNum == i & df_main_crit_word_q1$isLeg == "leg")] == df_main_crit_word_q1$text[which(df_main_crit_word_q1$sentNum == i & df_main_crit_word_q1$isLeg == "leg")])
  perc_crit_word$percentage_ill[i] <- mean(df_main_crit_word_q1$Data[which(df_main_crit_word_q1$sentNum == i & df_main_crit_word_q1$isLeg == "ill")] == df_main_crit_word_q1$text[which(df_main_crit_word_q1$sentNum == i & df_main_crit_word_q1$isLeg == "ill")])
}
round(perc_crit_word,2)






# GENDER STUFF
head(df_gender)
df_main_q1 <- filter(df_main, Question == "q1")
df_gender_q <- filter(df_gender, Question == "gender")
df_gender_q$Data <- gsub("Almost definitely a Man", 4,df_gender_q$Data)
df_gender_q$Data <- gsub("Probably a Man", 3,df_gender_q$Data)
df_gender_q$Data <- gsub("Not Sure", 2,df_gender_q$Data)
df_gender_q$Data <- gsub("Probably a Woman", 1,df_gender_q$Data)
df_gender_q$Data <- gsub("Almost definitely a Woman", 0,df_gender_q$Data)
df_gender_q$Data <- as.numeric(df_gender_q$Data)
df_gender_q$sentNum <- as.numeric(gsub("_","",df_gender_q$sentNum))




authors <- sort(unique(df_gender_q$author))
auth <- data.frame(authors = 0, sent_num = 0, leg = 0, gender = 0)

for (i in 1:length(authors)) {
  cur_author <- authors[i]
  sentNum <- unique(filter(df_main_q1, author == cur_author)$sentNum)
  for (j in 1:length(sentNum)) {
    cur_sent_num <- sentNum[j]
    cur_leg <- unique(filter(df_main_q1, author == cur_author & sentNum == cur_sent_num)$isLeg)
    row <- dim(auth)[1]+1
    auth[row,1] <- cur_author
    auth[row,2] <- cur_sent_num
    auth[row,3] <- cur_leg
    #auth_test[i,1] <- authors[i]
    
  }
}

auth$gender <- 0
auth <- auth[-1,]

for (i in 1:nrow(auth)) {
  cur_author <- auth$authors[i]
  cur_sent_num <- auth$sent_num[i]
  cur_df <- filter(df_gender_q, author == cur_author)
  auth$gender[i] <- mean(cur_df$Data)
}

auth$gender <- round(auth$gender,2)# larger the number, more male the author's writing is 
auth

# gender by photo
head(df_gender_q)
uniqueNames <- unique(df_gender_q$fileName)
isLeg <- ifelse(str_detect(uniqueNames,"leg"),"leg","ill")
gender_photo <- data.frame(photo = uniqueNames, isLeg = isLeg,gender = 0)

for (i in 1:nrow(gender_photo)) {
  cur_photo <- gender_photo$photo[i]
  cur_df <- filter(df_gender_q, fileName == cur_photo)
  gender_photo$gender[i] <- mean(cur_df$Data)
  
}
gender_photo$gender <- round(gender_photo$gender,2)
gender_photo
mean(filter(gender_photo,isLeg == "leg")$gender)


# DEMOGRAPHICS
df_demo <- select(df_demo,c("Name","Phase","TypeOption","Question","TypeChoice","Data"))
head(df_demo)
df_demo_age <- filter(df_demo, TypeChoice == "age")
ages <- as.numeric(df_demo_age$Data)
mean(ages) # for methods
sd(ages) # for methods

df_demo_gender <- filter(df_demo, TypeChoice == "gender")
gender <- as.factor(df_demo_gender$Data)
sum(as.numeric(gender)-1) # number males
sum(abs(as.numeric(gender)-2)) # number females