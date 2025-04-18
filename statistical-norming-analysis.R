# We need to make sure that the legible pictures are rated as significantly more legible than the illegible pictures :)
# The right way to do this would be with a linear, mixed-effects model, that predicts legibility rating as a function of condition (legible vs. illegible) and presentation type (full sentence vs. single word) with random effects by sentence and by participant:
#   
#   rating ~ 1 + condition*presentation + (1 + condition*presentation | sentence) + (1 + condition*presentation | participant)
# 
# If the model doesn’t converge, we can try removing some of the random slope.
# 
# We also need a plot showing two bars (one per condition, in including standard errors) of average ratings across pictures, with individual datapoints overlaid on top. This can be done (I think?) with geom_bar and geom_jitter in ggplot2. 
# 
# A quick-and-dirty test to see what the results are is to run a t-test:
#   * Average the ratings for each picture
# * Run a dependent-samples t-test comparing the ratings between the legible and illegible versions of each sentence
# 
# In addition, we should also look at the ratings of the fillers to see that they are legible as well. Hopefully they would be rated as more legible than the illegible critical sentences; and I expect them to not be statistically distinguishable from the legible critical sentences. For this, we can run a model just on full sentences (not individual words), and now the random effect is by “picture” (i.e., png file), not “sentence” (because some sentences only have the “filler” condition and some sentences only have the “legible”+”illegible” conditions):
#   
#   rating ~ 1 + condition + (1 | sentence) + (1 + condition | participant)
# 
# Here, condition has 3 levels (filler, legible, illegible), and we can do pairwise comparisons between filler and each of the other two levels.

library(readr)
library(dplyr)
library(stringr)
library(lme4)
library(lmerTest) # stack overflow said this would show p values
library(ggplot2)
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

# we need rating, condition, presentation, sentence, participant
# rating is data, condition is isLeg, presentation is isSent, sentence is sentNum
# participant is Name

df_main_qleg <- filter(df_main, Question == "leg")
df_main_qleg <- select(df_main_qleg, c("Name","Phase","Data","isLeg","isSent","sentNum"))

# set each person to a id so deidentified

key_deidentified <- setNames(as.character(1:length(list_of_names)),list_of_names)
df_main_qleg$Name <- str_replace_all(df_main_qleg$Name,key_deidentified)
df_main_qleg$Name <- as.factor(df_main_qleg$Name)

# fix sentNum
df_main_qleg$sentNum <- as.numeric(gsub("_","",df_main_qleg$sentNum)) # remove the first character with is an underscore

# fix data
df_main_qleg$Data <- as.numeric(df_main_qleg$Data)


df_main_qleg$isLeg <- ifelse(df_main_qleg$isLeg == "leg",1,0)

df_main_qleg$isSent <- ifelse(df_main_qleg$isSent == "sent",1,0)

# no fillers and no words
df_main_qleg_no_fill <- filter(df_main_qleg, sentNum < 21)

model_df <- df_main_qleg_no_fill


model_legible_data <- lmer(Data ~ 1 + isLeg*isSent
                                    + (1 + isLeg*isSent | sentNum)
                                    + (1 + isLeg*isSent | Name),
                                    data = df_main_qleg_no_fill)


update(model_legible_data, control = lmerControl(calc.derivs = FALSE))
summary(model_legible_data)

model_df$interaction <- interaction(model_df$isSent, model_df$isLeg)

model_interaction <- lmer(Data ~ 0 + interaction
                           + (1 + interaction | sentNum)
                           + (1 + interaction | Name),
                           data = model_df)
summary(model_interaction)

# two plots, one for sentences and one for words each with only CRITICAL ITEMS


df_main_no_fill <- filter(df_main, isCrit == "crit")

uniqueNames <- unique(df_main_no_fill$fileName)
isLeg <- ifelse(str_detect(uniqueNames,"leg"),"leg","ill")
isSent <- ifelse(str_detect(uniqueNames,"word"),"word","sent")
leg_photo <- data.frame(photo = uniqueNames, isLeg = isLeg, isSent = isSent, legibility = 0)
for (i in 1:nrow(leg_photo)) { # go through the rows
  curFileName <- leg_photo$photo[i]
  leg_photo$legibility[i] <- mean(as.numeric(df_main_no_fill$Data[which(df_main_no_fill$fileName == curFileName & df_main_no_fill$Question == "leg" & !is.na(df_main_no_fill$Data))]))
}
leg_photo$legibility <- round(leg_photo$legibility,2)
leg_photo
mean(filter(leg_photo,isLeg == "leg")$legibility) # avg legibility of legible sentences
mean(filter(leg_photo,isLeg == "ill")$legibility) # avg legibility of illegible sentences
mean(filter(leg_photo,isSent == "word" & isLeg == "ill")$legibility)


leg_photo_word <- filter(leg_photo, isSent == "word")
leg_photo_sent <- filter(leg_photo, isSent == "sent")


# filter out critical photos no longer using
# crit_19_leg_sent_16.png
# crit_02_ill_sent_8.png
# crit_08_ill_sent_113.png
# crit_15_ill_sent_104.png
# crit_18_ill_sent_113.png
# crit_19_ill_sent_101.png

removed_crit <- c("crit_19_leg_sent_16.png", 
                  "crit_02_ill_sent_8.png",
                  "crit_08_ill_sent_113.png",
                  "crit_15_ill_sent_104.png",
                  "crit_18_ill_sent_113.png",
                  "crit_19_ill_sent_101.png")
leg_photo_sent <- filter(leg_photo_sent, !(photo %in% removed_crit))

# PLOT
ggplot(leg_photo_word, aes(x = isLeg, y = legibility, fill = isLeg)) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6, alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2, color = "black") +
  theme_minimal() +
  labs(y = "Average Rating", x = "Legibility") +
  theme(legend.position = "none")
# continuous legibility???

ggplot(leg_photo_sent, aes(x = isLeg, y = legibility, fill = isLeg)) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6, alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2, color = "black") +
  theme_minimal() +
  labs(y = "Average Rating", x = "Legibility") +
  theme(legend.position = "none")



# DEPENDENT SAMPLES T TEST

#head(df_main)
uniqueNames <- unique(df_main$fileName)
isLeg <- ifelse(str_detect(uniqueNames,"leg"),"leg","ill")
leg_photo <- data.frame(photo = uniqueNames, isLeg = isLeg,legibility = 0)
for (i in 1:nrow(leg_photo)) { # go through the rows
  curFileName <- leg_photo$photo[i]
  leg_photo$legibility[i] <- mean(as.numeric(df_main$Data[which(df_main$fileName == curFileName & df_main$Question == "leg" & !is.na(df_main$Data))]))
}
leg_photo$legibility <- round(leg_photo$legibility,2)
leg_photo
#mean(filter(leg_photo,isLeg == "leg")$legibility) # avg legibility of legible sentences
#mean(filter(leg_photo,isLeg == "ill")$legibility) # avg legibility of illegible sentences
leg_photo$sentNum <- as.numeric(str_extract(leg_photo$photo,"\\d\\d"))

leg_photo_leg <- arrange(filter(leg_photo, isLeg == "leg"),sentNum)[1:40,] # only take the critical ones
leg_photo_ill <- arrange(filter(leg_photo, isLeg == "ill"),sentNum)

ratings_leg <- leg_photo_leg$legibility
ratings_ill <- leg_photo_ill$legibility


t.test(ratings_leg,ratings_ill,paired=TRUE)


# RATINGS OF FILLERS
# remove the 14 images that are changed:

# fill_23_leg_sent_89.png
# fill_30_leg_sent_120.png
# fill_31_leg_sent_62.png
# fill_43_leg_sent_85.png
# fill_60_leg_sent_66.png
# fill_61_leg_sent_28.png
# fill_68_leg_sent_81.png
# fill_72_leg_sent_55.png

removed_fill <- c("fill_23_leg_sent_89.png",
                  "fill_30_leg_sent_120.png",
                  "fill_31_leg_sent_62.png",
                  "fill_43_leg_sent_85.png",
                  "fill_60_leg_sent_66.png",
                  "fill_61_leg_sent_28.png",
                  "fill_68_leg_sent_81.png",
                  "fill_72_leg_sent_55.png")

df_main_fill <- filter(df_main, isCrit == "fill" & !(fileName %in% removed_fill))


uniqueNames <- unique(df_main_fill$fileName)
leg_fill <- data.frame(photo = uniqueNames, legibility = 0)
for (i in 1:nrow(leg_fill)) { # go through the rows
  curFileName <- leg_fill$photo[i]
  leg_fill$legibility[i] <- mean(as.numeric(df_main_fill$Data[which(df_main_fill$fileName == curFileName & df_main_fill$Question == "leg" & !is.na(df_main_fill$Data))]))
}
leg_fill$legibility <- round(leg_fill$legibility,2)
leg_fill

  
# In addition, we should also look at the ratings of the fillers to see that they are legible as well. 
#Hopefully they would be rated as more legible than the illegible critical sentences; 
#and I expect them to not be statistically distinguishable from the legible critical sentences. 
#For this, we can run a model just on full sentences (not individual words), 
#and now the random effect is by “picture” (i.e., png file), not “sentence” 
#(because some sentences only have the “filler” condition and some sentences only have the “legible”+”illegible” conditions):
#   
#   rating ~ 1 + condition + (1 | sentence) + (1 + condition | participant)
# 
# Here, condition has 3 levels (filler, legible, illegible), and we can do pairwise comparisons between filler and each of the other two levels.

