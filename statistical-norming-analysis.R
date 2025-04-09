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
test <- select(df_main_qleg, c("Name","Phase","Data","isLeg","isSent"))

# set each person to a id so deidentified

key_deidentified <- setNames(as.character(1:length(list_of_names)),list_of_names)
test$Name <- str_replace_all(test$Name,key_deidentified)
test$Name <- as.numeric(test$Name)
