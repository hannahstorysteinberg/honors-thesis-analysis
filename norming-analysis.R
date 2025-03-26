library(readr)
library(dplyr)
df <- read_csv(file = "results.csv",col_names = FALSE)
head(df)
colnames(df) <- LETTERS[1:24]
head(df)
