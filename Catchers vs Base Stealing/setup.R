library(tidyverse)
library(DT)

df <- read_csv("Matchup Predictor/fl_data_25.csv") %>%
  select(-`...1`)

keyPhrases = c("Catcher-SB", "Catcher-SB (2nd base)", "stole second")

keyPhrasesCaught = c("caught stealing", "Catcher-CS", "Catcher-CS (2nd base)", "catcher throws, out")

prep_sb_data <- function(df) {
  df %>%
    filter(PlayResult %in% c("StolenBase", "CaughtStealing") | 
             Notes %in% keyPhrases | Notes %in% keyPhrasesCaught) %>%
    mutate(
      SB_Attempt = 1,
      SB_Success = if_else((PlayResult == "StolenBase" | 
                             Notes %in% keyPhrases), 1, 0),
      SB_Fail = 1 - SB_Success
    )
}

# Notes = Catcher-SB
# Notes = Catcher-SB (2nd base)
