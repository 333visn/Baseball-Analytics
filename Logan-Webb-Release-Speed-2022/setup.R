library(baseballr)
library(tidyverse)

webb2022stats = try(statcast_search(start_date = "2022-04-08", 
                                    end_date = "2022-09-27", 
                                    playerid = 657277, 
                                    player_type = "pitcher"))
view(webb2022stats)

# Stand (r v l), pitch type(changeup, sinker, slider, four seam), innings, outs when up (e.g. 5th inning, 1 out)

loganwebb2022stats = webb2022stats %>%
  select(home_team, away_team, description, events, release_speed, inning, outs_when_up, pitch_type, stand, type)%>%
  mutate(stand = fct_recode(stand, "Right" = "R", "Left" = "L")) %>%
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH", "Slider" = "SL", "4-Seam" = "FF", "Sinker" = "SI")) %>%
  mutate(type = fct_recode(type, "Strike" = "S", "Ball" = "B"))
view(loganwebb2022stats)
write_csv(x=loganwebb2022stats, file = "data/loganwebb2022stats.csv")

