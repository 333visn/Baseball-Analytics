library(baseballr)
library(tidyverse)

# Update Sys.Date() -  as much as possible
# 788 pitches
# Season debut -> 5/9

schmitt2023stats = try(statcast_search(start_date = Sys.Date() - 200, 
                                    end_date = Sys.Date(), 
                                    playerid = 669477, 
                                    player_type = "batter"))
view(schmitt2023stats)

caseyschmitt2023stats = schmitt2023stats %>%
  # select(home_team, away_team, des, events, release_speed, inning, outs_when_up, pitch_type, p_throws, type)%>%
  # filter(!is.na(pitch_type)) %>%
  mutate(p_throws = fct_recode(p_throws, "Right" = "R", "Left" = "L")) %>%
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH", "Slider" = "SL", "4-Seam" = "FF", "Sinker" = "SI", 
                                 "Curveball" = "CU", "Cutter" = "FC", "Splitter" = "FS", "2-Seam" = "ST", "Knuckle Curve" = "KC",
                                 "Screwball" = "SC", "Slurve" = "SV", "IDK" = "")) %>%
  mutate(type = fct_recode(type, "Strike" = "S", "Ball" = "B", "X" = "X")) %>%
  mutate(events = fct_recode(events, "No event" = "", "Strikeout" = "strikeout", "Field out" = "field_out", "Single" = "single", "Force out" = "force_out",
                             "Field error" = "field_error", "Double" = "double", "Home run" = "home_run",
                             "Sac fly" = "sac_fly", "Double Play" = "grounded_into_double_play", "Hit by Pitch" = "hit_by_pitch"))
write_csv(x=caseyschmitt2023stats, file = "data/caseyschmitt2023stats.csv")

view(caseyschmitt2023stats)
