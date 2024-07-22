library(baseballr)
library(tidyverse)

Mariners2023Rotation <- read_csv("data/Mariners2023Rotation.csv")

dwar_df <- read_csv("data/dwar_df.csv") %>%
  mutate("team_ID" = `teamID`) %>%
  select(`team_ID`, `dWAR`)

bwar_pit = readr::read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt", na = "NULL")

bwar_pit23 = bwar_pit %>%
  filter(year_ID == 2023) %>%
  filter(GS >= 10) %>%
  select(team_ID, IPouts, IPouts_start, WAR, runs_above_avg, runs_above_rep, xRA, ERA_plus, xRA_def_pitcher) %>%
  group_by(team_ID) %>%
  summarise("IPouts" = sum(IPouts), "IPouts_start" = sum(IPouts_start), "WAR" = sum(WAR), 
            "runs_above_avg" = sum(runs_above_avg), "runs_above_rep" = sum(runs_above_rep), "xRA" = sum(xRA), 
            "ERA_plus" = sum(ERA_plus), "xRA_def_pitcher" = sum(xRA_def_pitcher)) %>%
  mutate("ERA_plus" = ERA_plus * 9 / IPouts) %>%
  left_join(dwar_df)
bwar_pit23

catcher_dwar23 <- read_csv("data/catcherdwar.csv")
CatcherReps <- Mariners2023Rotation %>%
  mutate("catcher_name" = case_when(fielder_2 == 663728 ~  "Raleigh, Cal", fielder_2 == 620443 ~ "Torrens, Luis",
                                    fielder_2 == 657247 ~ "O'Keefe, Brian", fielder_2 == 608596 ~ "Murphy, Tom")) %>%
  group_by(catcher_name) %>%
  select(catcher_name) %>%
  filter(!is.na(catcher_name)) %>%
  summarise("reps" = n()) %>%
  mutate("repsPerc" = reps / (reps[1] + reps[2] + reps[3] + reps[4]))

wobaAgainst <- Mariners2023Rotation %>%
  filter(!is.na(woba_value)) %>%
  filter(pitch_name != "") %>%
  group_by(player_name, pitch_name) %>%
  summarise(WOBA = mean(woba_value), BABIP = mean(babip_value))