library(tidyverse)
library(lubridate)
library(zoo)

# Load Data
data <- read_csv("Matchup Predictor/fl_data_25.csv") %>%
  select(-`...1`)

df <- data %>%
  mutate(
    GameDate = ymd(Date),
    pitch_in_game = row_number(),
    inning_half = paste(Inning, Top.Bottom, sep = "_")
  ) %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    pitch_in_outing = row_number()
  ) %>%
  ungroup()

df <- df %>%
  mutate(
    in_zone = if_else(PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5 &
                        PlateLocSide >= -0.83 & PlateLocSide <= 0.83, 1, 0),
    pitch_in_outing = row_number(),
    GameDate = as.Date(Date)
  )

# Velo Drop
df <- df %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    velo_baseline = mean(RelSpeed[pitch_in_outing <= 10], na.rm = TRUE),
    velo_drop = RelSpeed - velo_baseline
  ) %>%
  ungroup()

df <- df %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    rel_dist = sqrt(
      (RelSide   - mean(RelSide,   na.rm = TRUE))^2 +
        (RelHeight - mean(RelHeight, na.rm = TRUE))^2
    ),
    rel_drift = rollapply(
      rel_dist,
      width = 15,
      FUN = mean,
      fill = NA,
      align = "right"
    )
  ) %>%
  ungroup()

df <- df %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    rel_dist = sqrt((RelSide - mean(RelSide, na.rm=TRUE))^2 +
                      (RelHeight - mean(RelHeight, na.rm=TRUE))^2),
    rel_drift_roll = rollapply(rel_dist, width = 15, FUN = mean, fill = NA, align = "right")
  ) %>%
  ungroup()


# Spin Rate Fatigue
df <- df %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    spin_baseline = mean(SpinRate[pitch_in_outing <= 10], na.rm = TRUE),
    spin_drop = SpinRate - spin_baseline
  ) %>%
  ungroup()

df <- df %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    zone_roll = rollapply(in_zone, width = 15, FUN = mean, fill = NA, align = "right"),
    velo_roll = rollapply(RelSpeed, width = 15, FUN = mean, fill = NA, align = "right"),
    spin_roll = rollapply(SpinRate, width = 15, FUN = mean, fill = NA, align = "right")
  ) %>%
  ungroup()

# Rolling SD of distance from cluster center
df <- df %>%
  group_by(Pitcher, GameDate) %>%
  mutate(
    dist_from_center = sqrt(
      (PlateLocSide  - mean(PlateLocSide,  na.rm = TRUE))^2 +
        (PlateLocHeight - mean(PlateLocHeight, na.rm = TRUE))^2
    ),
    loc_sd = rollapply(
      dist_from_center, 
      width = 15, 
      FUN = sd, 
      fill = NA, 
      align = "right"
    )
  ) %>%
  ungroup()

# Normalize each rolling metric
df <- df %>%
  mutate(
    z_velo  = scale(velo_roll)[,1] * -1,   # lower velo = more fatigue
    z_zone  = scale(zone_roll)[,1] * -1,   # lower zone% = more fatigue
    z_drift = scale(rel_drift_roll)[,1],   # more drift = more fatigue
    
    fatigue_index = z_velo + z_zone + z_drift
  )

pitcher_list <- sort(unique(df$Pitcher))

pitch_class_list <- c("All", sort(unique(df$PitchClass)))
