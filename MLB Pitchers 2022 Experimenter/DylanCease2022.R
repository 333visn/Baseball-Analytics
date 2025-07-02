library("tidyverse")
library("ggdensity")
library("ggridges")
library("Lahman")
library("DT")
library(maps)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(readxl)
library("devtools")
devtools::install_github("daviddalpiaz/bbd")
sc2022 = bbd::statcast_bbd(start = "2022-01-01", end = "2022-12-13", verbose = TRUE)
cease2022 = filter(sc2022, pitcher == 656302)

cease2022 <- readRDS("C:/Users/nikhi/Downloads/cease2022.rds")
view(cease2022)

geom_strikezone = function(color = "black", linewidth = 0.25, sz_top = 3.8, sz_bot = 1.1) {
  sz_left = -0.85
  sz_right = 0.85
  strikezone = data.frame(
    x = c(sz_left, sz_left, sz_right, sz_right, sz_left),
    y = c(sz_bot, sz_top, sz_top, sz_bot, sz_bot)
  )
  geom_path(aes(.data$x, .data$y), data = strikezone, linewidth = linewidth, col = color)
}

cease2022 |> 
  filter(stand == "R") |>
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = plate_x, y = plate_z) + 
  geom_hdr(
    method = method_kde(), 
    probs = c(0.95, 0.75, 0.50, 0.25, 0.05)
  ) + 
  geom_strikezone() + 
  geom_point(
    aes(color = pitch_type), 
    alpha = 0.4
  ) +
  theme_bw() + 
  theme(
    aspect.ratio = 1, 
    legend.position = c(1, 0),
    legend.justification = c(1, 0), 
    legend.box = "horizontal"
  ) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(pitch_type))

cease2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

cease2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

cease2022 |> 
  filter(!is.na(pitch_type)) |> 
  summarise(
    n = n(), 
    velo = round(mean(release_speed), digits = 1),
    spin = round(mean(release_spin_rate), digits = 1),
    h_move = round(mean(pfx_x), digits = 2),
    v_move = round(mean(pfx_z), digits = 2),
    .by = c(pitch_type, pitch_name)) |> 
  arrange(desc(n)) |> 
  datatable(
    rownames = FALSE, 
    options = list(dom = 't'),
    colnames = c(
      "Pitch Type" = "pitch_type",
      "Pitch Name" = "pitch_name",
      "Number Thrown" = "n",
      "Average Velocity" = "velo",
      "Average Spin" = "spin",
      "Average Horizontal Movement" = "h_move",
      "Average Vertical Movement" = "v_move"
    )
  )