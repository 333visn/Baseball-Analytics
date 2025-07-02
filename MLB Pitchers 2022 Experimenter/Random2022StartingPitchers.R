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
library("bbd")
library("viridis")

sc2022 = bbd::statcast_bbd(start = "2022-01-01", end = "2022-12-13", verbose = TRUE)
# bbd::people_search("Dylan Cease")

geom_strikezone = function(color = "firebrick1", linewidth = 0.25, sz_top = 3.8, sz_bot = 1.1) {
  sz_left = -0.85
  sz_right = 0.85
  strikezone = data.frame(
    x = c(sz_left, sz_left, sz_right, sz_right, sz_left),
    y = c(sz_bot, sz_top, sz_top, sz_bot, sz_bot)
  )
  geom_path(aes(.data$x, .data$y), data = strikezone, linewidth = linewidth, col = color)
}

# Shohei Ohtani

ohtani2022 = filter(sc2022, pitcher == 660271)
# saveRDS(ohtani2022, file = "C:/Users/nikhi/Documents/MLBPitchersR/ShoheiOhtani2022.rds")
view(ohtani2022)

ohtani2022 |> 
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

ohtani2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

ohtani2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

ohtani2022 |> 
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

# Justin Verlander
verlander2022 = filter(sc2022, pitcher == 434378)
view(verlander2022)

verlander2022 |> 
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

verlander2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

verlander2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

verlander2022 |> 
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

# Sandy Alcantara

alcantara2022 = filter(sc2022, pitcher == 645261)
# saveRDS(alcantara2022, file = "C:/Users/nikhi/Documents/MLBPitchersR/SandyAlcantara2022.rds")
view(alcantara2022)

alcantara2022 |> 
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

alcantara2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

alcantara2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

alcantara2022 |> 
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

# Yu Darvish

darvish2022 = filter(sc2022, pitcher == 506433)
# saveRDS(darvish2022, file = "C:/Users/nikhi/Documents/MLBPitchersR/YuDarvish2022.rds")
darvish2022 <- readRDS("C:/Users/nikhi/Downloads/darvish2022.rds")
view(darvish2022)

darvish2022 |> 
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

darvish2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

darvish2022 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

darvish2022 |> 
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

# Alex Cobb

cobb22 = filter(sc2022, pitcher == 502171)
# saveRDS(cobb22, file = "C:/Users/nikhi/Documents/MLBPitchersR/AlexCobb22.rds")
view(cobb22)

cobb22 |> 
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

cobb22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

cobb22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

cobb22 |> 
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

# Gerrit Cole

cole22 = filter(sc2022, pitcher == 543037)
# saveRDS(cole22, file = "C:/Users/nikhi/Documents/MLBPitchersR/GerritCole22.rds")
view(cole22)

cole22 |> 
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

cole22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

cole22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

cole22 |> 
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

# Max Scherzer

scherzer22 = filter(sc2022, pitcher == 453286)
# saveRDS(scherzer22, file = "C:/Users/nikhi/Documents/MLBPitchersR/MaxScherzer22.rds")
view(scherzer22)

scherzer22 |> 
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

scherzer22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

scherzer22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

scherzer22 |> 
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

# Shane McClanahan

mcclanahan22 = filter(sc2022, pitcher == 663556)
# saveRDS(mcclanahan22, file = "C:/Users/nikhi/Documents/MLBPitchersR/ShaneMcClanahan2022.rds")
view(mcclanahan22)

mcclanahan22 |> 
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

mcclanahan22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

mcclanahan22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

mcclanahan22 |> 
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

# Jacob deGrom

degrom22 = filter(sc2022, pitcher == 594798)
# saveRDS(degrom22, file = "C:/Users/nikhi/Documents/MLBPitchersR/JacobdeGrom2022.rds")
view(degrom22)

degrom22 |> 
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

degrom22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

degrom22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

degrom22 |> 
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

# Zack Wheeler

wheeler22 = filter(sc2022, pitcher == 554430)
# saveRDS(wheeler22, file = "C:/Users/nikhi/Documents/MLBPitchersR/ZackWheeler2022.rds")
view(wheeler22)

wheeler22 |> 
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

wheeler22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

wheeler22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

wheeler22 |> 
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

# Kyle Gibson

gibson22 = filter(sc2022, pitcher == 502043)
# saveRDS(gibson22, file = "C:/Users/nikhi/Documents/MLBPitchersR/KyleGibson2022.rds")
view(gibson22)

gibson22 |> 
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

gibson22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

gibson22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

gibson22 |> 
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
# Aaron Nola

nola22 = filter(sc2022, pitcher == 605400)
# saveRDS(nola22, file = "C:/Users/nikhi/Documents/MLBPitchersR/AaronNola2022.rds")
view(nola22)

nola22 |> 
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

nola22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

nola22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

nola22 |> 
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

# Spencer Strider

strider22 = filter(sc2022, pitcher == 675911)
#saveRDS(strider22, file = "C:/Users/nikhi/Documents/MLBPitchersR/SpencerStrider2022.rds")
view(strider22)

strider22 |> 
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

strider22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

strider22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

strider22 |> 
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

# Max Fried
fried22 = filter(sc2022, pitcher == 608331)
saveRDS(fried22, file = "C:/Users/nikhi/Documents/MLBPitchersR/MaxFried2022.rds")
view(fried22)

fried22 |> 
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

fried22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = pitch_type, fill = pitch_type) + 
  geom_density_ridges() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

fried22 |> 
  filter(!is.na(pitch_type)) |> 
  ggplot() + 
  aes(x = release_speed, y = release_spin_rate) + 
  geom_point(aes(color = pitch_type), alpha = 0.4, size = 2) +
  theme_bw() + 
  scale_color_brewer(palette = "Set1")

fried22 |> 
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

# Next: Do Julio Urias, Alek Manoah, Zac Gallen, Tyler Anderson, Framer Valdez
# Next: Do Shane Bieber, Carlos Rodon, Martin Perez, Corbin Burnes, Robbie Ray
# Next: Do Charlie Morton, Kevin Gausman, Tony Gonsolin, Merrill Kelly, Joe Ryan
# Next: Do Pedro Martinez