library(baseballr)
library(tidyverse)

# Kirby: 669923
# Castillo: 622491
# Woo: 693433
# Gilbert: 669302
# Miller: 682243

Kirby = try(statcast_search(start_date = "2023-03-01", 
                            end_date = "2023-11-01", 
                            playerid = c(669923),
                            player_type = "pitcher"))

Castillo = try(statcast_search(start_date = "2023-03-01", 
                               end_date = "2023-11-01", 
                               playerid = c(622491),
                               player_type = "pitcher"))

Woo = try(statcast_search(start_date = "2023-03-01", 
                          end_date = "2023-11-01", 
                          playerid = c(693433),
                          player_type = "pitcher"))

Gilbert = try(statcast_search(start_date = "2023-03-01", 
                              end_date = "2023-11-01", 
                              playerid = c(669302),
                              player_type = "pitcher"))

Miller = try(statcast_search(start_date = "2023-03-01", 
                             end_date = "2023-11-01", 
                             playerid = c(682243),
                             player_type = "pitcher"))

Mariners2023Rotation <- Kirby %>%
  full_join(Castillo) %>%
  full_join(Woo) %>%
  full_join(Gilbert) %>%
  full_join(Miller)

view(Mariners2023Rotation)

# write_csv(x=Mariners2023Rotation, file = "sp24_stat430_nsiva3/final/SeattleMarinersStartingRotation2023/data/Mariners2023Rotation.csv")
