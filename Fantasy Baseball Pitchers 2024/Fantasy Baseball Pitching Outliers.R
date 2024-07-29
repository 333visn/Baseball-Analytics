library(rvest)
library(tidyverse)

# Yearly Statistics
prop1_url <- "https://www.cbssports.com/fantasy/baseball/stats/SP/2024/ytd/stats/"

html <- read_html(prop1_url)

owners0 <- html_nodes(html, ".TableBase-bodyTd , .TableBase-headTh .inner-value")

html_text(owners0,trim=TRUE)

list <- html_text(owners0,trim=TRUE)

list1 <- matrix(list, nrow = 100, byrow = TRUE)

df1 <- as.data.frame(list1)

extract_full_name <- function(text) {
  lines <- unlist(str_split(text, "\n"))
  # Find the line that contains both a first name and a last name
  full_name <- lines[grep(" ", lines)]
  return(str_trim(full_name[5]))
}
df1$full_name <- sapply(df1$V1, extract_full_name)

yearlystats <- df1 %>%
  mutate(`V1` = `full_name`) %>%
  rename("Player" = V1, "FPTS" = V2, "FPPG" = V3,	"W" = V4, "L" = V5,	"ERA" = V6,
         "GP" = V7,	"GS" = V8,	"QS" = V9,	"CG" = V10,	"IP" = V11,	"H" = V12,	
         "HR" = V13,	"BB" = V14,	"SO" = V15,	"AVG" = V16,	"WHIP" = V17,	"SV" = V18,
         "BS" = V19,	"SHO" = V20, "HLD" = V21, "GB%" = V22, "SO9" = V23, "BB9" = V24) %>%
  mutate("W" = as.double(W), "L" = as.double(L), "QS" = as.double(QS), 
         "SO9" = as.double(`SO9`), "BB9" = as.double(`BB9`), "GS" = as.integer(GS)) %>%
  mutate("W" = if_else(condition = is.na(W), true = 0, false = W)) %>%
  mutate("L" = if_else(condition = is.na(L), true = 0, false = L)) %>%
  mutate("QS" = if_else(condition = is.na(QS), true = 0, false = QS)) %>%
  mutate("FPTS" = as.double(FPTS), "ERA" = as.double(ERA), "SO" = as.double(SO)) %>%
  mutate("IP" = (10*(as.double(IP)) - 7*(round(as.double(IP))))/3) %>%
  mutate("FPTS.ESPN" =  round(FPTS - W*7 + W*2 + L*5 - L*2 - QS*3 - (ERA * IP / 9) + SO*0.5)) %>%
  mutate("FPPG.ESPN" =  round(FPTS.ESPN / as.double(GP), 1)) %>%
  mutate("KW" = round(as.double(`SO9`)/as.double(WHIP), 2)) %>%
  mutate("KGB" = round(as.double(`SO9`) * as.double(`GB%`), 2)) %>%
  select(Player, FPTS.ESPN, FPPG.ESPN, KW, KGB, matches("."), -full_name) %>%
  filter(GS > 7)
View(yearlystats)

# Extracting outlier performances potentially
outlierstats <- yearlystats %>%
  # Temporarily removing W/L
  mutate(FPTS.ESPN = FPTS.ESPN - (W * 2) + (L * 2)) %>%
  mutate(FPPG.ESPN = round(FPTS.ESPN / as.double(GP), 2)) %>%
  select(Player, FPTS.ESPN, FPPG.ESPN, KW) %>%
  mutate("FPTS.ESPN.adj" = 0, "FPPG.ESPN.adj" = 0, "KW.adj" = 0)

# Function to create a URL for a given player
create_player_url <- function(player_name) {
  # Convert player name to lowercase
  player_name <- tolower(player_name)
  
  player_name <- gsub(" ", "-", player_name)
  
  url_template <- "https://www.foxsports.com/mlb/{player}-player-game-log?season=2024&seasonType=reg"
  
  url <- gsub("\\{player\\}", player_name, url_template)
  
  return(url)
}

# Example usage
player_name <- outlierstats$Player[1]
url <- create_player_url(player_name)

# Print the URL
print(url)

for (i in 1:100) {
  if (outlierstats$Player[i] == "Matt Waldron") {
    prop1_url <- create_player_url("Matthew Waldron")
  } else if (outlierstats$Player[i] == "Jon Gray") {
    prop1_url <- create_player_url("Jonathan Gray")
  }
  else if (outlierstats$Player[i] == "Logan Taylor Allen") {
    prop1_url <- create_player_url(paste("Logan Allen", "3"))
  }
  else if (outlierstats$Player[i] == "Luis Castillo") {
    prop1_url <- create_player_url(paste(outlierstats$Player[i], "3"))
  } else if (outlierstats$Player[i] %in% c("Dylan Cease", "Cole Ragans", "Tanner Houck", 
                                           "Mitch Keller", "Cal Quantrill", "Zac Gallen",
                                           "MacKenzie Gore", "Chris Paddack")) {
    prop1_url <- create_player_url(paste(outlierstats$Player[i], "2"))
  } else {
    prop1_url <- create_player_url(outlierstats$Player[i])
  }
  
  html <- read_html(prop1_url)
  
  owners0 <- html_nodes(html, ".cell-number")
  
  html_text(owners0,trim=TRUE)
  
  list <- html_text(owners0,trim=TRUE)
  
  list1 <- matrix(list, ncol = 19, byrow = TRUE)
  
  df1 <- as.data.frame(list1)
  
  colnames(df1) <- df1[1, ]
  
  df1 <- df1[-1, ]
  
  gamelog <- df1 %>%
    mutate("GS" = as.double(GS), "GP" = as.double(GP), "IP" = as.double(IP), "ER" = as.double(ER),
           "H" = as.double(H), "BB" = as.double(BB), "SO" = as.double(SO), "HBP" = as.double(HBP)) %>%
    select(GS, GP, IP, ER, H, BB, SO, HBP) %>%
    filter(!is.na(IP)) %>%
    mutate("ERA" = round(ER * 9 / ((10 * IP - 7 * round(IP))/ 3), 2)) %>%
    mutate("FPTS.ESPN" = (10 * IP - 7 * round(IP)) + SO - BB - H - 2*ER - HBP) %>%
    mutate("ZScore" = (FPTS.ESPN - mean(FPTS.ESPN))/ sd(FPTS.ESPN)) %>%
    mutate("ZScoreERA" = (ERA - mean(ERA))/ sd(ERA)) %>%
    mutate("Player" = outlierstats$Player[i]) %>%
    filter(ZScore > -3)
    
  gamelog2 <- gamelog %>%
    mutate("IP" = (10 * IP - 7 * round(IP)) / 3) %>%
    summarise(sum(GS), sum(GP), sum(IP), sum(H), sum(BB), sum(SO), sum(HBP), sum(ER), sum(FPTS.ESPN)) %>%
    mutate("ERA" = round(`sum(ER)` * 9 / `sum(IP)`, 2)) %>%
    mutate("WHIP" = round((`sum(H)` + `sum(BB)`) / `sum(IP)`, 2))
    #view(gamelog)
    #view(gamelog2)
  
  outlierstats$FPTS.ESPN.adj[i] = gamelog2$`sum(FPTS.ESPN)`
  outlierstats$`FPPG.ESPN.adj`[i] = round(gamelog2$`sum(FPTS.ESPN)`/gamelog2$`sum(GP)`, 2)
  outlierstats$KW.adj[i] = round(gamelog2$`sum(SO)` * 9 / gamelog2$`sum(IP)` / gamelog2$WHIP, 2)
}

View(outlierstats)

# Luis Castillo did not work in the algorithm above for some reason, so we are manually pulling his statistics
# Luis Castillo Stats
prop1_url <- "https://www.foxsports.com/mlb/luis-castillo-3-player-game-log?season=2024&seasonType=reg"

html <- read_html(prop1_url)

owners0 <- html_nodes(html, ".cell-number")

html_text(owners0,trim=TRUE)

list <- html_text(owners0,trim=TRUE)

list1 <- matrix(list, ncol = 19, byrow = TRUE)

df1 <- as.data.frame(list1)

colnames(df1) <- df1[1, ]

df1 <- df1[-1, ]

castillostats <- df1 %>%
  mutate("GS" = as.double(GS), "GP" = as.double(GP), "IP" = as.double(IP), "ER" = as.double(ER),
         "H" = as.double(H), "BB" = as.double(BB), "SO" = as.double(SO), "HBP" = as.double(HBP)) %>%
  select(GS, GP, IP, ER, H, BB, SO, HBP) %>%
  filter(!is.na(IP)) %>%
  mutate("ERA" = round(ER * 9 / ((10 * IP - 7 * round(IP))/ 3), 2)) %>%
  mutate("FPTS.ESPN" = (10 * IP - 7 * round(IP)) + SO - BB - H - 2*ER - HBP) %>%
  mutate("ZScore" = (FPTS.ESPN - mean(FPTS.ESPN))/ sd(FPTS.ESPN)) %>%
  mutate("ZScoreERA" = (ERA - mean(ERA))/ sd(ERA))
# view(castillostats)
castillostats2 <- castillostats %>%
  mutate("IP" = (10 * IP - 7 * round(IP)) / 3) %>%
  summarise(sum(GS), sum(GP), sum(IP), sum(H), sum(BB), sum(SO), sum(HBP), sum(ER), sum(FPTS.ESPN)) %>%
  mutate("ERA" = round(`sum(ER)` * 9 / `sum(IP)`, 2)) %>%
  mutate("WHIP" = round((`sum(H)` + `sum(BB)`) / `sum(IP)`, 2))

outlierstats$FPTS.ESPN.adj[18] = castillostats2$`sum(FPTS.ESPN)`
outlierstats$`FPPG.ESPN.adj`[18] = round(castillostats2$`sum(FPTS.ESPN)`/castillostats2$`sum(GS)`, 2)
outlierstats$KW.adj[18] = round(castillostats2$`sum(SO)` * 9 / castillostats2$`sum(IP)` / castillostats2$WHIP, 2)

# Turning outlier stats into a spreadsheet
# write.csv(outlierstats, "outlierstats.csv")
