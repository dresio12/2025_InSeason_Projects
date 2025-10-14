library(httr)
library(jsonlite)
library(tidyr)
library(purrr)
library(stringr)
library(readr)

# Team ID lookup
team_lookup <- c(
  "LAA","BAL","BOS","CHW","CLE","DET","KCR","MIN","NYY","ATH",
  "SEA","TBR","TEX","TOR","ARI","ATL","CHC","CIN","COL","MIA",
  "HOU","LAD","MIL","WSN","NYM","PHI","PIT","STL","SDP","SFG"
)

all_teams <- lapply(1:30, function(i) {
  url <- paste0("https://www.fangraphs.com/api/depth-charts/roster?teamid=", i, "&loaddate=1760112111")
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  df_flat <- jsonlite::fromJSON(txt, flatten = TRUE)
  df_flat$teamid <- i
  df_flat
})

# Combine into one data frame
df_all <- bind_rows(all_teams)
df_all$teamid <- team_lookup[df_all$teamid]

# filter down to R5 players
r5 <- df_all |>
  filter(options1 == "R5")

# Filter columns
r5_filt <- r5 |>
  select(1, 3:7, 10:12, 20:21, 24:25, 45:47, 49, 60, 63:64, 141:142, 212:213, 
         216:217, 223, 230, 235, 147:148, 244:245, 68)

#### cleaning columns and data ####

# create primary position
r5_filt$primary_pos <- sub("/.*", "", r5_filt$position)

#create current level
r5_filt <- r5_filt |>
  mutate(
    level = case_when(
      str_starts(type, "off") ~ role,  # if type starts with "off", take role
      TRUE ~ str_to_upper(str_extract(type, "^[^-]+"))  # otherwise take before "-"
    ),
    # Fix "HA" and "LA" special cases
    level = case_when(
      level == "HA" ~ "A+",
      level == "LA" ~ "A",
      TRUE ~ level
    )
  )

# Vector of row IDs to manually change for level "ss" or "Inj"
rok_rows <- c(
  197, 251, 252, 294, 389, 390, 662, 663, 743, 791, 873, 957,
  1006, 1007, 1008, 1081, 1082, 1132, 1133, 1226, 1227, 1327, 134, 
  507, 1030
)

# Update level for these rows
r5_filt$level[rok_rows] <- c(
  rep("ROK", 12), "A", "ROK", "A", "A", "A", "ROK", "A", "ROK", "ROK", "ROK",
  "AA", "AA", "AA"
)

# renaming columns to be more descriptive
r5_new <- r5_filt |>
  rename(
    Organization = teamid,
    Player = player,
    Age = age1,
    Bats = bats,
    Throws = throws,
    `Org Rank` = Org_Rank_Next,
    `Ovr Rank` = Ovr_Rank_Next,
    `Ovr Power Rank` = Overall_Rank,
    Position = primary_pos,
    Level = level
  )

r5_new <- r5_new |>
  select(1, 35, 36, 6, 11:13, 31, 33:34, 14:17)

#### load in minor league stats ####

# batting - split
url <- "https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=bat&qual=0&type=2&team=0,to&season=2025&seasonEnd=2025&org=&ind=0&splitTeam=true"
resp <- httr::GET(url)
httr::stop_for_status(resp)
txt <- httr::content(resp, as = "text", encoding = "UTF-8")

df_flat_bat <- jsonlite::fromJSON(txt, flatten = TRUE)

# batting - no split
url <- "https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=bat&qual=0&type=2&team=&season=2025&seasonEnd=2025&org=&ind=0&splitTeam=false"
resp <- httr::GET(url)
httr::stop_for_status(resp)
txt <- httr::content(resp, as = "text", encoding = "UTF-8")

df_flat_bat_tot <- jsonlite::fromJSON(txt, flatten = TRUE)

# pitching - split
url <- "https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=pit&qual=0&type=2&team=0,to&season=2025&seasonEnd=2025&org=&ind=0&splitTeam=true"
resp <- httr::GET(url)
httr::stop_for_status(resp)
txt <- httr::content(resp, as = "text", encoding = "UTF-8")

df_flat_pit <- jsonlite::fromJSON(txt, flatten = TRUE)

# pitching - no split
url <- "https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=pit&qual=0&type=2&team=&season=2025&seasonEnd=2025&org=&ind=0&splitTeam=false"
resp <- httr::GET(url)
httr::stop_for_status(resp)
txt <- httr::content(resp, as = "text", encoding = "UTF-8")

df_flat_pit_tot <- jsonlite::fromJSON(txt, flatten = TRUE)

# join data
pitchers <- left_join(df_flat_pit, r5_new, by = "minormasterid")

batters <- left_join(df_flat_bat, r5_new, by = "minormasterid")

pitchers_tot <- left_join(df_flat_pit_tot, r5_new, by = "minormasterid")

batters_tot <- left_join(df_flat_bat_tot, r5_new, by = "minormasterid")

# remove non-R5
pitchers <- pitchers |> filter(!is.na(Level))
batters <- batters |> filter(!is.na(Level))
pitchers_tot <- pitchers_tot |> filter(!is.na(Level))
batters_tot <- batters_tot |> filter(!is.na(Level))

# remove parentheses with team initials from player names
pitchers$Player <- gsub("\\s*\\(.*?\\)", "", pitchers$Player)
pitchers_tot$Player <- gsub("\\s*\\(.*?\\)", "", pitchers_tot$Player)
batters$Player <- gsub("\\s*\\(.*?\\)", "", batters$Player)
batters_tot$Player <- gsub("\\s*\\(.*?\\)", "", batters_tot$Player)
r5_new$Player <- gsub("\\s*\\(.*?\\)", "", r5_new$Player)

pitchers <- pitchers |>
  mutate(`Strike%` = Strikes / Pitches) |>   
  select(
    Player, Position, Throws, Age = Age.y, Level = aLevel, Organization, `Org Rank`, `Ovr Rank`,
    W, L, ERA, G, GS, SV, HLD = Hld, IP, TBF, Pitches, `K/9`, `BB/9`, `K/BB`, `K-BB%`, 
    `Strike%`, `SwStr%`, `HR/9`, AVG, WHIP, BABIP, `LOB%`, FIP, xFIP, `GB%`, `FB%`, 
    `LD%`, `HR/FB`, `GB/FB`, `Oppo%`, `Pull%`, `Cent%`
  )

pitchers_tot <- pitchers_tot |>
  mutate(`Strike%` = Strikes / Pitches) |>   
  select(
    Player, Position, Throws, Age = Age.y, Level, Organization, `Org Rank`, `Ovr Rank`,
    W, L, ERA, G, GS, SV, HLD = Hld, IP, TBF, Pitches, `K/9`, `BB/9`, `K/BB`, `K-BB%`, 
    `Strike%`, `SwStr%`, `HR/9`, AVG, WHIP, BABIP, `LOB%`, FIP, xFIP, `GB%`, `FB%`, 
    `LD%`, `HR/FB`, `GB/FB`, `Oppo%`, `Pull%`, `Cent%`
  )

batters <- batters |>
  select(
    Player, Position, Bats, Age = Age.y, Level = aLevel, Organization, `Org Rank`, `Ovr Rank`,
    G, AB, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, HBP, SO, SB, CS, 22:31,
    34, 35, 37:39, 41:45, 49, Pitches
  ) |>
  filter(Position != "SP" & Position != "RP")

batters_tot <- batters_tot |>
  select(
    Player, Position, Bats, Age = Age.y, Level, Organization, `Org Rank`, `Ovr Rank`,
    G, AB, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, HBP, SO, SB, CS, 22:31,
    34, 35, 37:39, 41:45, 49, Pitches
  ) |>
  filter(Position != "SP" & Position != "RP")

r5_new <- unique(r5_new)
batters <- unique(batters)
batters_tot <- unique(batters_tot)
pitchers <- unique(pitchers)
pitchers_tot <- unique(pitchers_tot)

saveRDS(r5_new, "r5_all.rds")
saveRDS(batters, "r5_bsplit.rds")
saveRDS(pitchers, "r5_psplit.rds")
saveRDS(batters_tot, "r5_btot.rds")
saveRDS(pitchers_tot, "r5_ptot.rds")

# Prospect Savant Data
aaa_hitters <- read_csv("aaa_hitters.csv")
aaa_pitchers <- read_csv("aaa_pitchers.csv")

aaa_hitters <- aaa_hitters |>
  mutate(Level = "AAA") |>
  select(
   1:3, 10, 8:9, 39, 56, 57, 40, 58, 41, 48, 42, 50, 44, 47, 36, 51:55, 5:6, Level
  )


aaa_r5_advstats <- left_join(batters |>
                    select(Player, Organization, Level) |>
                    filter(Level == "AAA"), 
                  aaa_hitters, 
                  by = join_by(Player, Organization, Level)) |>
  filter(!is.na(Pos)) |>
  unique()


aaa_pitchers <- aaa_pitchers |>
  mutate(Level = "AAA") |>
  select(
    1:3, 10, 13, 8:9, 30, 40, 31, 42, 41, 33, 37, 34, 39, 36, 54:57, 5:7, Level
  )

aaa_r5_advstatsp <- left_join(pitchers |>
                           select(Player, Organization, Level) |>
                           filter(Level == "AAA"), 
                         aaa_pitchers, 
                         by = join_by(Player, Organization, Level)) |>
  filter(!is.na(Pos)) |>
  unique()


saveRDS(aaa_r5_advstats, "r5_advh.rds")
saveRDS(aaa_r5_advstatsp, "r5_advp.rds")
