library(readr)
library(tidyverse)
library(httr)
library(jsonlite)

# read in home runs df to get player_ids (data obtaine from statcast)
homeruns <- read_csv("homeruns.csv")
ids <- homeruns$player_id


# Single player URL
url <- "https://baseballsavant.mlb.com/leaderboard/home-runs?type=details&player_id=663728&year=2025&player_type=Batter&cat=adj_xhr"

# GET request
res <- GET(url)

# Convert to text
res_text <- content(res, as = "text", encoding = "UTF-8")

# Parse JSON
res_json <- fromJSON(res_text, flatten = TRUE)

# Initialize empty list to store results
all_data <- list()

# Loop through player IDs
for (pid in ids) {
  url <- paste0(
    "https://baseballsavant.mlb.com/leaderboard/home-runs?type=details&player_id=",
    pid,
    "&year=2025&player_type=Batter&cat=adj_xhr"
  )
  
  # GET and parse JSON
  res <- GET(url)
  df <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  # Add player_id column for reference
  df$player_id <- pid
  
  # Store in list
  all_data[[as.character(pid)]] <- df
}

# Combine all players into one data frame
final_df <- bind_rows(all_data)

# Select only the 30 team columns
team_cols <- c("laa","bal","bos","cws","cle","kc","oak","tb","tex","tor",
               "ari","chc","col","lad","pit","mil","sea","hou","det","sf",
               "cin","sd","phi","stl","nym","wsh","min","nyy","mia","atl")

# Sum team columns and pivot to long format
team_totals_long <- final_df |> 
  mutate(across(all_of(team_cols), as.numeric)) |> 
  summarise(across(all_of(team_cols), sum, na.rm = TRUE)) |> 
  pivot_longer(cols = everything(), names_to = "team", values_to = "total")

mean(team_totals_long$total)  # mean of expected home runs