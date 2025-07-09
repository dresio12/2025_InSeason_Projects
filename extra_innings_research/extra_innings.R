library(tidyverse)
library(baseballr)
library(purrr)
library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
library(lubridate)
library(BSDA)
library(readxl)
library(readr)
library(progressr)
library(ggplot2)
library(ggrepel)

# ---- Getting Game Data for Extra Inning Games ----
# Define the seasons
seasons <- 2020:2025

# Loop through each season and bind rows
game_pks <- map_dfr(seasons, function(yr) {
  baseballr::mlb_schedule(season = yr) |>
    filter(game_type == "R", status_abstract_game_state == "Final") |>
    select(game_pk) |>
    distinct()
})

# currentInning serves as proxy for how many total innings were played
# Function to get currentInning from MLB API
get_extra_innings <- function(game_pk) {
  url <- paste0("http://statsapi.mlb.com/api/v1/game/", game_pk, "/linescore")
  
  tryCatch({
    res <- GET(url)
    if (res$status_code == 200) {
      content <- content(res, as = "text", encoding = "UTF-8")
      json <- fromJSON(content)
      tibble(game_pk = game_pk, currentInning = json$currentInning)
    } else {
      tibble(game_pk = game_pk, currentInning = NA_integer_)
    }
  }, error = function(e) {
    tibble(game_pk = game_pk, currentInning = NA_integer_)
  })
}

# Load or initialize progress
rds_path <- "extra_innings_progress.rds"
if (file.exists(rds_path)) {
  extra_innings <- readRDS(rds_path)
  completed_ids <- extra_innings$game_pk
} else {
  extra_innings <- tibble(game_pk = character(), currentInning = integer())
  completed_ids <- character()
}

# Get remaining game_pks
remaining_ids <- setdiff(game_pks$game_pk, completed_ids)

# Loop through remaining IDs with visible progress
for (i in seq_along(remaining_ids)) {
  pk <- remaining_ids[i]
  new_result <- get_extra_innings(pk)
  extra_innings <- bind_rows(extra_innings, new_result)
  saveRDS(extra_innings, rds_path)
  
  cat(sprintf("[%d/%d] Processed game_pk: %s\n", i, length(remaining_ids), pk))
  Sys.sleep(0.1)  # Optional rate limit
}
#get games that go intro extras
extra_innings <- extra_innings |>
  filter(currentInning >= 10)

#
#

# get game info for each game_pk in extra_innings
game_ids <- tibble(game_pk = unique(extra_innings$game_pk))

# function to identify game_pks in game_ids df, combine into single df
get_pbp_for_games <- function(game_ids, save_path = "ei_pbp.rds") {
  # Load existing data if the save file exists
  if (file.exists(save_path)) {
    message("Loading existing saved progress...")
    pbp_combined <- readRDS(save_path)
    
    # Identify already pulled game_pks
    pulled_game_pks <- unique(pbp_combined$game_pk)
  } else {
    pbp_combined <- tibble()
    pulled_game_pks <- c()
  }
  
  game_pks <- game_ids$game_pk
  
  # Only pull game_pks that have NOT been pulled yet
  game_pks_to_pull <- setdiff(game_pks, pulled_game_pks)
  
  message(length(game_pks_to_pull), " game(s) to pull...")
  
  for (pk in game_pks_to_pull) {
    message("Pulling game_pk: ", pk)
    
    pbp_data <- possibly(mlb_pbp, otherwise = NULL)(pk)
    
    if (!is.null(pbp_data)) {
      
      # FIXED: Convert startTime and endTime to Arizona time correctly
      # Parse as UTC first, then convert to Arizona time
      pbp_data <- pbp_data |>
        mutate(
          # Parse the UTC timestamp and convert to Arizona time
          startTime = with_tz(ymd_hms(startTime), tzone = "America/Phoenix"),
          endTime   = with_tz(ymd_hms(endTime), tzone = "America/Phoenix")
        ) |>
        filter(!is.na(pitchNumber)) |>
        arrange(game_date, game_pk, startTime) |>
        group_by(game_pk) |>
        # Extract first 10 characters (YYYY-MM-DD) from Arizona startTime and convert to date
        mutate(game_date = as.Date(substr(as.character(first(startTime)), 1, 10))) |>
        ungroup()
      
      # Bind the rows
      pbp_combined <- bind_rows(pbp_combined, pbp_data)
      
      # Save the current progress after each successful pull
      saveRDS(pbp_combined, save_path)
      
      message("Saved progress after game_pk: ", pk)
    } else {
      message("No data for game_pk: ", pk)
    }
  }
  
  return(pbp_combined)
}

# Use function to generate pbp df
get_pbp_for_games(game_ids, save_path = "ei_pbp.rds")

#read in pbp
pbp <- readRDS("ei_pbp.rds")

# Remove special characters
pbp <- pbp |>
  mutate(across(where(is.character), ~ str_replace_all(., "Ã±", "n")),
         across(where(is.character), ~ str_replace_all(., "Ã©", "e")),
         across(where(is.character), ~ str_replace_all(., "Ã³", "o")),
         across(where(is.character), ~ str_replace_all(., "Ã¡", "a")),
         across(where(is.character), ~ str_replace_all(., "Ãº", "u")),
         across(where(is.character), ~ str_replace_all(., "Ã", "i")))

# Save 
saveRDS(pbp, "ei_pbp.rds")

# ----Generating master dataframes for identifying trusted relievers ----

#get only 10+ innings rows
logs <- pbp 

#gets player FG and MLBAM ids
pitchers <- baseballr::chadwick_player_lu() |>
  select(3, 7, 27)

#
#join appropriate fangraphs IDs to MLBAM
logs <- left_join(logs, pitchers, by = c("matchup.pitcher.id" = "key_mlbam"))

#identify Fangraphs NAs and manually input
logs_na <- logs |> 
  filter(is.na(key_fangraphs)) |>
  select(72:73, key_fangraphs) |>
  unique()

logs_good <- logs |> 
  filter(!is.na(key_fangraphs)) |>
  select(72:73, key_fangraphs) |>
  unique()

#manual fangraphs keys
all_manual_keys <- read_excel("all_manual_keys.xlsx") |>
  select(1,3)

#apply to NA df
logs_na <- logs_na |>
  select(1:2) |> 
  left_join(all_manual_keys)

#apply to regular df
logs2 <- bind_rows(logs_na, logs_good)

logs <- logs |>
  select(-key_fangraphs) 

logs <- left_join(logs, logs2, by = c("matchup.pitcher.id", "matchup.pitcher.fullName"))

#function to get the yearly game logs for each pitcher in logs df
get_all_game_logs <- function(logs_df, save_path = "all_game_logs.rds") {
  # Load existing data if the save file exists
  if (file.exists(save_path)) {
    message("Loading existing saved progress...")
    logs_combined <- readRDS(save_path)
    
    # Get existing pitcher+date combinations
    existing_combinations <- logs_combined |>
      mutate(gamedate = as.Date(gamedate)) |>
      distinct(key_fangraphs, gamedate) |>
      mutate(combo_key = paste(key_fangraphs, gamedate, sep = "_"))
    
    existing_keys <- existing_combinations$combo_key
  } else {
    logs_combined <- tibble()
    existing_keys <- character()
  }
  
  # Get needed pitcher+date combinations from logs
  needed_combinations <- logs_df |>
    filter(!is.na(key_fangraphs)) |>
    distinct(key_fangraphs, game_date) |>
    mutate(combo_key = paste(key_fangraphs, game_date, sep = "_"))
  
  # Find combinations we don't have yet
  missing_combinations <- needed_combinations |>
    filter(!combo_key %in% existing_keys)
  
  message(nrow(missing_combinations), " pitcher-date combination(s) to pull...")
  
  if (nrow(missing_combinations) == 0) {
    message("All needed game logs already exist!")
    return(logs_combined)
  }
  
  for (i in seq_len(nrow(missing_combinations))) {
    fg_id <- missing_combinations$key_fangraphs[i]
    game_date <- missing_combinations$game_date[i]
    year <- as.numeric(format(game_date, "%Y"))
    
    message("Pulling logs for pitcher ", fg_id, " on date ", game_date)
    
    # Pull the full year's game logs for this pitcher
    pitcher_logs <- tryCatch(
      {
        result <- baseballr::pitcher_game_logs_fg(playerid = fg_id, year = year)
        if (!is.null(result) && nrow(result) > 0) {
          result |> mutate(key_fangraphs = fg_id)
        } else {
          NULL
        }
      },
      error = function(e) {
        message("  Error pulling year ", year, " for pitcher ", fg_id, ": ", e$message)
        return(NULL)
      }
    )
    
    if (!is.null(pitcher_logs)) {
      # Standardize date format before binding
      pitcher_logs <- pitcher_logs |>
        mutate(Date = as.Date(Date))
      
      # If logs_combined has dates, make sure they're also Date type
      if (nrow(logs_combined) > 0) {
        logs_combined <- logs_combined |>
          mutate(Date = as.Date(Date))
      }
      
      # Remove any potential duplicates before binding
      pitcher_logs <- pitcher_logs |>
        anti_join(logs_combined, by = c("key_fangraphs", "Date"))
      
      # Append to combined logs
      logs_combined <- bind_rows(logs_combined, pitcher_logs)
      message("  Added ", nrow(pitcher_logs), " game logs for ", year)
    } else {
      message("  No data found for pitcher ", fg_id, " in ", year)
    }
    
    # Save progress after each pitcher
    saveRDS(logs_combined, save_path)
    
    # Optional: sleep to reduce load on Fangraphs
    Sys.sleep(0.5)
  }
  
  return(logs_combined)
}

# run function
all_logs <- get_all_game_logs(logs, save_path = "all_game_logs.rds")

# What we have now are the 1) extra inning PBP rows and 
# 2) all game logs for pitchers that pitched in extra innings
# HOWEVER
# FG does not calculate running stats for game logs, everything is individual
# THUS, we need to calculate them. I am interested in 
# 1) Season avgs up to the specific appearance in logs dataframe
# 2) The end of season stat for the year of that appearance
# 3) the career avgs up to the specific appearance in logs dataframe
# 4) the career avgs at the end of the the year of that appearance
# 5) career avgs-to-date
# For ERA, WHIP, K%, K9, BB%, BB9, K/BB, GB%, FB%, GB/FB, HR/9, HR/FB

# NOTE: stats like FIP, xFIP, SIERA could be included if I calculated the constants and
# necessary lgSTATS using entire seasons of game logs of all pitchers
all_logs <- readRDS("all_game_logs.rds")
# reduce df size
all_logs <- all_logs |>
  arrange(PlayerName, Date) |>
  select(-49, -55:-60, -72:-114, -116:-192, -197:-363) |> 
  unique()

all_logs <- all_logs |>
  mutate(bipCount = ifelse(is.na(bipCount), 0, bipCount))

# calculate running stats by pitcher, season

# create outs recorded for stats reliant on IP
all_logs <- all_logs |>
  mutate(
    outs_recorded = floor(IP) * 3 + round((IP %% 1) * 10)
  )

# running stat columns for the season
all_logs<- all_logs |>
  group_by(PlayerName, playerid, season) |>
  mutate(
    cume_ER = cumsum(ER),
    cume_H = cumsum(H),
    cume_BB = cumsum(BB),
    cume_SO = cumsum(SO),
    cume_HR = cumsum(HR),
    cume_IP_outs = cumsum(outs_recorded),
    cume_IP = cume_IP_outs / 3,
    cume_TBF = cumsum(TBF),
    cume_GB = cumsum(GB),
    cume_FB = cumsum(FB),
    cume_bip = cumsum(bipCount),
    
    # Cumulative Rates
    SV_YTD = cumsum(SV),
    HLD_YTD = cumsum(HLD),
    BS_YTD = cumsum(BS),
    ERA_YTD = (cume_ER / cume_IP) * 9,
    WHIP_YTD = (cume_BB + cume_H) / cume_IP,
    K_pct_YTD = cume_SO / cume_TBF,
    BB_pct_YTD = cume_BB / cume_TBF,
    H_9_YTD = (cume_H / cume_IP) * 9,
    K_9_YTD = (cume_SO / cume_IP) * 9,
    BB_9_YTD = (cume_BB / cume_IP) * 9,
    K_BB_YTD = cume_SO / cume_BB,
    GB_pct_YTD = cume_GB / cume_bip,
    FB_pct_YTD = cume_FB / cume_bip,
    GB_FB_YTD = cume_GB / cume_FB,
    HR_9_YTD = (cume_HR / cume_IP) * 9,
    HR_FB_YTD = cume_HR / cume_FB
  ) |>
  ungroup()

#End of season stats for that season
all_logs <- all_logs |>
  group_by(PlayerName, playerid, season) |>
  mutate(
    SV_SZN = last(SV_YTD),
    HLD_SZN = last(HLD_YTD),
    BS_SZN = last(BS_YTD),
    ERA_SZN = last(ERA_YTD),
    WHIP_SZN = last(WHIP_YTD),
    K_pct_SZN = last(K_pct_YTD),
    BB_pct_SZN = last(BB_pct_YTD),
    H_9_SZN = last(H_9_YTD),
    K_9_SZN = last(K_9_YTD),
    BB_9_SZN = last(BB_9_YTD),
    K_BB_SZN = last(K_BB_YTD),
    GB_pct_SZN = last(GB_pct_YTD),
    FB_pct_SZN = last(FB_pct_YTD),
    GB_FB_SZN = last(GB_FB_YTD),
    HR_9_SZN = last(HR_9_YTD),
    HR_FB_SZN = last(HR_FB_YTD)
  ) |>
  ungroup()


# running stat columns for career stats
all_logs<- all_logs |>
  group_by(PlayerName, playerid) |>
  mutate(
    career_cume_ER = cumsum(ER),
    career_cume_H = cumsum(H),
    career_cume_BB = cumsum(BB),
    career_cume_SO = cumsum(SO),
    career_cume_HR = cumsum(HR),
    career_cume_IP_outs = cumsum(outs_recorded),
    career_cume_IP = career_cume_IP_outs / 3,
    career_cume_TBF = cumsum(TBF),
    career_cume_GB = cumsum(GB),
    career_cume_FB = cumsum(FB),
    career_cume_bip = cumsum(bipCount),
    
    # Cumulative Rates
    career_SV_YTD = cumsum(SV),
    career_HLD_YTD = cumsum(HLD),
    career_BS_YTD = cumsum(BS),
    career_ERA_YTD = (career_cume_ER / career_cume_IP) * 9,
    career_WHIP_YTD = (career_cume_BB + career_cume_H) / career_cume_IP,
    career_K_pct_YTD = career_cume_SO / career_cume_TBF,
    career_BB_pct_YTD = career_cume_BB / career_cume_TBF,
    career_H_9_YTD = (career_cume_H / career_cume_IP) * 9,
    career_K_9_YTD = (career_cume_SO / career_cume_IP) * 9,
    career_BB_9_YTD = (career_cume_BB / career_cume_IP) * 9,
    career_K_BB_YTD = career_cume_SO / career_cume_BB,
    career_GB_pct_YTD = career_cume_GB / career_cume_bip,
    career_FB_pct_YTD = career_cume_FB / career_cume_bip,
    career_GB_FB_YTD = career_cume_GB / career_cume_FB,
    career_HR_9_YTD = (career_cume_HR / career_cume_IP) * 9,
    career_HR_FB_YTD = career_cume_HR / career_cume_FB
  ) |>
  ungroup()


#career stats up to end of that season
all_logs <- all_logs |>
  group_by(PlayerName, playerid, season) |>
  mutate(
    career_SV_SZN = last(career_SV_YTD),
    career_HLD_SZN = last(career_HLD_YTD),
    career_BS_SZN = last(career_BS_YTD),
    career_ERA_SZN = last(career_ERA_YTD),
    career_WHIP_SZN = last(career_WHIP_YTD),
    career_K_pct_SZN = last(career_K_pct_YTD),
    career_BB_pct_SZN = last(career_BB_pct_YTD),
    career_H_9_SZN = last(career_H_9_YTD),
    career_K_9_SZN = last(career_K_9_YTD),
    career_BB_9_SZN = last(career_BB_9_YTD),
    career_K_BB_SZN = last(career_K_BB_YTD),
    career_GB_pct_SZN = last(career_GB_pct_YTD),
    career_FB_pct_SZN = last(career_FB_pct_YTD),
    career_GB_FB_SZN = last(career_GB_FB_YTD),
    career_HR_9_SZN = last(career_HR_9_YTD),
    career_HR_FB_SZN = last(career_HR_FB_YTD)
  ) |>
  ungroup()


#overall career stats
all_logs <- all_logs |>
  group_by(PlayerName, playerid) |>
  mutate(
    career_SV = last(career_SV_YTD),
    career_HLD = last(career_HLD_YTD),
    career_BS = last(career_BS_YTD),
    career_ERA = last(career_ERA_YTD),
    career_WHIP = last(career_WHIP_YTD),
    career_K_pct = last(career_K_pct_YTD),
    career_BB_pct = last(career_BB_pct_YTD),
    career_H_9 = last(career_H_9_YTD),
    career_K_9 = last(career_K_9_YTD),
    career_BB_9 = last(career_BB_9_YTD),
    career_K_BB = last(career_K_BB_YTD),
    career_GB_pct = last(career_GB_pct_YTD),
    career_FB_pct = last(career_FB_pct_YTD),
    career_GB_FB = last(career_GB_FB_YTD),
    career_HR_9 = last(career_HR_9_YTD),
    career_HR_FB = last(career_HR_FB_YTD)
  ) |>
  ungroup()

#reduce pbp to only the date, pitcher, pitcherid
logs <- logs |>
  select(game_pk, game_date, about.inning, key_fangraphs, matchup.pitcher.id, matchup.pitcher.fullName) |>
  unique()

#remove name column just in case before join
all_logs <- all_logs |>
  select(-PlayerName) 

all_logs$gamedate <- as.Date(all_logs$gamedate)


#join logs to pbp outings
all_logs_outings <- left_join(logs, all_logs, by = c("game_date" = "gamedate",  
                                                     "key_fangraphs" = "playerid"))

all_logs_outings <- all_logs_outings |>
  mutate(Team = ifelse(Team == "OAK", "ATH", Team)) 

#remove extra row from double logs doubleheader
temp1 <- all_logs_outings |>
  count(game_pk, key_fangraphs, game_date, about.inning) |>
  filter(n > 1) |>
  select(key_fangraphs, game_date)

temp2 <- temp1 |> filter(key_fangraphs == "10430" | key_fangraphs == "18384")

temp3 <- temp1 |> filter(key_fangraphs != "10430" & key_fangraphs != "18384")

#remove temp3 rows
to_filter <- all_logs_outings |>
  semi_join(temp3, by = c("key_fangraphs", "game_date"))

first_rows_to_remove <- to_filter |>
  group_by(key_fangraphs, game_date) |>
  slice(1) |>  # first row per combo
  ungroup()

all_logs_outings <- anti_join(all_logs_outings, first_rows_to_remove, 
                              by = colnames(first_rows_to_remove))

#remove temp2 rows
all_logs_outings <- all_logs_outings |>
  mutate(row_id = row_number()) |>
  filter(row_id != min(row_id[key_fangraphs == 10430 & Date == as.Date("2022-07-16") & about.inning == 11])) |>
  select(-row_id)

all_logs_outings <- all_logs_outings |>
  mutate(row_id = row_number()) |>
  filter(row_id != max(row_id[key_fangraphs == 10430 & Date == as.Date("2022-07-16") & about.inning == 10])) |>
  select(-row_id)

# Tag row numbers globally
all_logs_outings <- all_logs_outings |>
  mutate(global_row = row_number())

# Get the row numbers to keep (2nd and 3rd rows of the specific group)
rows_to_keep <- all_logs_outings |>
  filter(key_fangraphs == 18384,
         Date == as.Date("2022-07-16"),
         about.inning == 8) |>
  slice(2:3) |>
  pull(global_row)

# Keep all rows except the ones you're specifically removing
all_logs_outings <- all_logs_outings |>
  filter(global_row %in% rows_to_keep | 
           !(key_fangraphs == 18384 &
               Date == as.Date("2022-07-16") &
               about.inning == 8)) |>
  select(-global_row)

write.csv(all_logs_outings, "all_logs_outings.csv", row.names = FALSE)


# Load trusted arms data
trusted_arms <- readRDS("trusted_arms.rds")

# make pbp$atBatIndex numeric
pbp$atBatIndex <- as.numeric(pbp$atBatIndex)

# Prepare latest inning info per pitcher appearance to determine final inning pitched
latest_inning_df <- all_logs_outings |>
  group_by(game_pk, matchup.pitcher.fullName) |>
  slice_max(order_by = about.inning, with_ties = FALSE) |>
  ungroup()

# Identify losing team per game
losing_teams <- latest_inning_df |>
  filter(L == 1) |>
  select(game_pk, losing_team = Team)

# Join losing team info back and assign team_result (Win/Loss) for each pitcher appearance
latest_inning_df <- latest_inning_df |>
  left_join(losing_teams, by = "game_pk") |>
  mutate(
    team_result = ifelse(Team == losing_team, "Loss", "Win")
  )

# Separate pitching appearances for winning teams (relievers only, excluding starters)
wins <- latest_inning_df |>
  filter(team_result == "Win") |>
  arrange(game_date, about.inning) |>
  filter(GS == 0)

# Separate pitching appearances for losing teams (relievers only)
loss <- latest_inning_df |>
  filter(team_result == "Loss") |>
  arrange(game_date, about.inning) |>
  filter(GS == 0)

# team-level trusted pitcher usage: focusing on each team's situation
# Separate winning team reliever appearances
teamtrustw <- wins |>
  select(game_pk, matchup.pitcher.id, key_fangraphs, matchup.pitcher.fullName, Team, season, HomeAway, team_result) |>
  group_by(game_pk, matchup.pitcher.id, key_fangraphs, matchup.pitcher.fullName, Team, season, HomeAway, team_result) |>
  summarise(n = n()) |>
  ungroup()

# Separate losing team reliever appearances
teamtrustl <- loss |>
  select(game_pk, matchup.pitcher.id, key_fangraphs, matchup.pitcher.fullName, Team, season, HomeAway, team_result) |>
  group_by(game_pk, matchup.pitcher.id, key_fangraphs, matchup.pitcher.fullName, Team, season, HomeAway, team_result) |>
  summarise(n = n()) |>
  ungroup()

# Combine team-level trusted pitcher appearances
teamtrusted <- bind_rows(teamtrustw, teamtrustl) 

# Join with trusted arms and filter by minimum pitches thrown (100)
teamtrusted <- left_join(trusted_arms, teamtrusted, by = c("game_pk", "playerid" = "matchup.pitcher.id", "season")) |>
  mutate(pitch_percent = ifelse(is.na(pitch_percent), 0, pitch_percent),
         pitches = ifelse(is.na(pitches), 0, pitches))

pitchers <- pitchers |> select(1:2) |> filter(!is.na(key_mlbam))

teamtrusted <- left_join(teamtrusted,
                         pitchers ,
                         by = c("playerid" = "key_mlbam"))

teamtrusted <- teamtrusted |>
  mutate(key_fangraphs.x = ifelse(is.na(key_fangraphs.x), key_fangraphs.y, key_fangraphs.x)) |>
  select(-key_fangraphs.y) 

teamtrusted <- teamtrusted |>
  rename(key_fangraphs = key_fangraphs.x)

teamtrusted <- left_join(teamtrusted,
                         all_manual_keys,
                         by = c("playerid" = "matchup.pitcher.id"))

teamtrusted <- teamtrusted |>
  mutate(key_fangraphs.x = ifelse(is.na(key_fangraphs.x), key_fangraphs.y, key_fangraphs.x)) |>
  select(-key_fangraphs.y) 

teamtrusted <- teamtrusted |>
  rename(key_fangraphs = key_fangraphs.x)

teamtrusted <- teamtrusted |>
  arrange(game_date, playerid) |>
  group_by(playerid, season) |>
  mutate(
    cum_pitches       = cumsum(pitches),
    cum_total_pitches = cumsum(total_pitches)
  ) |>
  ungroup()



# load dataframes
high_lev <- read_csv("Splits Leaderboard Data High Leverage.csv")
med_lev <- read_csv("Splits Leaderboard Data Medium Leverage.csv")
low_lev <- read_csv("Splits Leaderboard Data Low Leverage.csv")

# add a column to each before binding
low_lev <- low_lev |>
  mutate(lev = "low")

med_lev <- med_lev |>
  mutate(lev = "medium")

high_lev <- high_lev |>
  mutate(lev = "high")

# stack them
all_lev <- bind_rows(low_lev, med_lev, high_lev) |>
  mutate(
    season = as.numeric(substr(Date, nchar(Date) - 3, nchar(Date)))
  )

# calculate percentage of all leverage that are high leverage, or med+high
# first aggregate the data to ONE row per pitcher-date
daily_summary <- all_lev |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
  group_by(key_fangraphs, Date, season, Name, Team) |>
  summarise(
    total_tbf    = sum(TBF, na.rm = TRUE),
    high_tbf     = sum(ifelse(lev == "high", TBF, 0), na.rm = TRUE),
    medhigh_tbf  = sum(ifelse(lev %in% c("high","medium"), TBF, 0), na.rm = TRUE),
    .groups = "drop"
  )

lev_pct <- left_join(teamtrusted |> select(-player_name), 
                     daily_summary,  
                     by = c("key_fangraphs", "season", "game_date" = "Date")
) 

lev_pct <- lev_pct |>
  mutate(Team.y = ifelse(Team.y == "OAK", "ATH", Team.y))

lev_pct <- lev_pct[-15650, ]

lev_pct <- lev_pct |>
  select(-9,-10) |>
  rename(Team = Team.y)

# then do cumulative calculations
lev_pct <- lev_pct |>
  arrange(game_date, key_fangraphs) |>
  group_by(key_fangraphs, season, Name, Team) |>
  mutate(
    cum_total_tbf    = cumsum(total_tbf),
    cum_high_tbf     = cumsum(high_tbf),
    high_pct_ytd     = ifelse(cum_total_tbf > 0, cum_high_tbf / cum_total_tbf, NA_real_)
  ) |>
  ungroup()

#assigning trusted ranks

# define baseline minimum shrinkage
k <- 200

# get unique dates
tadates <- sort(unique(lev_pct$game_date))

ranked_list <- list()
checkpoint_file <- "progressive_ranks_checkpoint.rds"

if (file.exists(checkpoint_file)) {
  ranked_list <- readRDS(checkpoint_file)
  last_done <- max(sapply(ranked_list, function(x) max(x$Date)))
  remaining_dates <- tadates[tadates > last_done]
  message("Resuming from date ", last_done)
} else {
  remaining_dates <- tadates
  message("Starting fresh")
}

for (i in seq_along(remaining_dates)) {
  date_i <- remaining_dates[i]
  
  tmp <- lev_pct |>
    filter(game_date <= date_i, season == as.numeric(format(date_i, "%Y")))
  
  tmp_summary <- tmp |>
    group_by(Team, season, key_fangraphs) |> 
    summarise(
      cum_total_tbf     = last(cum_total_tbf),
      cum_pitches       = last(cum_pitches),
      cum_total_pitches = last(cum_total_pitches),
      high_pct_ytd      = last(high_pct_ytd),
      last_game_pk      = last(game_pk),
      .groups = "drop"
    ) |>
    mutate(
      pitch_weight = cum_pitches / (cum_total_pitches + k),
      season_pitch_share = cum_pitches / cum_total_pitches,
      final_weighted_high_score = (high_pct_ytd * .5) * pitch_weight * season_pitch_share
    ) |>
    group_by(Team, season) |>
    arrange(desc(final_weighted_high_score)) |>
    mutate(high_rank = row_number()) |>
    mutate(Date = date_i) |>  
    select(Team, season, key_fangraphs, last_game_pk, Date, high_rank)
  
  ranked_list[[length(ranked_list) + 1]] <- tmp_summary
  
  saveRDS(ranked_list, checkpoint_file)
  
  message(
    "Processed date ", i, " of ", length(remaining_dates),
    " (", date_i, ")"
  )
}


# final combine
progressive_ranks <- bind_rows(ranked_list) |> unique()

p_names <- lev_pct |> select(Name, key_fangraphs) |> unique()

progressive_ranks <- left_join(progressive_ranks, p_names) |>filter(!is.na(Name))

# remove relinquished players:
result <- readRDS("baseball_transactions.rds") 
result2 <- readRDS("baseball_injdem.rds") 

result2 <- result2[-19698, ]


result <- bind_rows(result, result2) |> unique()

result <- result |>
  select(-Notes) |>
  filter(
    !between(
      as.Date(Date),
      as.Date("2020-10-05"),
      as.Date("2021-03-14")
    )
  )

result_clean <- result |>
  mutate(
    Acquired = na_if(trimws(Acquired), ""),
    Relinquished = na_if(trimws(Relinquished), "")
  )

result_clean <- result_clean |>
  pivot_longer(
    cols = c(Acquired, Relinquished),
    names_to = "TransactionType",
    values_to = "Name"
  ) |>
  filter(!is.na(Name))  # only rows with an actual player

cleaned_ranks <- progressive_ranks |>
  left_join(
    result_clean,
    by = c("Date", "Team", "Name")
  ) |>
  mutate(
    Acquired_flag = TransactionType == "Acquired",
    Relinquished_flag = TransactionType == "Relinquished"
  ) |> 
  replace_na(list(Acquired_flag = FALSE, Relinquished_flag = FALSE)) |>
  select(-TransactionType)

cleaned_ranks <- cleaned_ranks |>
  arrange(Name, season, Date) |>
  group_by(Name, season, Team) |>
  mutate(
    # initialize a running delete status
    delete_flag = {
      flag <- FALSE
      out <- logical(length(Date))
      for (i in seq_along(Date)) {
        if (Relinquished_flag[i]) flag <- TRUE
        if (Acquired_flag[i]) flag <- FALSE
        out[i] <- flag
      }
      out
    }
  ) |>
  ungroup()

cleaned_ranks <- cleaned_ranks |>
  filter(!delete_flag) |>
  arrange(Team, season, Date, high_rank) |>
  group_by(Team, season, Date) |> 
  mutate(high_rank = row_number()) |> 
  ungroup() |>
  select(
    Team, season, key_fangraphs, last_game_pk, Date, high_rank, Name)


# join with lev_pct so only the necessary rankings rows are used
lev_pct <- lev_pct |> 
  rename(Date = game_date)

lev_pct <- left_join(lev_pct, 
                     cleaned_ranks)

lev_pct <- lev_pct |>
  mutate(ta_h = ifelse(high_rank == 1 | high_rank == 2, TRUE, FALSE))
        
lev_pct <- lev_pct |>
  filter(!is.na(ta_h)) |>
  rename(game_date = Date)


###
# team-level game summary: did the team use a trusted reliever in the game and what was the outcome?

#whats the game context (ie 8th/9th inning score) that leads to usage discrepancy
context <- pbp |>
  mutate(row_index = row_number()) |>  # preserve true order of appearance
  select(game_pk, game_date, about.inning, about.halfInning, count.outs.end, 
         matchup.pitcher.id, 
         matchup.pitcher.fullName, fielding_team, 
         result.awayScore, result.homeScore, row_index) |>
  unique()

# Get first and last appearance row for each pitcher in each game
pcontext <- context |>
  group_by(game_pk, matchup.pitcher.id) |>
  slice_min(order_by = row_index, n = 1) |>
  bind_rows(
    context |>
      group_by(game_pk, matchup.pitcher.id) |>
      slice_max(order_by = row_index, n = 1)
  ) |>
  arrange(game_date, row_index) |>
  ungroup()

icontext <- context |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(order_by = row_index, n = 1) |>
  bind_rows(
    context |>
      group_by(game_pk, about.inning, about.halfInning) |>
      slice_max(order_by = row_index, n = 1)
  ) |>
  ungroup() |>
  arrange(game_date, row_index)

context <- bind_rows(icontext, pcontext) |>
  arrange(game_date, row_index) |>
  unique() |>
  mutate(result.homeScore = ifelse(
    row_number() == max(row_number()[game_pk == 717170]),
    7,
    result.homeScore
  ))

# Add season + home/away
context <- context |>
  mutate(season = as.numeric(substr(game_date, 1, 4)),
         HomeAway = ifelse(about.halfInning == "top", "H", "A")) |>
  unique()

# score differential
# Create appearance_id
context <- context |>
  mutate(
    appearance_id = paste(game_pk, matchup.pitcher.id, about.inning, about.halfInning, sep = "_"),
    score_diff_entry = NA_real_
  )

# Loop to properly compute score_diff_entry (pre-PA score state)
games <- unique(context$game_pk)

for (g in games) {
  game_rows <- which(context$game_pk == g)
  
  last_home_score <- 0
  last_away_score <- 0
  prev_appearance <- NA
  
  for (i in seq_along(game_rows)) {
    row_idx <- game_rows[i]
    
    appearance <- context$appearance_id[row_idx]
    ha <- context$HomeAway[row_idx]
    home <- context$result.homeScore[row_idx]
    away <- context$result.awayScore[row_idx]
    
    # Only write score_diff_entry if it's the first row of the appearance
    if (i == 1 || appearance != prev_appearance) {
      score_diff <- if (ha == "H") last_home_score - last_away_score
      else last_away_score - last_home_score
      context$score_diff_entry[row_idx] <- score_diff
    }
    
    # Update for next iteration
    last_home_score <- home
    last_away_score <- away
    prev_appearance <- appearance
  }
}

# Compute score_diff_exit using post-PA scores from the *last row* in each appearance
context <- context |>
  group_by(appearance_id) |>
  mutate(
    score_diff_exit = if_else(
      row_number() == n(),
      if_else(
        HomeAway == "H",
        result.homeScore - result.awayScore,
        result.awayScore - result.homeScore
      ),
      NA_real_
    )
  ) |>
  ungroup()

# Collapse to one row per appearance — pulling score_diff_entry from recorded value
appearances <- context |>
  group_by(
    appearance_id, game_pk, matchup.pitcher.id, matchup.pitcher.fullName,
    fielding_team, about.inning, 
    about.halfInning, HomeAway, season
  ) |>
  summarize(
    score_diff_entry = first(na.omit(score_diff_entry)),
    score_diff_exit  = last(na.omit(score_diff_exit)),
    .groups = "drop"
  )

# Add game date and chronological sorting
appearance_meta <- context |>
  group_by(appearance_id) |>
  summarize(
    game_date = first(game_date),
    first_row_index = min(row_index),
    .groups = "drop"
  )

appearances <- appearances |>
  left_join(appearance_meta, by = "appearance_id") |>
  arrange(game_date, game_pk, first_row_index)


#add team result
final_scores2 <- context |>
  group_by(game_pk) |>
  slice_max(order_by = row_index, n = 1) |>  # last row per game
  select(game_pk, result.awayScore, result.homeScore) |>
  ungroup() |>
  mutate(winning_team = ifelse(result.awayScore > result.homeScore, "A", "H")) |>
  select(1,4)

appearances <- appearances |>
  left_join(final_scores2, by = "game_pk") |>
  mutate(
    team_result = case_when(
      winning_team == HomeAway ~ "Win",
      winning_team != HomeAway & winning_team %in% c("A", "H") ~ "Loss",
      TRUE ~ NA_character_  # for ties or missing data
    )
  ) |>
  select(-winning_team)

team_lookup <- tibble::tibble(
  Team_Full = c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", 
                "Boston Red Sox", "Chicago White Sox", "Chicago Cubs", 
                "Cincinnati Reds", "Cleveland Guardians", "Colorado Rockies", 
                "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
                "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", 
                "Milwaukee Brewers", "Minnesota Twins", "New York Yankees", 
                "New York Mets", "Oakland Athletics", "Philadelphia Phillies", 
                "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants", 
                "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", 
                "Texas Rangers", "Toronto Blue Jays", "Washington Nationals",
                "Athletics", "Cleveland Indians"),
  Team_Abbr = c("ARI", "ATL", "BAL", "BOS", "CHW", "CHC", "CIN", "CLE", "COL", 
                "DET", "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYY", 
                "NYM", "ATH", "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", 
                "TEX", "TOR", "WSN", "ATH", "CLE")
)

# Join and replace
appearances <- appearances |>
  left_join(team_lookup, by = c("fielding_team" = "Team_Full")) |>
  mutate(Team = Team_Abbr) |>
  select(-Team_Abbr)


seren <- left_join(appearances, lev_pct, by = c("game_pk", "game_date",
                                                "matchup.pitcher.id" = "playerid", 
                                                "HomeAway", "Team", "season",
                                                "team_result")) |>
  filter(!is.na(ta_h))

#Fix bad data
seren <- seren |> 
  mutate(HomeAway = ifelse(game_date == "2020-09-20" & Team == "SEA", "A", HomeAway),
         HomeAway = ifelse(game_date == "2020-09-20" & Team == "SDP", "H", HomeAway),
         HomeAway = ifelse(game_date == "2020-07-29" & Team == "TOR", "A", HomeAway),
         HomeAway = ifelse(game_date == "2020-07-29" & Team == "WSN", "H", HomeAway))


teamtrust_summ_h <- seren |>
  filter(about.inning >= 10) |>
  group_by(ta_h, HomeAway) |>
  summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(names_from = HomeAway, values_from = n, values_fill = 0) |>
  mutate(
    total = H + A,
    pct_away = A / total,
    pct_home = H / total
  )

teamtrust_summ_h <- seren |>
  filter(about.inning >= 10) |>
  group_by(ta_h, HomeAway) |>
  summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(names_from = HomeAway, values_from = n, values_fill = 0) |>
  mutate(
    total = H + A,
    pct_away = A / total,
    pct_home = H / total
  )

# Aggregate to weighted and unweighted average trusted usage by Home/Away
teamtrust_summ_h <- teamtrust_summ_h |>
  group_by(ta_h) |>
  summarise(unweighted_avg_pct_away = mean(pct_away, na.rm = TRUE),
            unweighted_avg_pct_home = mean(pct_home, na.rm = TRUE),
            weighted_avg_pct_away = weighted.mean(pct_away, total, na.rm = TRUE),
            weighted_avg_pct_home = weighted.mean(pct_home, total, na.rm = TRUE)
  )


# Summarize trusted usage by pitcher, Home/Away, trusted status, and team result (Win/Loss)
teamtrust_summ_h_wl <- seren |>
  filter(about.inning >= 10) |>
  arrange(matchup.pitcher.fullName, HomeAway) |>
  group_by(matchup.pitcher.fullName, HomeAway, ta_h, team_result) |>
  summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(names_from = HomeAway, values_from = n) |>
  mutate(
    H = coalesce(H, 0),
    A = coalesce(A, 0),
    total = H + A,
    pct_away = A / total,
    pct_home = H / total
  )

# Aggregate trusted usage with Win/Loss results to weighted and unweighted averages
teamtrust_summ_h_wl <- teamtrust_summ_h_wl |>
  group_by(ta_h, team_result) |>
  summarise(
    unweighted_avg_pct_away = mean(pct_away, na.rm = TRUE),
    unweighted_avg_pct_home = mean(pct_home, na.rm = TRUE),
    weighted_avg_pct_away = weighted.mean(pct_away, total, na.rm = TRUE),
    weighted_avg_pct_home = weighted.mean(pct_home, total, na.rm = TRUE),
    .groups = "drop"
  )


###

# Step 1: summarize Team-level trusted reliever usage specifically in extra innings (10th inning or later)
team_level <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, Team, HomeAway) |>
  summarise(
    used_trusted_in_extras = any(ta_h),
    team_result = first(team_result),
    .groups = "drop"
  )

trusted_rate <- team_level |>
  summarise(
    total_games = n(),
    games_with_trusted = sum(used_trusted_in_extras),
    pct_trusted_used = games_with_trusted / total_games
  )

trsuted_rate2 <- team_level |>
  group_by(HomeAway) |>
  summarise(
    total_games = n(),
    games_with_trusted = sum(used_trusted_in_extras),
    pct_trusted_used = games_with_trusted / total_games
  )

trusted_rate3 <- team_level |>
  group_by(team_result) |>
  summarise(
    total_games = n(),
    games_with_trusted = sum(used_trusted_in_extras),
    pct_trusted_used = games_with_trusted / total_games
  )

win_rate_by_trusted_use <- team_level |>
  group_by(used_trusted_in_extras) |>
  summarise(
    total_games = n(),
    wins = sum(team_result == "Win"),
    win_rate = wins / total_games,
    .groups = "drop"
  )


# Summarize trusted reliever usage and win % by HomeAway in extras
team_level_pct <- team_level |>
  group_by(HomeAway, used_trusted_in_extras) |>
  summarise(
    n_games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / n_games,
    .groups = "drop"
  )

#statistically signifcant
# Away team win outcomes
away_yes <- c(wins = 296, total = 535)
away_no  <- c(wins = 227, total = 501)

# Home team win outcomes
home_yes <- c(wins = 251, total = 470)
home_no  <- c(wins = 262, total = 566)

# Tests
away_test <- prop.test(
  x = c(away_yes["wins"], away_no["wins"]),
  n = c(away_yes["total"], away_no["total"]),
  alternative = "two.sided"
)

home_test <- prop.test(
  x = c(home_yes["wins"], home_no["wins"]),
  n = c(home_yes["total"], home_no["total"]),
  alternative = "two.sided"
)

away_test
home_test

team_level2 <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, high_rank, Team, HomeAway) |>
  summarise(
    used_trusted_in_extras = any(ta_h),
    team_result = first(team_result),
    .groups = "drop"
  )

team_level_pct2 <- team_level2 |>
  group_by(high_rank, HomeAway, used_trusted_in_extras) |>
  summarise(
    n_games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / n_games,
    .groups = "drop"
  ) |>
  filter(high_rank <= 2)


# ---- Phase 2 ----

# Identify multi-inning appearances:
pitcher_inning_level <- seren |>
  arrange(game_pk, Team, matchup.pitcher.fullName, about.inning) |>
  group_by(game_pk, Team, matchup.pitcher.fullName) |>
  mutate(
    innings_pitched = n_distinct(about.inning),
    multi_inning_flag = ifelse(innings_pitched > 1, 1, 0),
    first_inning = min(about.inning),
    entered_in_extras = ifelse(first_inning >= 10, 1, 0)
  ) |>
  ungroup()

# Identify trusted reliever multi-inning types
trusted_inning_spans <- pitcher_inning_level |>
  filter(ta_h == TRUE, multi_inning_flag == 1) |>
  group_by(game_pk, Team, matchup.pitcher.fullName) |>
  summarise(
    innings_pitched = list(sort(unique(about.inning))),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(
    multi_type = case_when(
      max(innings_pitched) < 10 ~ "pre_extra_multi_inning",
      any(innings_pitched < 10) & any(innings_pitched >= 10) ~ "split_multi_inning",  # e.g. 9+10
      min(innings_pitched) >= 10 ~ "extra_inning_multi",
      TRUE ~ NA_character_
    )
  ) |>
  ungroup()

# merge to the pitcher_inning_level
pitcher_inning_level <- pitcher_inning_level |>
  left_join(
    trusted_inning_spans |> select(game_pk, Team, matchup.pitcher.fullName, multi_type),
    by = c("game_pk", "Team", "matchup.pitcher.fullName")
  )

# Summarize team-game level
game_level_patterns <- pitcher_inning_level |>
  group_by(game_pk, Team, HomeAway, team_result) |>
  summarise(
    total_trusted = sum(ta_h == TRUE),
    trusted_extras = sum(ta_h == TRUE & entered_in_extras == 1),
    trusted_5th = sum(ta_h == TRUE & about.inning == 5),
    trusted_6th = sum(ta_h == TRUE & about.inning == 6),
    trusted_7th = sum(ta_h == TRUE & about.inning == 7),
    trusted_8th = sum(ta_h == TRUE & about.inning == 8),
    trusted_9th = sum(ta_h == TRUE & about.inning == 9),
    trusted_10th = sum(ta_h == TRUE & about.inning == 10),
    trusted_11_plus = sum(ta_h == TRUE & about.inning >= 11),
    trusted_before_8th = sum(ta_h == TRUE & about.inning < 8),
    # advanced pairs
    two_trusted_9_10 = sum(ta_h == TRUE & about.inning == 9) >= 1 & sum(ta_h == TRUE & about.inning == 10) >= 1,
    two_trusted_8_9 = sum(ta_h == TRUE & about.inning == 8) >= 1 & sum(ta_h == TRUE & about.inning == 9) >= 1 & trusted_extras == 0,
    trusted_8_and_10 = sum(ta_h == TRUE & about.inning == 8) >= 1 & sum(ta_h == TRUE & about.inning == 10) >= 1,
    trusted_8_and_11 = sum(ta_h == TRUE & about.inning == 8) >= 1 & sum(ta_h == TRUE & about.inning >= 11) >= 1,
    trusted_9_and_11 = sum(ta_h == TRUE & about.inning == 9) >= 1 & sum(ta_h == TRUE & about.inning >= 11) >= 1,
    two_trusted_8th = sum(ta_h == TRUE & about.inning == 8) >= 2,
    two_trusted_9th = sum(ta_h == TRUE & about.inning == 9) >= 2,
    
    # multi_inning summaries:
    any_pre_extra_multi = any(multi_type == "pre_extra_multi_inning"),
    any_split_multi = any(multi_type == "split_multi_inning"),
    any_extra_multi = any(multi_type == "extra_inning_multi"),
    .groups = "drop"
  ) |>
  mutate(
    strategy = case_when(
      total_trusted == 0 ~ "No trusted used",
      
      # prioritize BOTH
      any_pre_extra_multi & any_split_multi ~ "Pre-extra multi- AND split multi-inning",
      
      # then single ones
      trusted_extras == 0 & trusted_before_8th > 0 ~ "Trusted early, no trusted in extras",
      any_pre_extra_multi ~ "Pre-extra multi-inning",
      any_split_multi ~ "Split multi-inning (9/10)",
      any_extra_multi ~ "Extra-inning multi-inning",
      
      two_trusted_9_10 ~ "Two trusted in 9/10",
      two_trusted_8_9 ~ "Two trusted in 8/9 (no extras)",
      trusted_8_and_10 ~ "Trusted 8 and 10",
      trusted_8_and_11 ~ "Trusted 8 and 11+",
      trusted_9_and_11 ~ "Trusted 9 and 11+",
      two_trusted_8th ~ "Two trusted in 8th",
      two_trusted_9th ~ "Two trusted in 9th",
      
      trusted_extras >= 1 & trusted_8th == 0 & trusted_9th == 0 ~ "Trusted saved for extras",
      total_trusted == 1 & trusted_8th >= 1 ~ "One trusted for 8th",
      total_trusted == 1 & trusted_9th >= 1 ~ "One trusted for 9th",
      
      TRUE ~ "Other"
    )
    
  )

# strategy_success fully overwritten
strategy_success <- game_level_patterns |>
  group_by(strategy, HomeAway) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  )


# . Which patterns correlate with winning?
strat_table <- strategy_success |> arrange(desc(win_pct))

strat_table_filt <- strat_table |> filter(games >= 100)

# First classify these two strategies
game_level_patterns_trusted10plus <- game_level_patterns |>
  mutate(
    tenth_or_later_strategy = case_when(
      trusted_10th >= 1 | trusted_11_plus >= 1 ~ "Trusted used in 10th or later",
      trusted_10th == 0 & trusted_11_plus == 0 ~ "No trusted used in extras",
      TRUE ~ "Other"
    )
  )

# Summarize win rates by these two categories
strategy_success_10plus <- game_level_patterns_trusted10plus |>
  filter(tenth_or_later_strategy %in% c(
    "Trusted used in 10th or later", "No trusted used in extras"
  )) |>
  group_by(tenth_or_later_strategy, HomeAway) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  )

# Show it
strat_table2 <- strategy_success_10plus |> arrange(desc(win_pct))


# ---- Phase 3 ----

# What is the single-most used strategy for each team, and their win pct

# Calculate overall extra innings record for each team
team_overall_record <- game_level_patterns |>
  group_by(Team) |>
  summarise(
    total_games = n(),
    total_wins = sum(team_result == "Win"),
    overall_win_pct = total_wins / total_games,
    .groups = "drop"
  ) |>
  select(Team, overall_win_pct)

# calculate strategy counts per team
team_strategy_counts <- game_level_patterns |>
  group_by(Team, strategy) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  )

# get the single most-used strategy for each team
team_most_used_strategy <- team_strategy_counts |>
  group_by(Team) |>
  slice_max(order_by = games, n = 1, with_ties = FALSE) |>
  ungroup()

# Join overall record to most used strategy dataframe
team_most_used_strategy <- team_most_used_strategy |>
  left_join(team_overall_record, by = "Team")

# show it
team_strat_table <- team_most_used_strategy |> arrange(desc(win_pct))


#  what their top 2 and bottom 2 highest win pct strategies are
# summarize win pct for each team + strategy
team_strategy_summary <- game_level_patterns |>
  group_by(Team, strategy) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  )

# for each team, get their top 2 highest win pct strategies
team_top2 <- team_strategy_summary |>
  group_by(Team) |>
  arrange(desc(win_pct)) |>
  slice_head(n = 2) |>
  ungroup()

# for each team, get their bottom 2 lowest win pct strategies
team_bottom2 <- team_strategy_summary |>
  group_by(Team) |>
  arrange(win_pct) |>
  slice_head(n = 2) |>
  ungroup()

# Join overall record to top 2 strategies
team_top2 <- team_top2 |>
  left_join(team_overall_record, by = "Team")

# Join overall record to bottom 2 strategies
team_bottom2 <- team_bottom2 |>
  left_join(team_overall_record, by = "Team")

# show them
team_strat_table_wl <- bind_rows(team_top2, team_bottom2) |> 
  arrange(Team, desc(win_pct)) |>
  unique()

print(team_top2 |> arrange(Team, desc(win_pct)))
print(team_bottom2 |> arrange(Team, win_pct))

# for each team, get their top 2 highest win pct strategies, min 5
team_top2_5 <- team_strategy_summary |>
  filter(games >= 10) |>
  group_by(Team) |>
  arrange(desc(win_pct)) |>
  slice_head(n = 2) |>
  ungroup()

# for each team, get their bottom 2 lowest win pct strategies
team_bottom2_5 <- team_strategy_summary |>
  filter(games >= 10) |>
  group_by(Team) |>
  arrange(win_pct) |>
  slice_head(n = 2) |>
  ungroup()

# Join overall record to top 2 strategies
team_top2_5 <- team_top2_5 |>
  left_join(team_overall_record, by = "Team")

# Join overall record to bottom 2 strategies
team_bottom2_5 <- team_bottom2_5 |>
  left_join(team_overall_record, by = "Team")

# show them
team_strat_table_wl_5 <- bind_rows(team_top2_5, team_bottom2_5) |> 
  arrange(Team, desc(win_pct)) |>
  unique()


# ---- Phase 4 ----

#Score Contexts
# Add your extras-trusted classification
game_level_patterns <- game_level_patterns |>
  mutate(
    trusted_in_extras_flag = trusted_10th >= 1 | trusted_11_plus >= 1
  )

# Merge back to pitcher_inning_level so you can look at inning-by-inning states
seren_trusted_flag <- pitcher_inning_level |>
  left_join(
    game_level_patterns |> select(game_pk, Team, trusted_in_extras_flag),
    by = c("game_pk", "Team")
  )

# keep only the first pitcher in each half-inning for each game/team
first_pitcher_per_halfinning <- seren_trusted_flag |>
  filter(about.inning %in% c(8,9)) |>
  group_by(game_pk, Team, about.inning, about.halfInning) |>
  slice_min(first_row_index, n = 1, with_ties = FALSE) |>
  ungroup()

# then summarize those score diffs
score_diff_summary <- first_pitcher_per_halfinning |>
  group_by(trusted_in_extras_flag, about.inning) |>
  summarise(
    mean_score_diff = mean(score_diff_entry, na.rm = TRUE),
    median_score_diff = median(score_diff_entry, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

score_diff_summaryHA <- first_pitcher_per_halfinning |>
  group_by(trusted_in_extras_flag, about.inning, about.halfInning) |>
  summarise(
    mean_score_diff = mean(score_diff_entry, na.rm = TRUE),
    median_score_diff = median(score_diff_entry, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

score_diff_freqHA <- first_pitcher_per_halfinning |>
  group_by(about.inning, about.halfInning, score_diff_entry) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  arrange(about.inning, about.halfInning, score_diff_entry)

score_diff_bottom_9 <- first_pitcher_per_halfinning |>
  filter(about.inning == 9, about.halfInning == "bottom") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry)

score_diff_top_9 <- first_pitcher_per_halfinning |>
  filter(about.inning == 9, about.halfInning == "top") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry)

score_diff_bottom_8 <- first_pitcher_per_halfinning |>
  filter(about.inning == 8, about.halfInning == "bottom") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry)

score_diff_top_8 <- first_pitcher_per_halfinning |>
  filter(about.inning == 8, about.halfInning == "top") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry)

#tied games
score_diff_bottom_90 <- first_pitcher_per_halfinning |>
  filter(about.inning == 9, about.halfInning == "bottom") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry) |> filter(score_diff_entry == 0)

score_diff_top_90 <- first_pitcher_per_halfinning |>
  filter(about.inning == 9, about.halfInning == "top") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry) |> filter(score_diff_entry == 0)

score_diff_bottom_8 <- first_pitcher_per_halfinning |>
  filter(about.inning == 8, about.halfInning == "bottom") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry)

score_diff_top_8 <- first_pitcher_per_halfinning |>
  filter(about.inning == 8, about.halfInning == "top") |>
  group_by(ta_h, trusted_in_extras_flag, score_diff_entry) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = n / sum(n)) |>
  arrange(score_diff_entry)

lead_lost <- seren |>
  filter(
    about.inning %in% c(8, 9),
    about.halfInning == "top",     # home team pitching
    ta_h == TRUE,                  # trusted reliever
    HomeAway == "H"
  ) |>
  group_by(game_pk, about.inning) |>
  summarise(
    first_score_diff = score_diff_entry[which.min(first_row_index)],
    last_score_diff  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  ) |>
  mutate(blown_lead = first_score_diff > 0 & last_score_diff <= 0)

# Count how often lead was lost
blown_summary <- lead_lost |>
  summarise(
    total_frames = n(),
    blown_leads = sum(blown_lead),
    pct_blown = blown_leads / total_frames
  )
  

# first pitcher per half-inning
trusted_counts_by_inning <- seren_trusted_flag |>
  filter(about.inning %in% c(8,9)) |>
  group_by(game_pk, Team, about.inning, about.halfInning) |>
  slice_min(first_row_index, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    trusted_8th = ifelse(ta_h == TRUE & about.inning == 8, 1, 0),
    trusted_9th = ifelse(ta_h == TRUE & about.inning == 9, 1, 0)
  ) |>
  group_by(game_pk, Team, trusted_in_extras_flag) |>
  summarise(
    trusted_8th_total = sum(trusted_8th),
    trusted_9th_total = sum(trusted_9th),
    trusted_total_8_9 = sum(trusted_8th + trusted_9th),
    .groups = "drop"
  )

# summarise by extras flag
trusted_usage_summary_by_inning <- trusted_counts_by_inning |>
  group_by(trusted_in_extras_flag) |>
  summarise(
    avg_trusted_8th = mean(trusted_8th_total),
    median_trusted_8th = median(trusted_8th_total),
    avg_trusted_9th = mean(trusted_9th_total),
    median_trusted_9th = median(trusted_9th_total),
    avg_trusted_total_8_9 = mean(trusted_total_8_9),
    median_trusted_total_8_9 = median(trusted_total_8_9),
    .groups = "drop"
  )


# summarise by extras flag and HomeAway
trusted_usage_summary_by_inning_homeaway <- trusted_counts_by_inning |>
  group_by(trusted_in_extras_flag, HomeAway) |>
  summarise(
    avg_trusted_8th = mean(trusted_8th_total),
    median_trusted_8th = median(trusted_8th_total),
    avg_trusted_9th = mean(trusted_9th_total),
    median_trusted_9th = median(trusted_9th_total),
    avg_trusted_total_8_9 = mean(trusted_total_8_9),
    median_trusted_total_8_9 = median(trusted_total_8_9),
    n_games = n(),
    .groups = "drop"
  )

trusted_unique_pitchers <- seren_trusted_flag |>
  filter(about.inning %in% c(8,9)) |>
  group_by(game_pk, Team, matchup.pitcher.fullName) |>   # pitcher level
  summarise(
    first_inning = min(about.inning),
    trusted = any(ta_h == TRUE),
    HomeAway = first(HomeAway),
    trusted_in_extras_flag = first(trusted_in_extras_flag),
    .groups = "drop"
  ) |>
  group_by(game_pk, Team, trusted_in_extras_flag, HomeAway) |>
  summarise(
    n_unique_trusted_pitchers = sum(trusted),
    .groups = "drop"
  )

# summarise by trusted extras flag and home/away
trusted_unique_summary <- trusted_unique_pitchers |>
  group_by(trusted_in_extras_flag, HomeAway) |>
  summarise(
    avg_unique_trusted_8_9 = mean(n_unique_trusted_pitchers),
    median_unique_trusted_8_9 = median(n_unique_trusted_pitchers),
    sd_unique_trusted_8_9 = sd(n_unique_trusted_pitchers),
    .groups = "drop"
  )

multi_inning_8_9 <- seren_trusted_flag |>
  filter(about.inning %in% c(8,9)) |>
  group_by(game_pk, Team, matchup.pitcher.fullName) |>
  summarise(
    innings_worked = n_distinct(about.inning),
    is_trusted = any(ta_h == TRUE),
    trusted_in_extras_flag = first(trusted_in_extras_flag),
    HomeAway = first(HomeAway),
    .groups = "drop"
  ) |>
  mutate(
    multi_inning = ifelse(innings_worked > 1, 1, 0)
  )

# summarize
multi_inning_rate <- multi_inning_8_9 |>
  group_by(trusted_in_extras_flag, is_trusted) |>
  summarise(
    total_pitchers = n(),
    n_multi_inning = sum(multi_inning),
    multi_inning_rate = n_multi_inning / total_pitchers,
    .groups = "drop"
  )

# Step 1: Flag games with trusted reliever in extras (>= 10th inning)
trusted_in_extras_games <- pitcher_inning_level |>
  filter(about.inning >= 10 & ta_h == TRUE) |>
  distinct(game_pk, Team) |>
  mutate(trusted_in_extras_flag = TRUE)

# Step 2: Identify pitchers who pitched both 8th and 9th innings in each game
pitchers_8_9 <- pitcher_inning_level |>
  filter(about.inning %in% c(8, 9)) |>
  group_by(game_pk, Team, matchup.pitcher.fullName) |>
  summarise(
    innings_pitched_8_9 = n_distinct(about.inning),
    trusted_pitcher = any(ta_h == TRUE),
    .groups = "drop"
  ) |>
  filter(innings_pitched_8_9 == 2)  # only pitchers who pitched BOTH innings

# Step 3: For each game, determine if there was:
# - a trusted multi-inning pitcher in 8/9
# - a nontrusted multi-inning pitcher in 8/9
# - no multi-inning pitcher in 8/9

multi_inning_status <- pitchers_8_9 |>
  group_by(game_pk, Team) |>
  summarise(
    trusted_multi_8_9 = any(trusted_pitcher),
    nontrusted_multi_8_9 = any(!trusted_pitcher),
    .groups = "drop"
  )

# Step 4: Combine everything into game-level dataframe

game_summary <- pitcher_inning_level |>
  distinct(game_pk, Team, team_result) |>
  left_join(trusted_in_extras_games, by = c("game_pk", "Team")) |>
  mutate(trusted_in_extras_flag = ifelse(is.na(trusted_in_extras_flag), FALSE, trusted_in_extras_flag)) |>
  left_join(multi_inning_status, by = c("game_pk", "Team")) |>
  mutate(
    trusted_multi_8_9 = ifelse(is.na(trusted_multi_8_9), FALSE, trusted_multi_8_9),
    nontrusted_multi_8_9 = ifelse(is.na(nontrusted_multi_8_9), FALSE, nontrusted_multi_8_9),
    multi_inning_strategy = case_when(
      trusted_in_extras_flag & trusted_multi_8_9 ~ "Trusted in extras + trusted multi-inning 8/9",
      trusted_in_extras_flag & nontrusted_multi_8_9 ~ "Trusted in extras + nontrusted multi-inning 8/9",
      trusted_in_extras_flag & !trusted_multi_8_9 & !nontrusted_multi_8_9 ~ "Trusted in extras + no multi-inning 8/9",
      !trusted_in_extras_flag & trusted_multi_8_9 ~ "No trusted in extras + trusted multi-inning 8/9",
      !trusted_in_extras_flag & nontrusted_multi_8_9 ~ "No trusted in extras + nontrusted multi-inning 8/9",
      TRUE ~ "No trusted in extras + no multi-inning 8/9"
    )
  )

# Step 5: Summarize frequencies and win percentages

strategy_summary <- game_summary |>
  group_by(multi_inning_strategy) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  ) |>
  arrange(desc(win_pct))

# Create game-level summary with HomeAway
game_summary_home_away <- pitcher_inning_level |>
  distinct(game_pk, Team, HomeAway, team_result) |>
  left_join(trusted_in_extras_games, by = c("game_pk", "Team")) |>
  mutate(trusted_in_extras_flag = ifelse(is.na(trusted_in_extras_flag), FALSE, trusted_in_extras_flag)) |>
  left_join(multi_inning_status, by = c("game_pk", "Team")) |>
  mutate(
    trusted_multi_8_9 = ifelse(is.na(trusted_multi_8_9), FALSE, trusted_multi_8_9),
    nontrusted_multi_8_9 = ifelse(is.na(nontrusted_multi_8_9), FALSE, nontrusted_multi_8_9),
    multi_inning_strategy = case_when(
      trusted_in_extras_flag & trusted_multi_8_9 ~ "Trusted in extras + trusted multi-inning 8/9",
      trusted_in_extras_flag & nontrusted_multi_8_9 ~ "Trusted in extras + nontrusted multi-inning 8/9",
      trusted_in_extras_flag & !trusted_multi_8_9 & !nontrusted_multi_8_9 ~ "Trusted in extras + no multi-inning 8/9",
      !trusted_in_extras_flag & trusted_multi_8_9 ~ "No trusted in extras + trusted multi-inning 8/9",
      !trusted_in_extras_flag & nontrusted_multi_8_9 ~ "No trusted in extras + nontrusted multi-inning 8/9",
      TRUE ~ "No trusted in extras + no multi-inning 8/9"
    )
  )

# Summarize by strategy and HomeAway
strategy_summary_HA <- game_summary_home_away |>
  group_by(multi_inning_strategy, HomeAway) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  ) |>
  arrange(desc(win_pct))


first_extras_pitcher <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, Team) |>
  slice_min(order_by = about.inning, n = 1, with_ties = FALSE) |>
  select(game_pk, Team, high_rank, HomeAway, team_result) |>
  distinct()


ranked_summary <- first_extras_pitcher |>
  group_by(high_rank, HomeAway) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  ) |>
  arrange(desc(win_pct))

ranked_summary2 <- first_extras_pitcher |>
  group_by(high_rank, HomeAway) |>
  summarise(
    games = n(),
    wins = sum(team_result == "Win"),
    win_pct = wins / games,
    .groups = "drop"
  ) |>
  mutate(HomeAway = factor(HomeAway, levels = c("H", "A"))) |>
  arrange(desc(HomeAway)) |>
  filter(games >= 100)

ggplot(ranked_summary2, aes(x = factor(high_rank), y = win_pct, fill = HomeAway)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Win % above the bar
  geom_text(
    aes(label = scales::percent(win_pct, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 3
  ) +
  
  # Games inside the bar
  geom_text(
    aes(label = games),
    position = position_dodge(width = 0.7),
    vjust = 1.5,
    color = "white",
    size = 3
  ) +
  
  scale_fill_manual(
    values = c("H" = "red3", "A" = "darkblue"),
    labels = c("Home", "Away")
  ) +
  scale_y_continuous(
    limits = c(0, 0.65),
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Win Percentage by Reliever Rank and Location",
    x = "Reliever Rank",
    y = "Win Percentage",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 5)),
    plot.margin = margin(t = 5, r = 10, b = 10, l = 10),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.box.background = element_blank(),
    legend.key = element_blank()
  )

# rank1/2 relievers are not leaking into rank 4
first_extras_pitcher4 <- seren |>
  filter(about.inning >= 10, high_rank == 4) |>
  group_by(game_pk, Team) |>
  slice_min(order_by = about.inning, n = 1, with_ties = FALSE) |>
  group_by(about.inning, HomeAway) |>
  summarise(n = n())

# ---- Phase 5 ----

# Specifically look at bottom 10th inning tied, and down 1
# Why are home teams struggling to score
# what type of pitcher do they face when they win or lose (ie trusted or not)
# what is the result of the first AB, win and lose
# does the K vs in play type of pitcher dictate first AB result
# does the K vs in play type of pitcher dictate win or loss
# how often does home team score in both situations
# does any team ever try to squeeze the runner home

#identify games that ended in the 10th
ended10 <- seren |>
  group_by(game_pk) |>
  slice_max(order_by = first_row_index, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(about.inning == 10, about.halfInning == "bottom")

# games that ended in 10th
ended10_df <- seren |>
  filter(game_pk %in% ended10$game_pk) 

#identify games that ended in the 11th
ended11 <- seren |>
  group_by(game_pk) |>
  slice_max(order_by = first_row_index, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(about.inning == 11, about.halfInning == "bottom")

# games that ended in 11th
ended11_df <- seren |>
  filter(game_pk %in% ended11$game_pk)

# runs given up by trusted and non trusted pitchers
runs_10th_summary <- ended10_df |>
  filter(about.inning == 10) |>
  mutate(
    runs_allowed = ifelse(score_diff_exit - score_diff_entry < 0, 
                          abs(score_diff_exit - score_diff_entry), 
                          0)
  ) |>
  group_by(HomeAway, ta_h) |>
  summarise(
    total_runs_allowed = sum(runs_allowed, na.rm = TRUE),
    appearances = n(),
    .groups = "drop"
  )

# runs given up by 1/2/3/non
runs_10th_summary2 <- ended10_df |>
  filter(about.inning == 10) |>
  mutate(
    runs_allowed = ifelse(score_diff_exit - score_diff_entry < 0, 
                          abs(score_diff_exit - score_diff_entry), 
                          0),
    rank_type = case_when(
      high_rank == 1 ~ "rank1",
      high_rank == 2 ~ "rank2",
      high_rank == 3 ~ "rank3",
      TRUE ~ "other"
    )
  ) |>
  group_by(HomeAway, rank_type) |>
  summarise(
    total_runs_allowed = sum(runs_allowed, na.rm = TRUE),
    appearances = n(),
    .groups = "drop"
  )

# look at same as above, but in games that were tied or 1-run away lead
close_games_b10 <- seren |>
  filter(about.inning == 10, about.halfInning == "bottom") |>
  group_by(game_pk) |>
  slice_max(order_by = first_row_index, n = 1, with_ties = FALSE) |>  # last pitch top 10th
  filter(score_diff_exit %in% c(0, -1)) |>
  pull(game_pk)

# subset to those entire games
close_b10_df <- seren |> 
  filter(game_pk %in% close_games_b10) |>
  filter(about.inning >= 10)

close_b10_summary <- close_b10_df |>
  filter(about.inning == 10) |>
  mutate(
    runs_allowed = ifelse(score_diff_exit - score_diff_entry < 0, 
                          abs(score_diff_exit - score_diff_entry), 
                          0),
    rank_type = case_when(
      high_rank == 1 ~ "rank1",
      high_rank == 2 ~ "rank2",
      high_rank == 3 ~ "rank3",
      TRUE ~ "other"
    )
  ) |>
  group_by(HomeAway, rank_type) |>
  summarise(
    total_runs_allowed = sum(runs_allowed, na.rm = TRUE),
    appearances = n(),
    .groups = "drop"
  )

# identify candidate innings with tied or down1 situations
candidate_innings <- seren |> 
  filter(
    about.inning >= 10,
    about.halfInning == "bottom",
    score_diff_entry %in% c(0, 1)
  ) |>
  mutate(
    situation_type = case_when(
      score_diff_entry == 0 ~ "tied",
      score_diff_entry == 1 ~ "down1"
    ),
    rank_type = case_when(
      high_rank == 1 ~ "rank1",
      high_rank == 2 ~ "rank2",
      high_rank == 3 ~ "rank3",
      TRUE ~ "other"
    )
  )

# add a situation_type "combined" as well
candidate_innings_combined <- candidate_innings |> 
  mutate(situation_type = "combined")

# bind back the two
candidate_innings_all <- bind_rows(candidate_innings, candidate_innings_combined)

# first pitcher for each half-inning
first_pitcher_per_halfinning <- candidate_innings_all |>
  group_by(game_pk, about.inning, situation_type) |>
  slice_min(order_by = first_row_index, n = 1, with_ties = FALSE) |>
  select(game_pk, about.inning, situation_type, ta_h, matchup.pitcher.fullName, HomeAway) |>
  ungroup()

# runs scored in the half-inning
runs_scored_in_halfinning <- candidate_innings_all |>
  group_by(game_pk, about.inning, situation_type) |>
  summarise(
    runs_scored = first(score_diff_entry) - last(score_diff_exit),
    .groups = "drop"
  )

# join them
bottom_half_summary <- first_pitcher_per_halfinning |>
  left_join(runs_scored_in_halfinning, by = c("game_pk", "about.inning", "situation_type")) |>
  mutate(
    runs_bucket = case_when(
      runs_scored == 0 ~ "0 runs",
      runs_scored == 1 ~ "1 run",
      runs_scored >= 2 ~ "2+ runs",
      TRUE ~ NA_character_
    )
  )

# summarise
run_bucket_summary <- bottom_half_summary |>
  group_by(situation_type, ta_h, runs_bucket) |>
  summarise(
    n_innings = n(),
    .groups = "drop"
  ) |>
  group_by(situation_type, ta_h) |>
  mutate(
    pct = n_innings / sum(n_innings)
  )


# top half innings where score_diff_entry is 0
candidate_inningstop <- seren |> 
  filter(
    about.inning >= 10,
    about.halfInning == "top"
  ) |>
  mutate(rank_type = case_when(
    high_rank == 1 ~ "rank1",
    high_rank == 2 ~ "rank2",
    high_rank == 3 ~ "rank3",
    TRUE ~ "other"
  ))

first_pitcher_per_halfinningtop <- candidate_inningstop |>
  group_by(game_pk, about.inning) |>
  slice_min(order_by = first_row_index, n = 1, with_ties = FALSE) |>
  select(game_pk, about.inning, ta_h, matchup.pitcher.fullName, HomeAway) |>
  ungroup()

runs_scored_in_halfinningtop <- candidate_inningstop |>
  group_by(game_pk, about.inning) |>
  summarise(
    runs_scored = first(score_diff_entry) - last(score_diff_exit),
    .groups = "drop"
  )

top_half_summary <- first_pitcher_per_halfinningtop |>
  left_join(runs_scored_in_halfinningtop, by = c("game_pk", "about.inning")) |>
  mutate(
    runs_bucket = case_when(
      runs_scored == 0 ~ "0 runs",
      runs_scored == 1 ~ "1 run",
      runs_scored >= 2 ~ "2+ runs",
      TRUE ~ NA_character_
    )
  )

run_bucket_summarytop <- top_half_summary |>
  group_by(ta_h, runs_bucket) |>
  summarise(
    n_innings = n(),
    .groups = "drop"
  ) |>
  group_by(ta_h) |>
  mutate(
    pct = n_innings / sum(n_innings)
  )


# Bunt results

# first identify the atBatIndex of the first AB in each relevant half-inning
first_ab_indices <- pbp |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning) |>
  summarise(first_ab = min(about.atBatIndex), .groups = "drop")

# then filter pbp to keep all rows matching that first AB
first_ab_allrows <- pbp |>
  inner_join(first_ab_indices, 
             by = c("game_pk", "about.inning")) |>
  filter(atBatIndex == first_ab) |>
  arrange(game_date, game_pk, atBatIndex)

# flag each row as having a bunt keyword
first_ab_allrows <- first_ab_allrows |>
  mutate(
    bunt_flag = grepl("bunt", result.description, ignore.case = TRUE) |
      grepl("bunt", result.event, ignore.case = TRUE) |
      grepl("bunt", details.description, ignore.case = TRUE)
  )

# summarize across each AB whether *any* row had a bunt
buntattempts <- first_ab_allrows |>
  group_by(game_pk, about.inning, atBatIndex) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

ba_ratio <- buntattempts |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

# get the first pitcher of each half-inning in extras
seren_first_p <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(first_row_index, with_ties = FALSE) |>
  select(game_pk, about.inning, about.halfInning, matchup.pitcher.id,
         score_diff_entry, score_diff_exit)

# then join this to the first_ab_allrows (which is one AB per half-inning)
first_ab_allrows <- first_ab_allrows |>
  left_join(
    seren_first_p,
    by = c("game_pk", "about.inning", "about.halfInning", "matchup.pitcher.id")
  ) |> 
  arrange(game_date, atBatIndex)


buntattempts_close <- first_ab_allrows |>
  filter(score_diff_entry == 1 | score_diff_entry == 0) |>
  group_by(game_pk, about.inning, atBatIndex) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

bac_ratio <- buntattempts_close |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

buntattempts_zero <- first_ab_allrows |>
  filter(score_diff_entry == 0) |>
  group_by(game_pk, about.inning, atBatIndex) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

baz_ratio <- buntattempts_zero |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

buntattempts_one <- first_ab_allrows |>
  filter(score_diff_entry == 1) |>
  group_by(game_pk, about.inning, atBatIndex) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

bao_ratio <- buntattempts_one |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

# get last row of each AB in extras
last_ab_rows <- first_ab_allrows |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup()

# then filter and classify bunts only on the final AB rows
bunt_success <- last_ab_rows |>
  filter(bunt_flag == TRUE) |>
  filter(grepl("bunt", result.description, ignore.case = TRUE)) |>
  mutate(
    num_bases_occupied_post = rowSums(
      across(
        c(matchup.postOnFirst.id, matchup.postOnSecond.id, matchup.postOnThird.id),
        ~ !is.na(.x)
      )
    ),
    success_flag = case_when(
      !is.na(matchup.postOnThird.id) ~ TRUE,  # runner advanced to 3rd
      num_bases_occupied_post >= 2 ~ TRUE,    # at least two runners on base
      grepl("scores", result.description, ignore.case = TRUE) ~ TRUE,  # scored
      TRUE ~ FALSE
    )
  ) |>
  mutate(success_flag = ifelse(result.event == "Bunt Pop Out", FALSE, success_flag),
         success_flag = ifelse(result.event == "Strikeout", FALSE, success_flag),
         matchup.postOnFirst.id = ifelse(result.event == "Strikeout", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(result.event == "Bunt Pop Out", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(
           str_detect(result.description, "out on a sacrifice bunt"),
           NA,
           matchup.postOnFirst.id
         ),
         matchup.postOnSecond.id = ifelse(
           str_detect(result.description, "Andres Gimenez out on a sacrifice bunt, third baseman Alex Bregman to first baseman Jose Abreu. Tyler Freeman to 3rd."),
           NA,
           matchup.postOnSecond.id
         )
  )

bunt_success_summary <- bunt_success |>
  summarise(
    total_bunts = n(),
    successful_bunts = sum(success_flag == TRUE, na.rm = TRUE),
    success_rate = successful_bunts / total_bunts
  )

bunt_event_summary <- bunt_success |>
  mutate(
    bases_start = case_when(
      !is.na(matchup.postOnFirst.id) & !is.na(matchup.postOnThird.id) ~ "1st and 3rd",
      !is.na(matchup.postOnFirst.id) ~ "1st",
      !is.na(matchup.postOnSecond.id) & !is.na(matchup.postOnThird.id) ~ "2nd and 3rd",
      !is.na(matchup.postOnSecond.id) ~ "2nd",
      !is.na(matchup.postOnThird.id) ~ "3rd",
      TRUE ~ "empty"
    )
  ) |>
  group_by(bases_start, result.event, count.outs.end) |>
  summarise(
    instances = n(),
    successful_bunts = sum(success_flag, na.rm = TRUE),
    success_rate = successful_bunts / instances * 100,
    .groups = "drop"
  ) |>
  arrange(desc(instances))

# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_0games <- bunt_success |>
  filter(score_diff_entry == 0)

# 2. all first ABs in tied games
firstabs_tied <- first_ab_allrows |>
  filter(score_diff_entry == 0) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_0games <- firstabs_tied |>
  anti_join(
    bunt_attempts_0games |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exit <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, about.halfInning) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_0games <- bunt_attempts_0games |>
  left_join(seren_exit, 
            by = c("game_pk", "about.inning", "about.halfInning"))

nobunt_attempts_0games <- nobunt_attempts_0games |>
  left_join(seren_exit,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_0games <- bunt_attempts_0games |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored
  )

nobunt_attempts_0games <- nobunt_attempts_0games |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summary <- bind_rows(bunt_attempts_0games, nobunt_attempts_0games) |>
  group_by(attempt_category) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )

# look at no-bunt innings

# Get all first ABs with no bunt attempts by anti-joining bunt attempts
nobunt_attempts_0games_keys <- nobunt_attempts_0games 

#  Join back to first_ab_allrows to get all rows of those no-bunt first ABs
nobunt_attempts_0games_df <- first_ab_allrows |>
  semi_join(nobunt_attempts_0games_keys, 
            by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex"))

# Get last rows of each no bunt AB to check for intentional walk
last_rows_nobunt <- nobunt_attempts_0games_df |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    intentional_walk = ifelse(
      result.event == "Intent Walk" | 
        grepl("intentional walk", tolower(result.description)), TRUE, FALSE)
  ) |>
  select(game_pk, about.inning, about.halfInning, atBatIndex, intentional_walk)

# Find all innings where intentional walk happened in no bunt AB
ibb_innings <- last_rows_nobunt |>
  filter(intentional_walk) |>
  select(game_pk, about.inning, about.halfInning, first_ab = atBatIndex)

# get the next AB immediately after the IBB from the full pbp
next_ab_after_ibb <- pbp |>
  semi_join(ibb_innings, by = c("game_pk", "about.inning", "about.halfInning")) |>
  left_join(ibb_innings, 
            by = c("game_pk", "about.inning", "about.halfInning")) |>
  filter(atBatIndex == first_ab + 1)

# get last row of the next AB
last_row_next_ab <- next_ab_after_ibb |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties=FALSE) |>
  ungroup()

# did that AB *end* with a bunt?
next_ab_summary <- last_row_next_ab |>
  mutate(
    bunt_attempted_in_next_ab = ifelse(
      grepl("bunt", result.description, ignore.case=TRUE) |
        grepl("bunt", details.description, ignore.case=TRUE) |
        grepl("bunt", result.event, ignore.case=TRUE),
      TRUE,
      FALSE
    )
  ) |>
  select(game_pk, about.inning, about.halfInning, bunt_attempted_in_next_ab)

# attach whether a run scored in that half-inning
next_ab_summary <- next_ab_summary |>
  left_join(
    seren_exit |> 
      select(game_pk, about.inning, about.halfInning, score_diff_entry, score_diff_exit),
    by = c("game_pk","about.inning","about.halfInning")
  ) |>
  mutate(
    run_scored = score_diff_exit < score_diff_entry
  )

# summarise
final_next_ab_summary <- next_ab_summary |>
  group_by(bunt_attempted_in_next_ab) |>
  summarise(
    n = n(),
    runs_scored_n = sum(run_scored, na.rm=TRUE),
    runs_scored_pct = runs_scored_n / n * 100,
    .groups="drop"
  )

###

# how many runs score when no bunt/failed bunt/success bunt to start inning of 1run game

# 1. first AB bunts in tied games
bunt_attempts_1games <- bunt_success |>
  filter(score_diff_entry == 1)

# 2. all first ABs in tied games
firstabs_one <- first_ab_allrows |>
  filter(score_diff_entry == 1) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_1games <- firstabs_one |>
  anti_join(
    bunt_attempts_1games |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exitone <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, about.halfInning) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_1games <- bunt_attempts_1games |>
  left_join(seren_exitone, 
            by = c("game_pk", "about.inning", "about.halfInning"))

nobunt_attempts_1games <- nobunt_attempts_1games |>
  left_join(seren_exitone,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_1games <- bunt_attempts_1games |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored
  )

nobunt_attempts_1games <- nobunt_attempts_1games |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summaryone <- bind_rows(bunt_attempts_1games, nobunt_attempts_1games) |>
  group_by(attempt_category) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )

# look at no-bunt innings

# Get all first ABs with no bunt attempts by anti-joining bunt attempts
nobunt_attempts_1games_keys <- nobunt_attempts_1games 

#  Join back to first_ab_allrows to get all rows of those no-bunt first ABs
nobunt_attempts_1games_df <- first_ab_allrows |>
  semi_join(nobunt_attempts_1games_keys, 
            by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex"))

# Get last rows of each no bunt AB to check for intentional walk
last_rows_nobuntone <- nobunt_attempts_1games_df |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    intentional_walk = ifelse(
      result.event == "Intent Walk" | 
        grepl("intentional walk", tolower(result.description)), TRUE, FALSE)
  ) |>
  select(game_pk, about.inning, about.halfInning, atBatIndex, intentional_walk)

# Find all innings where intentional walk happened in no bunt AB
ibb_inningsone <- last_rows_nobuntone |>
  filter(intentional_walk) |>
  select(game_pk, about.inning, about.halfInning, first_ab = atBatIndex)

# get the next AB immediately after the IBB from the full pbp
next_ab_after_ibbone <- pbp |>
  semi_join(ibb_inningsone, by = c("game_pk", "about.inning", "about.halfInning")) |>
  left_join(ibb_inningsone, 
            by = c("game_pk", "about.inning", "about.halfInning")) |>
  filter(atBatIndex == first_ab + 1)

# get last row of the next AB
last_row_next_abone <- next_ab_after_ibbone |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties=FALSE) |>
  ungroup()

# did that AB *end* with a bunt?
next_ab_summaryone <- last_row_next_abone |>
  mutate(
    bunt_attempted_in_next_ab = ifelse(
      grepl("bunt", result.description, ignore.case=TRUE) |
        grepl("bunt", details.description, ignore.case=TRUE) |
        grepl("bunt", result.event, ignore.case=TRUE),
      TRUE,
      FALSE
    )
  ) |>
  select(game_pk, about.inning, about.halfInning, bunt_attempted_in_next_ab)

# attach whether a run scored in that half-inning
next_ab_summaryone <- next_ab_summaryone |>
  left_join(
    seren_exit |> 
      select(game_pk, about.inning, about.halfInning, score_diff_entry, score_diff_exit),
    by = c("game_pk","about.inning","about.halfInning")
  ) |>
  mutate(
    run_scored = score_diff_exit < score_diff_entry
  )

# summarise
final_next_ab_summaryone <- next_ab_summaryone |>
  group_by(bunt_attempted_in_next_ab) |>
  summarise(
    n = n(),
    runs_scored_n = sum(run_scored, na.rm=TRUE),
    runs_scored_pct = runs_scored_n / n * 100,
    .groups="drop"
  )


### AWAY TEAM SCORING

# first identify the atBatIndex of the first AB in each relevant half-inning
first_ab_indices_top <- pbp |>
  filter(about.inning >= 10, about.halfInning == "top") |>
  group_by(game_pk, about.inning) |>
  summarise(first_ab = min(about.atBatIndex), .groups = "drop")

# then filter pbp to keep all rows matching that first AB
first_ab_allrows_top <- pbp |>
  inner_join(first_ab_indices_top, 
             by = c("game_pk", "about.inning")) |>
  filter(atBatIndex == first_ab) |>
  arrange(game_date, game_pk, about.atBatIndex)

# flag each row as having a bunt keyword
first_ab_allrows_top <- first_ab_allrows_top |>
  mutate(
    bunt_flag = grepl("bunt", result.description, ignore.case = TRUE) |
      grepl("bunt", result.event, ignore.case = TRUE) |
      grepl("bunt", details.description, ignore.case = TRUE)
  )

# summarize across each AB whether *any* row had a bunt
buntattempts_top <- first_ab_allrows_top |>
  group_by(game_pk, about.inning, atBatIndex) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

ba_ratio_top <- buntattempts_top |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

# get the first pitcher of each half-inning in extras
seren_first_p <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(first_row_index, with_ties = FALSE) |>
  select(game_pk, about.inning, about.halfInning, matchup.pitcher.id,
         score_diff_entry, score_diff_exit)

# then join this to the first_ab_allrows (which is one AB per half-inning)
first_ab_allrows_top <- first_ab_allrows_top |>
  left_join(
    seren_first_p,
    by = c("game_pk", "about.inning", "about.halfInning", "matchup.pitcher.id")
  )


# get last row of each AB in extras
last_ab_rows_top <- first_ab_allrows_top |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup()

# then filter and classify bunts only on the final AB rows
bunt_success_top <- last_ab_rows_top |>
  filter(bunt_flag == TRUE) |>
  filter(grepl("bunt", result.description, ignore.case = TRUE)) |>
  mutate(
    num_bases_occupied_post = rowSums(
      across(
        c(matchup.postOnFirst.id, matchup.postOnSecond.id, matchup.postOnThird.id),
        ~ !is.na(.x)
      )
    ),
    success_flag = case_when(
      !is.na(matchup.postOnThird.id) ~ TRUE,  # runner advanced to 3rd
      num_bases_occupied_post >= 2 ~ TRUE,    # at least two runners on base
      grepl("scores", result.description, ignore.case = TRUE) ~ TRUE,  # scored
      TRUE ~ FALSE
    )
  ) |>
  mutate(success_flag = ifelse(result.event == "Bunt Pop Out", FALSE, success_flag),
         success_flag = ifelse(result.event == "Strikeout", FALSE, success_flag),
         matchup.postOnFirst.id = ifelse(result.event == "Strikeout", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(result.event == "Bunt Pop Out", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(
           str_detect(result.description, "out on a sacrifice bunt"),
           NA,
           matchup.postOnFirst.id
         ),
         matchup.postOnSecond.id = ifelse(
           str_detect(result.description, "Andres Gimenez out on a sacrifice bunt, third baseman Alex Bregman to first baseman Jose Abreu. Tyler Freeman to 3rd."),
           NA,
           matchup.postOnSecond.id
         )
  )

bunt_success_summary_top <- bunt_success_top |>
  summarise(
    total_bunts = n(),
    successful_bunts = sum(success_flag == TRUE, na.rm = TRUE),
    success_rate = successful_bunts / total_bunts
  )

bunt_event_summary_top <- bunt_success_top |>
  mutate(
    bases_start = case_when(
      !is.na(matchup.postOnFirst.id) & !is.na(matchup.postOnThird.id) ~ "1st and 3rd",
      !is.na(matchup.postOnFirst.id) ~ "1st",
      !is.na(matchup.postOnSecond.id) & !is.na(matchup.postOnThird.id) ~ "2nd and 3rd",
      !is.na(matchup.postOnSecond.id) ~ "2nd",
      !is.na(matchup.postOnThird.id) ~ "3rd",
      TRUE ~ "empty"
    ),
    matchup.postOnSecond.id = ifelse(bases_start == "2nd and 3rd", NA, matchup.postOnSecond.id),
    bases_start = ifelse(bases_start == "2nd and 3rd", "3rd", bases_start),
    matchup.postOnSecond.id = ifelse(bases_start == "1st and 3rd", NA, matchup.postOnSecond.id)
  ) |>
  group_by(bases_start, result.event, count.outs.end) |>
  summarise(
    instances = n(),
    successful_bunts = sum(success_flag, na.rm = TRUE),
    success_rate = successful_bunts / instances * 100,
    .groups = "drop"
  ) |>
  arrange(desc(instances))


# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_0games_top <- bunt_success_top |>
  filter(score_diff_entry == 0)

# 2. all first ABs in tied games
firstabs_tied_top <- first_ab_allrows_top |>
  filter(score_diff_entry == 0) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_0games_top <- firstabs_tied_top |>
  anti_join(
    bunt_attempts_0games_top |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exit_top <- seren |>
  filter(about.inning >= 10, about.halfInning == "top") |>
  group_by(game_pk, about.inning, about.halfInning) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_0games_top <- bunt_attempts_0games_top |>
  left_join(seren_exit_top, 
            by = c("game_pk", "about.inning", "about.halfInning"))

nobunt_attempts_0games_top <- nobunt_attempts_0games_top |>
  left_join(seren_exit_top,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_0games_top <- bunt_attempts_0games_top |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored
  )

nobunt_attempts_0games_top <- nobunt_attempts_0games_top |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summary_top <- bind_rows(bunt_attempts_0games_top, nobunt_attempts_0games_top) |>
  group_by(attempt_category) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )

# look at no-bunt innings

# Get all first ABs with no bunt attempts by anti-joining bunt attempts
nobunt_attempts_0games_keys_top <- nobunt_attempts_0games_top

#  Join back to first_ab_allrows to get all rows of those no-bunt first ABs
nobunt_attempts_0games_df_top <- first_ab_allrows_top |>
  semi_join(nobunt_attempts_0games_keys_top, 
            by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex"))

# Get last rows of each no bunt AB to check for intentional walk
last_rows_nobunt_top <- nobunt_attempts_0games_df_top |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    intentional_walk = ifelse(
      result.event == "Intent Walk" | 
        grepl("intentional walk", tolower(result.description)), TRUE, FALSE)
  ) |>
  select(game_pk, about.inning, about.halfInning, atBatIndex, intentional_walk)

# Find all innings where intentional walk happened in no bunt AB
ibb_innings_top <- last_rows_nobunt_top |>
  filter(intentional_walk) |>
  select(game_pk, about.inning, about.halfInning, first_ab = atBatIndex)

# get the next AB immediately after the IBB from the full pbp
next_ab_after_ibb_top <- pbp |>
  semi_join(ibb_innings_top, by = c("game_pk", "about.inning", "about.halfInning")) |>
  left_join(ibb_innings_top, 
            by = c("game_pk", "about.inning", "about.halfInning")) |>
  filter(atBatIndex == first_ab + 1)

# get last row of the next AB
last_row_next_ab_top <- next_ab_after_ibb_top |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_max(pitchNumber, n = 1, with_ties=FALSE) |>
  ungroup()

# did that AB *end* with a bunt?
next_ab_summary_top <- last_row_next_ab_top |>
  mutate(
    bunt_attempted_in_next_ab = ifelse(
      grepl("bunt", result.description, ignore.case=TRUE) |
        grepl("bunt", details.description, ignore.case=TRUE) |
        grepl("bunt", result.event, ignore.case=TRUE),
      TRUE,
      FALSE
    )
  ) |>
  select(game_pk, about.inning, about.halfInning, bunt_attempted_in_next_ab)

# attach whether a run scored in that half-inning
next_ab_summary_top <- next_ab_summary_top |>
  left_join(
    seren_exit_top |> 
      select(game_pk, about.inning, about.halfInning, score_diff_entry, score_diff_exit),
    by = c("game_pk","about.inning","about.halfInning")
  ) |>
  mutate(
    run_scored = score_diff_exit < score_diff_entry
  )

# summarise
final_next_ab_summary_top <- next_ab_summary_top |>
  group_by(bunt_attempted_in_next_ab) |>
  summarise(
    n = n(),
    runs_scored_n = sum(run_scored, na.rm=TRUE),
    runs_scored_pct = runs_scored_n / n * 100,
    .groups="drop"
  )

###

# win percentage by bottom inning deficit
inning_results_wlt <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)], # first pitcher
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],  # last pitcher
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Loss",
      score_diff_exit <  0 ~ "Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  count(score_diff_entry, result, name = "n") |>
  group_by(score_diff_entry) |>
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) |>
  ungroup()

# bunt attempt and success breakdown by TA

# first identify the atBatIndex of the first AB in each relevant half-inning
first_ab_indices_ta <- pbp |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning) |>
  summarise(first_ab = min(about.atBatIndex), .groups = "drop")

# then filter pbp to keep all rows matching that first AB
first_ab_allrows_ta <- pbp |>
  inner_join(first_ab_indices_ta, 
             by = c("game_pk", "about.inning")) |>
  filter(atBatIndex == first_ab) |>
  arrange(game_date, game_pk, about.atBatIndex)

# flag each row as having a bunt keyword
first_ab_allrows_ta <- first_ab_allrows_ta |>
  mutate(
    bunt_flag = grepl("bunt", result.description, ignore.case = TRUE) |
      grepl("bunt", result.event, ignore.case = TRUE) |
      grepl("bunt", details.description, ignore.case = TRUE)
  )

# get the first pitcher of each half-inning in extras
seren_first_p_ta <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(first_row_index, with_ties = FALSE) |>
  select(game_pk, about.inning, about.halfInning, matchup.pitcher.id,
         score_diff_entry, score_diff_exit, ta_h)

# then join this to the first_ab_allrows (which is one AB per half-inning)
first_ab_allrows_ta <- first_ab_allrows_ta |>
  left_join(
    seren_first_p_ta,
    by = c("game_pk", "about.inning", "about.halfInning", "matchup.pitcher.id")
  )


buntattempts_close_ta <- first_ab_allrows_ta |>
  filter(score_diff_entry == 1 | score_diff_entry == 0) |>
  group_by(game_pk, about.inning, atBatIndex, ta_h) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

bac_ratio_ta <- buntattempts_close_ta |>
  group_by(ta_h) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

buntattempts_zero_ta <- first_ab_allrows_ta |>
  filter(score_diff_entry == 0) |>
  group_by(game_pk, about.inning, atBatIndex, ta_h) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

baz_ratio_ta <- buntattempts_zero_ta |>
  group_by(ta_h) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

buntattempts_one_ta <- first_ab_allrows_ta |>
  filter(score_diff_entry == 1) |>
  group_by(game_pk, about.inning, atBatIndex, ta_h) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

bao_ratio_ta <- buntattempts_one_ta |>
  group_by(ta_h) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

# get last row of each AB in extras
last_ab_rows_ta <- first_ab_allrows_ta |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, ta_h) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup()

# then filter and classify bunts only on the final AB rows
bunt_success_ta <- last_ab_rows_ta |>
  filter(bunt_flag == TRUE) |>
  filter(grepl("bunt", result.description, ignore.case = TRUE)) |>
  mutate(
    num_bases_occupied_post = rowSums(
      across(
        c(matchup.postOnFirst.id, matchup.postOnSecond.id, matchup.postOnThird.id),
        ~ !is.na(.x)
      )
    ),
    success_flag = case_when(
      !is.na(matchup.postOnThird.id) ~ TRUE,  # runner advanced to 3rd
      num_bases_occupied_post >= 2 ~ TRUE,    # at least two runners on base
      grepl("scores", result.description, ignore.case = TRUE) ~ TRUE,  # scored
      TRUE ~ FALSE
    )
  ) |>
  mutate(success_flag = ifelse(result.event == "Bunt Pop Out", FALSE, success_flag),
         success_flag = ifelse(result.event == "Strikeout", FALSE, success_flag),
         matchup.postOnFirst.id = ifelse(result.event == "Strikeout", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(result.event == "Bunt Pop Out", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(
           str_detect(result.description, "out on a sacrifice bunt"),
           NA,
           matchup.postOnFirst.id
         ),
         matchup.postOnSecond.id = ifelse(
           str_detect(result.description, "Andres Gimenez out on a sacrifice bunt, third baseman Alex Bregman to first baseman Jose Abreu. Tyler Freeman to 3rd."),
           NA,
           matchup.postOnSecond.id
         )
  )

bunt_success_summary_ta <- bunt_success_ta |>
  group_by(ta_h) |> 
  summarise(
    total_bunts = n(),
    successful_bunts = sum(success_flag == TRUE, na.rm = TRUE),
    success_rate = successful_bunts / total_bunts
  )

bunt_event_summary_ta <- bunt_success_ta |>
  mutate(
    bases_start = case_when(
      !is.na(matchup.postOnFirst.id) & !is.na(matchup.postOnThird.id) ~ "1st and 3rd",
      !is.na(matchup.postOnFirst.id) ~ "1st",
      !is.na(matchup.postOnSecond.id) & !is.na(matchup.postOnThird.id) ~ "2nd and 3rd",
      !is.na(matchup.postOnSecond.id) ~ "2nd",
      !is.na(matchup.postOnThird.id) ~ "3rd",
      TRUE ~ "empty"
    )
  ) |>
  group_by(bases_start, result.event, count.outs.end, ta_h) |>
  summarise(
    instances = n(),
    successful_bunts = sum(success_flag, na.rm = TRUE),
    success_rate = successful_bunts / instances * 100,
    .groups = "drop"
  ) |>
  arrange(desc(instances))

# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_0games_ta <- bunt_success_ta |>
  filter(score_diff_entry == 0)

# 2. all first ABs in tied games
firstabs_tied_ta <- first_ab_allrows_ta |>
  filter(score_diff_entry == 0) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex, ta_h)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_0games_ta <- firstabs_tied_ta |>
  anti_join(
    bunt_attempts_0games_ta |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex, ta_h),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex", "ta_h")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exitone_ta <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, about.halfInning, ta_h) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_0games_ta <- bunt_attempts_0games_ta |>
  left_join(seren_exit, 
            by = c("game_pk", "about.inning", "about.halfInning"))

nobunt_attempts_0games_ta <- nobunt_attempts_0games_ta |>
  left_join(seren_exit,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_0games_ta <- bunt_attempts_0games_ta |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored,
    ta_h
  )

nobunt_attempts_0games_ta <- nobunt_attempts_0games_ta |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summary_ta <- bind_rows(bunt_attempts_0games_ta, nobunt_attempts_0games_ta) |>
  group_by(attempt_category, ta_h) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )

# look at no-bunt innings

# Get all first ABs with no bunt attempts by anti-joining bunt attempts
nobunt_attempts_0games_keys_ta <- nobunt_attempts_0games_ta 

#  Join back to first_ab_allrows to get all rows of those no-bunt first ABs
nobunt_attempts_0games_df_ta <- first_ab_allrows_ta |>
  semi_join(nobunt_attempts_0games_keys_ta, 
            by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex", "ta_h"))

# Get last rows of each no bunt AB to check for intentional walk
last_rows_nobunt_ta <- nobunt_attempts_0games_df_ta |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, ta_h) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    intentional_walk = ifelse(
      result.event == "Intent Walk" | 
        grepl("intentional walk", tolower(result.description)), TRUE, FALSE)
  ) |>
  select(game_pk, about.inning, about.halfInning, atBatIndex, intentional_walk, ta_h)

# Find all innings where intentional walk happened in no bunt AB
ibb_innings_ta <- last_rows_nobunt_ta |>
  filter(intentional_walk) |>
  select(game_pk, about.inning, about.halfInning, first_ab = atBatIndex, ta_h)

# get the next AB immediately after the IBB from the full pbp
next_ab_after_ibb_ta <- pbp |>
  semi_join(ibb_innings_ta, by = c("game_pk", "about.inning", "about.halfInning")) |>
  left_join(ibb_innings_ta, 
            by = c("game_pk", "about.inning", "about.halfInning")) |>
  filter(atBatIndex == first_ab + 1)

# get last row of the next AB
last_row_next_ab_ta <- next_ab_after_ibb_ta |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, ta_h) |>
  slice_max(pitchNumber, n = 1, with_ties=FALSE) |>
  ungroup()

# did that AB *end* with a bunt?
next_ab_summary_ta <- last_row_next_ab_ta |>
  mutate(
    bunt_attempted_in_next_ab = ifelse(
      grepl("bunt", result.description, ignore.case=TRUE) |
        grepl("bunt", details.description, ignore.case=TRUE) |
        grepl("bunt", result.event, ignore.case=TRUE),
      TRUE,
      FALSE
    )
  ) |>
  select(game_pk, about.inning, about.halfInning, bunt_attempted_in_next_ab, ta_h)

# attach whether a run scored in that half-inning
next_ab_summary_ta <- next_ab_summary_ta |>
  left_join(
    seren_exit |> 
      select(game_pk, about.inning, about.halfInning, score_diff_entry, score_diff_exit),
    by = c("game_pk","about.inning","about.halfInning")
  ) |>
  mutate(
    run_scored = score_diff_exit < score_diff_entry
  )

# summarise
final_next_ab_summary_ta <- next_ab_summary_ta |>
  group_by(bunt_attempted_in_next_ab, ta_h) |>
  summarise(
    n = n(),
    runs_scored_n = sum(run_scored, na.rm=TRUE),
    runs_scored_pct = runs_scored_n / n * 100,
    .groups="drop"
  )

###

# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_1games_ta <- bunt_success_ta |>
  filter(score_diff_entry == 1)

# 2. all first ABs in tied games
firstabs_one_ta <- first_ab_allrows_ta |>
  filter(score_diff_entry == 1) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_1games_ta <- firstabs_one_ta |>
  anti_join(
    bunt_attempts_1games_ta |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exitone_ta <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, about.halfInning, ta_h) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_1games_ta <- bunt_attempts_1games_ta |>
  left_join(seren_exitone_ta, 
            by = c("game_pk", "about.inning", "about.halfInning", "ta_h"))

nobunt_attempts_1games_ta <- nobunt_attempts_1games_ta |>
  left_join(seren_exitone_ta,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_1games_ta <- bunt_attempts_1games_ta |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored,
    ta_h
  )

nobunt_attempts_1games_ta <- nobunt_attempts_1games_ta |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summaryone_ta <- bind_rows(bunt_attempts_1games_ta, nobunt_attempts_1games_ta) |>
  group_by(attempt_category, ta_h) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )


### AWAY TEAM SCORING

# first identify the atBatIndex of the first AB in each relevant half-inning
first_ab_indices_top_ta <- pbp |>
  filter(about.inning >= 10, about.halfInning == "top") |>
  group_by(game_pk, about.inning) |>
  summarise(first_ab = min(about.atBatIndex), .groups = "drop")

# then filter pbp to keep all rows matching that first AB
first_ab_allrows_top_ta <- pbp |>
  inner_join(first_ab_indices_top_ta, 
             by = c("game_pk", "about.inning")) |>
  filter(atBatIndex == first_ab) |>
  arrange(game_date, game_pk, about.atBatIndex)

# flag each row as having a bunt keyword
first_ab_allrows_top_ta <- first_ab_allrows_top_ta |>
  mutate(
    bunt_flag = grepl("bunt", result.description, ignore.case = TRUE) |
      grepl("bunt", result.event, ignore.case = TRUE) |
      grepl("bunt", details.description, ignore.case = TRUE)
  )

# get the first pitcher of each half-inning in extras
seren_first_p_top_ta <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(first_row_index, with_ties = FALSE) |>
  select(game_pk, about.inning, about.halfInning, matchup.pitcher.id,
         score_diff_entry, score_diff_exit, ta_h)

# then join this to the first_ab_allrows (which is one AB per half-inning)
first_ab_allrows_top_ta <- first_ab_allrows_top_ta |>
  left_join(
    seren_first_p_top_ta,
    by = c("game_pk", "about.inning", "about.halfInning", "matchup.pitcher.id")
  )


# summarize across each AB whether *any* row had a bunt
buntattempts_top_ta <- first_ab_allrows_top_ta |>
  group_by(game_pk, about.inning, atBatIndex, ta_h) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

ba_ratio_top_ta <- buntattempts_top_ta |>
  group_by(ta_h) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )


# get last row of each AB in extras
last_ab_rows_top_ta <- first_ab_allrows_top_ta |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, ta_h) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup()

# then filter and classify bunts only on the final AB rows
bunt_success_top_ta <- last_ab_rows_top_ta |>
  filter(bunt_flag == TRUE) |>
  filter(grepl("bunt", result.description, ignore.case = TRUE)) |>
  mutate(
    num_bases_occupied_post = rowSums(
      across(
        c(matchup.postOnFirst.id, matchup.postOnSecond.id, matchup.postOnThird.id),
        ~ !is.na(.x)
      )
    ),
    success_flag = case_when(
      !is.na(matchup.postOnThird.id) ~ TRUE,  # runner advanced to 3rd
      num_bases_occupied_post >= 2 ~ TRUE,    # at least two runners on base
      grepl("scores", result.description, ignore.case = TRUE) ~ TRUE,  # scored
      TRUE ~ FALSE
    )
  ) |>
  mutate(success_flag = ifelse(result.event == "Bunt Pop Out", FALSE, success_flag),
         success_flag = ifelse(result.event == "Strikeout", FALSE, success_flag),
         matchup.postOnFirst.id = ifelse(result.event == "Strikeout", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(result.event == "Bunt Pop Out", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(
           str_detect(result.description, "out on a sacrifice bunt"),
           NA,
           matchup.postOnFirst.id
         ),
         matchup.postOnSecond.id = ifelse(
           str_detect(result.description, "Andres Gimenez out on a sacrifice bunt, third baseman Alex Bregman to first baseman Jose Abreu. Tyler Freeman to 3rd."),
           NA,
           matchup.postOnSecond.id
         )
  )

bunt_success_summary_top_ta <- bunt_success_top_ta |>
  group_by(ta_h) |>
  summarise(
    total_bunts = n(),
    successful_bunts = sum(success_flag == TRUE, na.rm = TRUE),
    success_rate = successful_bunts / total_bunts
  )

bunt_event_summary_top_ta <- bunt_success_top_ta |>
  mutate(
    bases_start = case_when(
      !is.na(matchup.postOnFirst.id) & !is.na(matchup.postOnThird.id) ~ "1st and 3rd",
      !is.na(matchup.postOnFirst.id) ~ "1st",
      !is.na(matchup.postOnSecond.id) & !is.na(matchup.postOnThird.id) ~ "2nd and 3rd",
      !is.na(matchup.postOnSecond.id) ~ "2nd",
      !is.na(matchup.postOnThird.id) ~ "3rd",
      TRUE ~ "empty"
    ),
    matchup.postOnSecond.id = ifelse(bases_start == "2nd and 3rd", NA, matchup.postOnSecond.id),
    bases_start = ifelse(bases_start == "2nd and 3rd", "3rd", bases_start),
    matchup.postOnSecond.id = ifelse(bases_start == "1st and 3rd", NA, matchup.postOnSecond.id)
  ) |>
  group_by(bases_start, result.event, count.outs.end, ta_h) |>
  summarise(
    instances = n(),
    successful_bunts = sum(success_flag, na.rm = TRUE),
    success_rate = successful_bunts / instances * 100,
    .groups = "drop"
  ) |>
  arrange(desc(instances))


# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_0games_top_ta <- bunt_success_top_ta |>
  filter(score_diff_entry == 0)

# 2. all first ABs in tied games
firstabs_tied_top_ta <- first_ab_allrows_top_ta |>
  filter(score_diff_entry == 0) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex, ta_h)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_0games_top_ta <- firstabs_tied_top_ta |>
  anti_join(
    bunt_attempts_0games_top_ta |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex, ta_h),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex", "ta_h")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exit_top_ta <- seren |>
  filter(about.inning >= 10, about.halfInning == "top") |>
  group_by(game_pk, about.inning, about.halfInning, ta_h) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_0games_top_ta <- bunt_attempts_0games_top_ta |>
  left_join(seren_exit_top_ta, 
            by = c("game_pk", "about.inning", "about.halfInning", "ta_h"))

nobunt_attempts_0games_top_ta <- nobunt_attempts_0games_top_ta |>
  left_join(seren_exit_top_ta,
            by = c("game_pk", "about.inning", "about.halfInning", "ta_h"))

# classify
bunt_attempts_0games_top_ta <- bunt_attempts_0games_top_ta |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored,
    ta_h
  )

nobunt_attempts_0games_top_ta <- nobunt_attempts_0games_top_ta |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summary_top_ta <- bind_rows(bunt_attempts_0games_top_ta, nobunt_attempts_0games_top_ta) |>
  group_by(attempt_category, ta_h) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )

###

# win percentage by bottom inning deficit
inning_results_wlt_ta <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, ta_h) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)], # first pitcher
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],  # last pitcher
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Loss",
      score_diff_exit <  0 ~ "Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  count(score_diff_entry, ta_h, result) |>
  group_by(score_diff_entry, ta_h) |>
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) |>
  ungroup()



inning_results_wlt_ta_trusted <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, ta_h) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)], # first pitcher
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],  # last pitcher
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Loss",
      score_diff_exit <  0 ~ "Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  count(score_diff_entry, ta_h, result, name = "n") |>
  group_by(score_diff_entry) |>
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) |>
  ungroup()

inning_results_winpct_trusted <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, ta_h) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Home Loss",
      score_diff_exit <  0 ~ "Home Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  # count for each score_diff_entry and ta
  count(score_diff_entry, ta_h, result, name = "n") |>
  group_by(score_diff_entry, ta_h) |>
  mutate(
    total = sum(n)
  ) |>
  ungroup() |>
  mutate(
    pct = n / total * 100
  ) |>
  arrange(score_diff_entry, result, desc(ta_h))

#analysis using K/FB/GB pitchers
kfbgb <- all_logs |> 
  filter(season >= 2020) |> 
  select(playerid, Date, season, Team, HomeAway, cume_TBF, cume_GB, cume_FB, 
         cume_bip, cume_SO, cume_BB, K_pct_YTD, K_pct_SZN, BB_pct_YTD, BB_pct_SZN,
         GB_pct_YTD, GB_pct_SZN, FB_pct_YTD, FB_pct_SZN, SO, FB, GB, bipCount, TBF)

kfbgb <- kfbgb |>
  arrange(Date) |>
  group_by(playerid, season) |>
  mutate(GB_tbfpct_YTD = cume_GB / cume_TBF,
         FB_tbfpct_YTD = cume_FB / cume_TBF) |>
  ungroup()

kfbgb <- kfbgb |>
  arrange(Date) |>
  group_by(playerid, season) |>
  mutate(GB_tbfpct_SZN  = last(GB_tbfpct_YTD),
         FB_tbfpct_SZN  = last(FB_tbfpct_YTD)
  ) |> ungroup()


kfbgb <- kfbgb |>
  select(season, playerid, K_pct_SZN, FB_pct_SZN, GB_pct_SZN) |>
  unique()

kfbgb <- kfbgb |>
  mutate(FB_pct_SZN = ifelse(is.na(FB_pct_SZN), 0, FB_pct_SZN),
         GB_pct_SZN = ifelse(is.na(GB_pct_SZN), 0, GB_pct_SZN))

kfbgb <- kfbgb |>
  mutate(
    K_percentile  = percent_rank(K_pct_SZN),
    GB_percentile = percent_rank(GB_pct_SZN),
    FB_percentile = percent_rank(FB_pct_SZN)
  ) |>
  mutate(
    pitcher_type = pmap_chr(
      list(K_percentile, GB_percentile, FB_percentile),
      function(k, gb, fb) {
        paste(na.omit(c(
          if(k >= 0.75) "K" else NA,
          if(gb >= 0.75) "GB" else NA,
          if(fb >= 0.75) "FB" else NA
        )), collapse = "-")
      }
    )
  )

kfbgb <- kfbgb |> mutate(
  pitcher_type = ifelse(pitcher_type == "", "Neutral", pitcher_type)
)

kfbgb <- kfbgb |> select(season, playerid, pitcher_type)

#add to seren
seren <- left_join(seren, kfbgb, by = c("season", "key_fangraphs" = "playerid"))


# Example aggregated table
plot_df <- seren |>  # or wherever you have ta_h and pitcher_type merged
  select(season, matchup.pitcher.fullName, ta_h, pitcher_type) |>
  unique() |>
  group_by(pitcher_type, ta_h) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(pitcher_type) |>
  mutate(pct = n / sum(n))

ggplot(plot_df, aes(x = pitcher_type, y = pct, fill = ta_h)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = c("TRUE" = "green4", "FALSE" = "red3"),  # higher contrast than green/red
    ) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1.05, by = 0.2),
    minor_breaks = seq(0, 1.05, by = 0.05),
    limits = c(0, 1.05)  # little more space for text above bars
  ) +
  labs(
    title = "Trusted Arm Usage by Pitcher Type",
    x = "Pitcher Type",
    y = "Proportion",
    fill = "Trusted Arm"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

chi_type_table <- table(seren$pitcher_type, seren$ta_h) 
chi_type_table |> chisq.test()

###

# bunt attempt and success breakdown by TA

# first identify the atBatIndex of the first AB in each relevant half-inning
first_ab_indices_ptype <- pbp |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning) |>
  summarise(first_ab = min(about.atBatIndex), .groups = "drop")

# then filter pbp to keep all rows matching that first AB
first_ab_allrows_ptype <- pbp |>
  inner_join(first_ab_indices_ptype, 
             by = c("game_pk", "about.inning")) |>
  filter(atBatIndex == first_ab) |>
  arrange(game_date, game_pk, about.atBatIndex)

# flag each row as having a bunt keyword
first_ab_allrows_ptype <- first_ab_allrows_ptype |>
  mutate(
    bunt_flag = grepl("bunt", result.description, ignore.case = TRUE) |
      grepl("bunt", result.event, ignore.case = TRUE) |
      grepl("bunt", details.description, ignore.case = TRUE)
  )

# get the first pitcher of each half-inning in extras
seren_first_p_ptype <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(first_row_index, with_ties = FALSE) |>
  select(game_pk, about.inning, about.halfInning, matchup.pitcher.id,
         score_diff_entry, score_diff_exit, pitcher_type)

# then join this to the first_ab_allrows (which is one AB per half-inning)
first_ab_allrows_ptype <- first_ab_allrows_ptype |>
  left_join(
    seren_first_p_ptype,
    by = c("game_pk", "about.inning", "about.halfInning", "matchup.pitcher.id")
  )


buntattempts_close_ptype <- first_ab_allrows_ptype |>
  filter(score_diff_entry == 1 | score_diff_entry == 0) |>
  group_by(game_pk, about.inning, atBatIndex, pitcher_type) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

bac_ratio_ptype <- buntattempts_close_ptype |>
  group_by(pitcher_type) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

buntattempts_zero_ptype <- first_ab_allrows_ptype |>
  filter(score_diff_entry == 0) |>
  group_by(game_pk, about.inning, atBatIndex, pitcher_type) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

baz_ratio_ptype <- buntattempts_zero_ptype |>
  group_by(pitcher_type) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

buntattempts_one_ptype <- first_ab_allrows_ptype |>
  filter(score_diff_entry == 1) |>
  group_by(game_pk, about.inning, atBatIndex, pitcher_type) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

bao_ratio_ptype <- buntattempts_one_ptype |>
  group_by(pitcher_type) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )

# get last row of each AB in extras
last_ab_rows_ptype <- first_ab_allrows_ptype |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup()

# then filter and classify bunts only on the final AB rows
bunt_success_ptype <- last_ab_rows_ptype |>
  filter(bunt_flag == TRUE) |>
  filter(grepl("bunt", result.description, ignore.case = TRUE)) |>
  mutate(
    num_bases_occupied_post = rowSums(
      across(
        c(matchup.postOnFirst.id, matchup.postOnSecond.id, matchup.postOnThird.id),
        ~ !is.na(.x)
      )
    ),
    success_flag = case_when(
      !is.na(matchup.postOnThird.id) ~ TRUE,  # runner advanced to 3rd
      num_bases_occupied_post >= 2 ~ TRUE,    # at least two runners on base
      grepl("scores", result.description, ignore.case = TRUE) ~ TRUE,  # scored
      TRUE ~ FALSE
    )
  ) |>
  mutate(success_flag = ifelse(result.event == "Bunt Pop Out", FALSE, success_flag),
         success_flag = ifelse(result.event == "Strikeout", FALSE, success_flag),
         matchup.postOnFirst.id = ifelse(result.event == "Strikeout", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(result.event == "Bunt Pop Out", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(
           str_detect(result.description, "out on a sacrifice bunt"),
           NA,
           matchup.postOnFirst.id
         ),
         matchup.postOnSecond.id = ifelse(
           str_detect(result.description, "Andres Gimenez out on a sacrifice bunt, third baseman Alex Bregman to first baseman Jose Abreu. Tyler Freeman to 3rd."),
           NA,
           matchup.postOnSecond.id
         )
  )

bunt_success_summary_ptype <- bunt_success_ptype |>
  group_by(pitcher_type) |> 
  summarise(
    total_bunts = n(),
    successful_bunts = sum(success_flag == TRUE, na.rm = TRUE),
    success_rate = successful_bunts / total_bunts
  )

bunt_event_summary_ptype <- bunt_success_ptype |>
  mutate(
    bases_start = case_when(
      !is.na(matchup.postOnFirst.id) & !is.na(matchup.postOnThird.id) ~ "1st and 3rd",
      !is.na(matchup.postOnFirst.id) ~ "1st",
      !is.na(matchup.postOnSecond.id) & !is.na(matchup.postOnThird.id) ~ "2nd and 3rd",
      !is.na(matchup.postOnSecond.id) ~ "2nd",
      !is.na(matchup.postOnThird.id) ~ "3rd",
      TRUE ~ "empty"
    )
  ) |>
  group_by(bases_start, result.event, count.outs.end, pitcher_type) |>
  summarise(
    instances = n(),
    successful_bunts = sum(success_flag, na.rm = TRUE),
    success_rate = successful_bunts / instances * 100,
    .groups = "drop"
  ) |>
  arrange(desc(instances))

# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_0games_ptype <- bunt_success_ptype |>
  filter(score_diff_entry == 0)

# 2. all first ABs in tied games
firstabs_tied_ptype <- first_ab_allrows_ptype |>
  filter(score_diff_entry == 0) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_0games_ptype <- firstabs_tied_ptype |>
  anti_join(
    bunt_attempts_0games_ptype |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex", "pitcher_type")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exitone_ptype <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, about.halfInning, pitcher_type) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_0games_ptype <- bunt_attempts_0games_ptype |>
  left_join(seren_exit, 
            by = c("game_pk", "about.inning", "about.halfInning"))

nobunt_attempts_0games_ptype <- nobunt_attempts_0games_ptype |>
  left_join(seren_exit,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_0games_ptype <- bunt_attempts_0games_ptype |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored,
    pitcher_type
  )

nobunt_attempts_0games_ptype <- nobunt_attempts_0games_ptype |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summary_ptype <- bind_rows(bunt_attempts_0games_ptype, nobunt_attempts_0games_ptype) |>
  group_by(attempt_category, pitcher_type) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  ) |>
  arrange(attempt_category, desc(run_scored_pct))

# look at no-bunt innings

# Get all first ABs with no bunt attempts by anti-joining bunt attempts
nobunt_attempts_0games_keys_ptype <- nobunt_attempts_0games_ptype 

#  Join back to first_ab_allrows to get all rows of those no-bunt first ABs
nobunt_attempts_0games_df_ptype <- first_ab_allrows_ptype |>
  semi_join(nobunt_attempts_0games_keys_ptype, 
            by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex", "pitcher_type"))

# Get last rows of each no bunt AB to check for intentional walk
last_rows_nobunt_ptype <- nobunt_attempts_0games_df_ptype |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    intentional_walk = ifelse(
      result.event == "Intent Walk" | 
        grepl("intentional walk", tolower(result.description)), TRUE, FALSE)
  ) |>
  select(game_pk, about.inning, about.halfInning, atBatIndex, intentional_walk, pitcher_type)

# Find all innings where intentional walk happened in no bunt AB
ibb_innings_ptype <- last_rows_nobunt_ptype |>
  filter(intentional_walk) |>
  select(game_pk, about.inning, about.halfInning, first_ab = atBatIndex, pitcher_type)

# get the next AB immediately after the IBB from the full pbp
next_ab_after_ibb_ptype <- pbp |>
  semi_join(ibb_innings_ptype, by = c("game_pk", "about.inning", "about.halfInning")) |>
  left_join(ibb_innings_ptype, 
            by = c("game_pk", "about.inning", "about.halfInning")) |>
  filter(atBatIndex == first_ab + 1)

# get last row of the next AB
last_row_next_ab_ptype <- next_ab_after_ibb_ptype |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type) |>
  slice_max(pitchNumber, n = 1, with_ties=FALSE) |>
  ungroup()

# did that AB *end* with a bunt?
next_ab_summary_ptype <- last_row_next_ab_ptype |>
  mutate(
    bunt_attempted_in_next_ab = ifelse(
      grepl("bunt", result.description, ignore.case=TRUE) |
        grepl("bunt", details.description, ignore.case=TRUE) |
        grepl("bunt", result.event, ignore.case=TRUE),
      TRUE,
      FALSE
    )
  ) |>
  select(game_pk, about.inning, about.halfInning, bunt_attempted_in_next_ab, pitcher_type)

# attach whether a run scored in that half-inning
next_ab_summary_ptype <- next_ab_summary_ptype |>
  left_join(
    seren_exit |> 
      select(game_pk, about.inning, about.halfInning, score_diff_entry, score_diff_exit),
    by = c("game_pk","about.inning","about.halfInning")
  ) |>
  mutate(
    run_scored = score_diff_exit < score_diff_entry
  )

# summarise
final_next_ab_summary_ptype <- next_ab_summary_ptype |>
  group_by(bunt_attempted_in_next_ab, pitcher_type) |>
  summarise(
    n = n(),
    runs_scored_n = sum(run_scored, na.rm=TRUE),
    runs_scored_pct = runs_scored_n / n * 100,
    .groups="drop"
  )

###

# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_1games_ptype <- bunt_success_ptype |>
  filter(score_diff_entry == 1)

# 2. all first ABs in tied games
firstabs_one_ptype <- first_ab_allrows_ptype |>
  filter(score_diff_entry == 1) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_1games_ptype <- firstabs_one_ptype |>
  anti_join(
    bunt_attempts_1games_ptype |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exitone_ptype <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, about.halfInning, pitcher_type) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_1games_ptype <- bunt_attempts_1games_ptype |>
  left_join(seren_exitone_ptype, 
            by = c("game_pk", "about.inning", "about.halfInning", "pitcher_type"))

nobunt_attempts_1games_ptype <- nobunt_attempts_1games_ptype |>
  left_join(seren_exitone_ptype,
            by = c("game_pk", "about.inning", "about.halfInning"))

# classify
bunt_attempts_1games_ptype <- bunt_attempts_1games_ptype |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored,
    pitcher_type
  )

nobunt_attempts_1games_ptype <- nobunt_attempts_1games_ptype |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summaryone_ptype <- bind_rows(bunt_attempts_1games_ptype, nobunt_attempts_1games_ptype) |>
  group_by(attempt_category, pitcher_type) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  ) |>
  arrange(attempt_category, desc(run_scored_pct))

all_bunt_run_summary_ptype <- bind_rows(bunt_run_summary_ptype, bunt_run_summaryone_ptype) |>
  group_by(attempt_category, pitcher_type) |>
  summarise(
    n = sum(n),
    run_scored_n = sum(run_scored_n, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  ) |>
  arrange(attempt_category, desc(run_scored_pct))
### AWAY TEAM SCORING

# first identify the atBatIndex of the first AB in each relevant half-inning
first_ab_indices_top_ptype <- pbp |>
  filter(about.inning >= 10, about.halfInning == "top") |>
  group_by(game_pk, about.inning) |>
  summarise(first_ab = min(about.atBatIndex), .groups = "drop")

# then filter pbp to keep all rows matching that first AB
first_ab_allrows_top_ptype <- pbp |>
  inner_join(first_ab_indices_top_ptype, 
             by = c("game_pk", "about.inning")) |>
  filter(atBatIndex == first_ab) |>
  arrange(game_date, game_pk, about.atBatIndex)

# flag each row as having a bunt keyword
first_ab_allrows_top_ptype <- first_ab_allrows_top_ptype |>
  mutate(
    bunt_flag = grepl("bunt", result.description, ignore.case = TRUE) |
      grepl("bunt", result.event, ignore.case = TRUE) |
      grepl("bunt", details.description, ignore.case = TRUE)
  )

# get the first pitcher of each half-inning in extras
seren_first_p_top_ptype <- seren |>
  filter(about.inning >= 10) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  slice_min(first_row_index, with_ties = FALSE) |>
  select(game_pk, about.inning, about.halfInning, matchup.pitcher.id,
         score_diff_entry, score_diff_exit, pitcher_type)

# then join this to the first_ab_allrows (which is one AB per half-inning)
first_ab_allrows_top_ptype <- first_ab_allrows_top_ptype |>
  left_join(
    seren_first_p_top_ptype,
    by = c("game_pk", "about.inning", "about.halfInning", "matchup.pitcher.id")
  )


# summarize across each AB whether *any* row had a bunt
buntattempts_top_ptype <- first_ab_allrows_top_ptype |>
  group_by(game_pk, about.inning, atBatIndex, pitcher_type) |>
  summarise(
    bunt_attempted = any(bunt_flag, na.rm = TRUE),
    .groups = "drop"
  )

ba_ratio_top_ptype <- buntattempts_top_ptype |>
  group_by(pitcher_type) |>
  count(bunt_attempted) |>
  mutate(
    pct = n / sum(n)
  )


# get last row of each AB in extras
last_ab_rows_top_ptype <- first_ab_allrows_top_ptype |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type) |>
  slice_max(pitchNumber, n = 1, with_ties = FALSE) |>
  ungroup()

# then filter and classify bunts only on the final AB rows
bunt_success_top_ptype <- last_ab_rows_top_ptype |>
  filter(bunt_flag == TRUE) |>
  filter(grepl("bunt", result.description, ignore.case = TRUE)) |>
  mutate(
    num_bases_occupied_post = rowSums(
      across(
        c(matchup.postOnFirst.id, matchup.postOnSecond.id, matchup.postOnThird.id),
        ~ !is.na(.x)
      )
    ),
    success_flag = case_when(
      !is.na(matchup.postOnThird.id) ~ TRUE,  # runner advanced to 3rd
      num_bases_occupied_post >= 2 ~ TRUE,    # at least two runners on base
      grepl("scores", result.description, ignore.case = TRUE) ~ TRUE,  # scored
      TRUE ~ FALSE
    )
  ) |>
  mutate(success_flag = ifelse(result.event == "Bunt Pop Out", FALSE, success_flag),
         success_flag = ifelse(result.event == "Strikeout", FALSE, success_flag),
         matchup.postOnFirst.id = ifelse(result.event == "Strikeout", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(result.event == "Bunt Pop Out", NA, matchup.postOnFirst.id),
         matchup.postOnFirst.id = ifelse(
           str_detect(result.description, "out on a sacrifice bunt"),
           NA,
           matchup.postOnFirst.id
         ),
         matchup.postOnSecond.id = ifelse(
           str_detect(result.description, "Andres Gimenez out on a sacrifice bunt, third baseman Alex Bregman to first baseman Jose Abreu. Tyler Freeman to 3rd."),
           NA,
           matchup.postOnSecond.id
         )
  )

bunt_success_summary_top_ptype <- bunt_success_top_ptype |>
  group_by(pitcher_type) |>
  summarise(
    total_bunts = n(),
    successful_bunts = sum(success_flag == TRUE, na.rm = TRUE),
    success_rate = successful_bunts / total_bunts
  )

bunt_event_summary_top_ptype <- bunt_success_top_ptype |>
  mutate(
    bases_start = case_when(
      !is.na(matchup.postOnFirst.id) & !is.na(matchup.postOnThird.id) ~ "1st and 3rd",
      !is.na(matchup.postOnFirst.id) ~ "1st",
      !is.na(matchup.postOnSecond.id) & !is.na(matchup.postOnThird.id) ~ "2nd and 3rd",
      !is.na(matchup.postOnSecond.id) ~ "2nd",
      !is.na(matchup.postOnThird.id) ~ "3rd",
      TRUE ~ "empty"
    ),
    matchup.postOnSecond.id = ifelse(bases_start == "2nd and 3rd", NA, matchup.postOnSecond.id),
    bases_start = ifelse(bases_start == "2nd and 3rd", "3rd", bases_start),
    matchup.postOnSecond.id = ifelse(bases_start == "1st and 3rd", NA, matchup.postOnSecond.id)
  ) |>
  group_by(bases_start, result.event, count.outs.end, pitcher_type) |>
  summarise(
    instances = n(),
    successful_bunts = sum(success_flag, na.rm = TRUE),
    success_rate = successful_bunts / instances * 100,
    .groups = "drop"
  ) |>
  arrange(desc(instances))


# how many runs score when no bunt/failed bunt/success bunt to start inning of tied game

# 1. first AB bunts in tied games
bunt_attempts_0games_top_ptype <- bunt_success_top_ptype |>
  filter(score_diff_entry == 0)

# 2. all first ABs in tied games
firstabs_tied_top_ptype <- first_ab_allrows_top_ptype |>
  filter(score_diff_entry == 0) |>
  distinct(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type)

# 3. get first ABs that did not attempt a bunt
nobunt_attempts_0games_top_ptype <- firstabs_tied_top_ptype |>
  anti_join(
    bunt_attempts_0games_top_ptype |> 
      select(game_pk, about.inning, about.halfInning, atBatIndex, pitcher_type),
    by = c("game_pk", "about.inning", "about.halfInning", "atBatIndex", "pitcher_type")
  )

# 4. attach whether a run scored after that first AB
# (seren data holds the inning-level pitcher rows)
seren_exit_top_ptype <- seren |>
  filter(about.inning >= 10, about.halfInning == "top") |>
  group_by(game_pk, about.inning, about.halfInning, pitcher_type) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  )

# add to each group
bunt_attempts_0games_top_ptype <- bunt_attempts_0games_top_ptype |>
  left_join(seren_exit_top_ptype, 
            by = c("game_pk", "about.inning", "about.halfInning", "pitcher_type"))

nobunt_attempts_0games_top_ptype <- nobunt_attempts_0games_top_ptype |>
  left_join(seren_exit_top_ptype,
            by = c("game_pk", "about.inning", "about.halfInning", "pitcher_type"))

# classify
bunt_attempts_0games_top_ptype <- bunt_attempts_0games_top_ptype |>
  mutate(
    attempt_category = ifelse(success_flag, "successful bunt", "failed bunt"),
    run_scored = ifelse(score_diff_exit.y < score_diff_entry.y, TRUE, FALSE)
  ) |>
  select(
    game_pk, 
    about.inning, 
    about.halfInning, 
    atBatIndex, 
    score_diff_entry = score_diff_entry.y,
    score_diff_exit  = score_diff_exit.y,
    attempt_category, 
    run_scored,
    pitcher_type
  )

nobunt_attempts_0games_top_ptype <- nobunt_attempts_0games_top_ptype |>
  mutate(
    attempt_category = "no bunt attempt",
    run_scored = ifelse(score_diff_exit < score_diff_entry, TRUE, FALSE)
  )

# 5. combine
bunt_run_summary_top_ptype <- bind_rows(bunt_attempts_0games_top_ptype, nobunt_attempts_0games_top_ptype) |>
  group_by(attempt_category, pitcher_type) |>
  summarise(
    n = n(),
    run_scored_n = sum(run_scored, na.rm=TRUE),
    run_scored_pct = run_scored_n / n * 100,
    .groups="drop"
  )

###

# win percentage by bottom inning deficit
inning_results_wlt_ptype <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, pitcher_type) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)], # first pitcher
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],  # last pitcher
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Loss",
      score_diff_exit <  0 ~ "Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  count(score_diff_entry, result, pitcher_type, name = "n") |>
  group_by(score_diff_entry) |>
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) |>
  ungroup()


inning_results_wlt_ptype_trusted <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, pitcher_type) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)], # first pitcher
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],  # last pitcher
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Loss",
      score_diff_exit <  0 ~ "Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  count(score_diff_entry, pitcher_type, result, name = "n") |>
  group_by(score_diff_entry) |>
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) |>
  ungroup()

inning_results_winpct_trusted <- seren |>
  filter(about.inning >= 10, about.halfInning == "bottom") |>
  group_by(game_pk, about.inning, pitcher_type) |>
  summarise(
    score_diff_entry = score_diff_entry[which.min(first_row_index)],
    score_diff_exit  = score_diff_exit[which.max(first_row_index)],
    .groups = "drop"
  ) |>
  mutate(
    result = case_when(
      score_diff_exit >  0 ~ "Home Loss",
      score_diff_exit <  0 ~ "Home Win",
      score_diff_exit == 0 ~ "Tie"
    )
  ) |>
  # count for each score_diff_entry and ta
  count(score_diff_entry, pitcher_type, result, name = "n") |>
  group_by(score_diff_entry, pitcher_type) |>
  mutate(
    total = sum(n)
  ) |>
  ungroup() |>
  mutate(
    pct = n / total * 100
  ) |>
  arrange(score_diff_entry, result, desc(pitcher_type))

#### plot
ggplot(all_bunt_run_summary_ptype |> filter(attempt_category != "failed bunt"), 
       aes(x = pitcher_type, y = run_scored_pct, fill = attempt_category)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  # Percentage label ABOVE bar
  geom_text(
    aes(label = paste0(round(run_scored_pct, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  
  # PA label INSIDE bar
  geom_text(
    aes(label = paste0(n, "\nPA")),
    position = position_dodge(width = 0.9),
    vjust = 1.5,
    size = 3,
    color = "black"
  ) +
  
  scale_fill_manual(
    values = c("successful bunt" = "indianred2", 
               "no bunt attempt" = "gray"),
    labels = c("successful bunt" = "Successful Bunt", 
               "no bunt attempt" = "No Bunt Attempt")
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 100, 20),
    limits = c(0, 105)
  ) +
  
  labs(
    title = "Home Run Scoring Rate by Pitcher Type and Bunt Decision",
    x = "Pitcher Type",
    y = "At Least One Run Scored (%)",
    fill = "Bunt Outcome"
  ) +
  
  theme_minimal(base_size = 11.5) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

###
ggplot(bunt_run_summary_top_ptype |> filter(attempt_category != "failed bunt",
                                            n >= 5), 
       aes(x = pitcher_type, y = run_scored_pct, fill = attempt_category)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  # Percentage label ABOVE bar
  geom_text(
    aes(label = paste0(round(run_scored_pct, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  
  # PA label INSIDE bar
  geom_text(
    aes(label = paste0(n, "\nPA")),
    position = position_dodge(width = 0.9),
    vjust = 1.5,
    size = 3,
    color = "black"
  ) +
  
  scale_fill_manual(
    values = c("successful bunt" = "lightskyblue", 
               "no bunt attempt" = "gray"),
    labels = c("successful bunt" = "Successful Bunt", 
               "no bunt attempt" = "No Bunt Attempt")
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 90, 20),
    limits = c(0, 90)
  ) +
  
  labs(
    title = "Away Run Scoring Rate by Pitcher Type and Bunt Decision",
    x = "Pitcher Type",
    y = "At Least One Run Scored (%)",
    fill = "Bunt Outcome"
  ) +
  
  theme_minimal(base_size = 11.5) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )


### Other charts thought of later

# Calculate overall extra innings record for each team
team_ei_win_pct <- seren |>
  select(game_pk, Team, season, team_result, HomeAway) |>
  unique() |>
  group_by(Team, team_result) |>
  summarise(n = n()) |>
  ungroup()

team_ei_win_pct <- team_ei_win_pct |>
  pivot_wider(names_from = team_result, values_from = n) |>
  select(1,3,2) |>
  mutate(win_pct = Win/ (Win + Loss))

team_ei_win_pct_szn <- seren |>
  select(game_pk, Team, season, team_result) |>
  unique() |>
  group_by(season, Team, team_result) |>
  summarise(n = n()) |>
  ungroup()

team_ei_win_pct_szn <- team_ei_win_pct_szn |>
  pivot_wider(names_from = team_result, values_from = n) |>
  select(1,2,4,3) |>
  mutate(
    Win = ifelse(is.na(Win), 0, Win),
    Loss = ifelse(is.na(Loss), 0, Loss),
    win_pct = Win/ (Win + Loss))

# team records
team_ovr_rec <- read_csv("team_ovr_rec.csv")
team_ovr_szn <- read_csv("szn_rec_team.csv")

# Join by Team
team_ovr_joined <- left_join(team_ovr_rec, team_ei_win_pct, by = "Team", suffix = c("_ovr", "_ei")) |>
  mutate(win_pct_ovr = win_pct_ovr * 100,
         win_pct_ei = win_pct_ei * 100)

ovr_correlation <- cor(team_ovr_joined$win_pct_ovr, team_ovr_joined$win_pct_ei, use = "complete.obs")
cor_test <- cor.test(team_ovr_joined$win_pct_ovr, team_ovr_joined$win_pct_ei, use = "complete.obs")
print(ovr_correlation)
print(cor_test)

ggplot(team_ovr_joined, aes(x = win_pct_ovr, y = win_pct_ei)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text_repel(aes(label = Team), size = 3) +
  labs(
    title = "Team Overall Win % vs Overall Extra-Inning Win %",
    x = "Overall Win %",
    y = "Extra-Inning Win %"
  ) +
  annotate("text", x = 56, y = 66, 
           label = paste0("r = ", round(cor_test$estimate, 3),
                          ", p = ", signif(cor_test$p.value, 3)),
           size = 4, hjust = 0) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, hjust = 0.5))


# Join by Team
team_ovr_joined2 <- left_join(team_ovr_szn, team_ei_win_pct_szn, by = c("Team", "season"), suffix = c("_ovr", "_ei")) |>
  mutate(win_pct_ovr = win_pct_ovr * 100,
         win_pct_ei = win_pct_ei * 100)

ovr_correlation2 <- cor(team_ovr_joined2$win_pct_ovr, team_ovr_joined2$win_pct_ei, use = "complete.obs")
cor_test2 <- cor.test(team_ovr_joined2$win_pct_ovr, team_ovr_joined2$win_pct_ei, use = "complete.obs")
print(ovr_correlation2)
print(cor_test2)

ggplot(team_ovr_joined2, aes(x = win_pct_ovr, y = win_pct_ei)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Team Season Win % vs Season Extra-Inning Win %",
    x = "Season Win %",
    y = "Extra-Inning Win %"
  ) +
  annotate("text", x = 55, y = 90, 
           label = paste0("r = ", round(cor_test2$estimate, 3),
                          ", p = ", signif(cor_test2$p.value, 3)),
           size = 4, hjust = 0) +
  theme_minimal()

###
# Away record focus

# Calculate overall extra innings record for each team
team_ei_win_pct_a <- seren |>
  select(game_pk, Team, season, team_result, HomeAway) |>
  unique() 

team_ei_win_pct_a <- left_join(team_ei_win_pct_a, team_ovr_szn)

team_ei_win_pct_a <- team_ei_win_pct_a |>
  pivot_wider(
    id_cols = game_pk,
    names_from = HomeAway,
    values_from = c(Team, team_result, Win, Loss, win_pct),
    names_sep = "_"
  )

team_ei_comp <- team_ei_win_pct_a |>
  mutate(
    better_team = case_when(
      win_pct_A > win_pct_H ~ "Away",
      win_pct_H > win_pct_A ~ "Home",
      TRUE ~ "Same"
    ),
    winner = case_when(
      team_result_H == "Win" ~ "Home",
      team_result_A == "Win" ~ "Away",
      TRUE ~ "Tie"
    ),
    better_team_won = better_team == winner
  )

ei_sum_stats <- team_ei_comp |>
  count(better_team, winner, better_team_won) |>
  group_by(better_team) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

ei_sum_stats_filt <- ei_sum_stats |>
  filter(better_team != "Same")

write.csv(ei_sum_stats_filt, "ei_sum_stats_filt.csv")


ggplot(ei_sum_stats_filt, aes(x = better_team, y = pct, fill = better_team_won)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.6) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.6),
    vjust = -0.4,
    size = 4.2,  # slightly larger for better print legibility
    color = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),  # higher contrast than green/red
    labels = c("TRUE" = "Better Team Won", "FALSE" = "Better Team Lost")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.6)  # little more space for text above bars
  ) +
  labs(
    title = "Extra-Inning Win % by Better Team Location",
    x = "Better Team Location",
    y = "Win Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.grid.minor = element_line(size = 0.25),
    panel.grid.major = element_line(size = 0.4)
  )

#sample cleaned ramks

sample_cr <- head(lev_pct) 
