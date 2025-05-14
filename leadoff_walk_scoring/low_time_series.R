library(tidyverse)
library(baseballr)
library(ggplot2)
library(mlbplotR)

#load in csv
pbp <- readRDS("pbp_progress.rds")

#organize data to be first pitch to last
pbp <- pbp |>
  filter(!is.na(pitchNumber)) 

pbp$atBatIndex <- as.numeric(pbp$atBatIndex)

pbp <- pbp |>
  arrange(game_date, game_pk, atBatIndex, pitchNumber)

# Identify the first event of each half inning
first_event_in_inning <- pbp |>
  group_by(game_pk, about.inning, about.halfInning) |>
  mutate(row_num = row_number()) |>
  filter(row_num == 1) |>  # Get the first event (smallest row_num) in each half inning
  ungroup()

# Filter leadoff walks by checking the first event
leadoff_walks <- first_event_in_inning |>
  filter(result.event == "Walk" & 
           count.outs.end == 0 & 
           is.na(matchup.postOnFirst.fullName) &
           is.na(matchup.postOnSecond.fullName) &
           is.na(matchup.postOnThird.fullName)
  ) 

#pull rows within half-inning after the leadoff walk
lbb_with_events <- pbp |>
  semi_join(leadoff_walks, by = c("game_pk", "about.inning", "about.halfInning")) |>
  arrange(game_pk, about.inning, about.halfInning, atBatIndex) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  filter(atBatIndex >= first(atBatIndex)) |>
  ungroup() |>
  arrange(game_date, game_pk, atBatIndex, pitchNumber)


#pull data for all teams individually
ARI <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Arizona Diamondbacks") |>
  unique()

ARI_runs_allowed <- ARI |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

ATH <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Athletics") |>
  unique()

ATH_runs_allowed <- ATH |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

ATL <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Atlanta Braves") |>
  unique()

ATL_runs_allowed <- ATL |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

BAL <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Baltimore Orioles") |>
  unique()

BAL_runs_allowed <- BAL |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

BOS <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Boston Red Sox") |>
  unique()

BOS_runs_allowed <- BOS |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

CHC <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Chicago Cubs") |>
  unique()

CHC_runs_allowed <- CHC |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

CHW <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Chicago White Sox") |>
  unique()

CHW_runs_allowed <- CHW |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

CIN <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Cincinnati Reds") |>
  unique()

CIN_runs_allowed <- CIN |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

CLE <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Cleveland Guardians") |>
  unique()

CLE_runs_allowed <- CLE |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

COL <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Colorado Rockies") |>
  unique()

COL_runs_allowed <- COL |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

DET <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Detroit Tigers") |>
  unique()

DET_runs_allowed <- DET |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

HOU <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Houston Astros") |>
  unique()

HOU_runs_allowed <- HOU |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

KCR <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Kansas City Royals") |>
  unique()

KCR_runs_allowed <- KCR |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

LAA <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Los Angeles Angels") |>
  unique()

LAA_runs_allowed <- LAA |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

LAD <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Los Angeles Dodgers") |>
  unique()

LAD_runs_allowed <- LAD |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

MIA <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Miami Marlins") |>
  unique()

MIA_runs_allowed <- MIA |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

MIL <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Milwaukee Brewers") |>
  unique()

MIL_runs_allowed <- MIL |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

MIN <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Minnesota Twins") |>
  unique()

MIN_runs_allowed <- MIN |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

NYM <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "New York Mets") |>
  unique()

NYM_runs_allowed <- NYM |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )


NYY <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "New York Yankees") |>
  unique()

NYY_runs_allowed <- NYY |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

PHI <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Philadelphia Phillies") |>
  unique()

PHI_runs_allowed <- PHI |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

PIT <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Pittsburgh Pirates") |>
  unique()

PIT_runs_allowed <- PIT |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

SDP <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "San Diego Padres") |>
  unique()

SDP_runs_allowed <- SDP |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

SFG <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "San Francisco Giants") |>
  unique()

SFG_runs_allowed <- SFG |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

SEA <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Seattle Mariners") |>
  unique()

SEA_runs_allowed <- SEA |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

STL <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "St. Louis Cardinals") |>
  unique()

STL_runs_allowed <- STL |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

TBR <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Tampa Bay Rays") |>
  unique()

TBR_runs_allowed <- TBR |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

TEX <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Texas Rangers") |>
  unique()

TEX_runs_allowed <- TEX |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

TOR <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Toronto Blue Jays") |>
  unique()

TOR_runs_allowed <- TOR |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

WSH <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Washington Nationals") |>
  unique()

WSH_runs_allowed <- WSH |>
  group_by(fielding_team, game_pk, game_date, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )


#pull a row for all games played for all teams to create Game # column
GP <- pbp |>
  select(1,2,fielding_team) |>
  unique() |>
  arrange(fielding_team, game_date) |>
  group_by(fielding_team) |>
  mutate(G = row_number()) 


#combine all rows into one df
low_data <- bind_rows(
  ARI_runs_allowed, ATH_runs_allowed, ATL_runs_allowed,
  BAL_runs_allowed, BOS_runs_allowed, CHC_runs_allowed, 
  CHW_runs_allowed, CIN_runs_allowed, CLE_runs_allowed,
  COL_runs_allowed, DET_runs_allowed, HOU_runs_allowed,
  KCR_runs_allowed, LAA_runs_allowed, LAD_runs_allowed,
  MIA_runs_allowed, MIL_runs_allowed, MIN_runs_allowed,
  NYM_runs_allowed, NYY_runs_allowed, PHI_runs_allowed,
  PIT_runs_allowed, SDP_runs_allowed, SEA_runs_allowed,
  SFG_runs_allowed, STL_runs_allowed, TBR_runs_allowed,
  TEX_runs_allowed, TOR_runs_allowed, WSH_runs_allowed
)

#Aggregate multiple innings in a game to one game
low_data <- low_data |>
  group_by(fielding_team, game_pk, game_date) |>
  summarise(
    ingame_leadoff_walks = n(),
    runs_scored_after_walk = sum(runs_allowed),
    .groups = "drop"
  )

#bring in empty rows
low_data <- left_join(GP, low_data) |>
  mutate(
    ingame_leadoff_walks = ifelse(is.na(ingame_leadoff_walks),
                                  0,
                                  ingame_leadoff_walks),
    runs_scored_after_walk = ifelse(is.na(runs_scored_after_walk),
                                  0,
                                  runs_scored_after_walk),
  )

#add running percentage for run scored percentage after walk
low_data <- low_data |>
  arrange(fielding_team, game_date) |>
  group_by(fielding_team) |>
  mutate(
    tot_walks = cumsum(ingame_leadoff_walks),
    run_scored_num = ifelse(runs_scored_after_walk >= 1, 1, 0),  # 1 if a run scored, 0 if not
    run_scored_tot = cumsum(run_scored_num),  # running total of "yes" outcomes
    run_scored_pct = round((run_scored_tot / tot_walks) * 100, 1)
  ) |>
  ungroup()

#Create MLB-level rows by date for leage average
mlb_rows <- low_data |>
  group_by(game_date) |>
  summarise(
    fielding_team = "MLB",
    ingame_leadoff_walks = sum(ingame_leadoff_walks, na.rm = TRUE),
    runs_scored_after_walk = sum(runs_scored_after_walk, na.rm = TRUE),
    run_scored_num = sum(run_scored_num, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tot_walks = cumsum(ingame_leadoff_walks),
    run_scored_tot = cumsum(run_scored_num),
    run_scored_pct = round((run_scored_tot / tot_walks) * 100, 1),
    G = row_number()
        )

#Bind to existing data
low_data <- bind_rows(low_data, mlb_rows) |>
  arrange(fielding_team, game_date) 


#save
write.csv(low_data, "low_data.csv", row.names = FALSE)
