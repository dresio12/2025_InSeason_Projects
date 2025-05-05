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
  group_by(game_pk, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  )

ATH <- lbb_with_events |>
  filter(fielding_team == "Athletics")

ATL <- lbb_with_events |>
  filter(fielding_team == "Atlanta Braves")

BAL <- lbb_with_events |>
  filter(fielding_team == "Baltimore Orioles")

BOS <- lbb_with_events |>
  filter(fielding_team == "Boston Red Sox")

CHC <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "Chicago Cubs") |>
  unique()

CHW <- lbb_with_events |>
  filter(fielding_team == "Chicago White Sox")

CIN <- lbb_with_events |>
  filter(fielding_team == "Cincinnati Reds")

CLE <- lbb_with_events |>
  filter(fielding_team == "Cleveland Guardians")

COL <- lbb_with_events |>
  filter(fielding_team == "Colorado Rockies")

DET <- lbb_with_events |>
  filter(fielding_team == "Detroit Tigers")

HOU <- lbb_with_events |>
  filter(fielding_team == "Houston Astros")

KCR <- lbb_with_events |>
  filter(fielding_team == "Kansas City Royals")

LAA <- lbb_with_events |>
  filter(fielding_team == "Los Angeles Angels")

LAD <- lbb_with_events |>
  filter(fielding_team == "Los Angeles Dodgers")

MIA <- lbb_with_events |>
  filter(fielding_team == "Miami Marlins")

MIL <- lbb_with_events |>
  filter(fielding_team == "Milwaukee Brewers")

MIN <- lbb_with_events |>
  filter(fielding_team == "Minnesota Twins")

NYM <- lbb_with_events |>
  filter(fielding_team == "New York Mets")

NYY <- lbb_with_events |>
  filter(fielding_team == "New York Yankees")

PHI <- lbb_with_events |>
  filter(fielding_team == "Philadelphia Phillies")

PIT <- lbb_with_events |>
  filter(fielding_team == "Pittsburgh Pirates")

SDP <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  filter(fielding_team == "San Diego Padres") |>
  unique()

SFG <- lbb_with_events |>
  filter(fielding_team == "San Francisco Giants")

SEA <- lbb_with_events |>
  filter(fielding_team == "Seattle Mariners")

STL <- lbb_with_events |>
  filter(fielding_team == "St. Louis Cardinals")

TBR <- lbb_with_events |>
  filter(fielding_team == "Tampa Bay Rays")

TEX <- lbb_with_events |>
  filter(fielding_team == "Texas Rangers")

TOR <- lbb_with_events |>
  filter(fielding_team == "Toronto Blue Jays")

WSH <- lbb_with_events |>
  filter(fielding_team == "Washington Nationals")

team_runs_allowed <- lbb_with_events |>
  group_by(fielding_team, game_pk, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  ) |>
  group_by(fielding_team) |>
  summarize(total_runs_allowed = sum(runs_allowed), .groups = "drop")

team_leadoff_walks <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  unique() |>
  group_by(fielding_team, game_pk, about.inning, about.halfInning) |>
  slice_min(order_by = atBatIndex, n = 1) |>  # get first event per half-inning
  filter(result.event == "Walk") |>
  ungroup() |>
  count(fielding_team, name = "total_leadoff_walks")

league_summary <- team_leadoff_walks |>
  inner_join(team_runs_allowed, by = "fielding_team")

league_averages <- league_summary |>
  summarise(
    average_leadoff_walks = mean(total_leadoff_walks, na.rm = TRUE),
    average_runs_allowed = mean(total_runs_allowed, na.rm = TRUE)
  )

# Step 1: For each leadoff walk inning, calculate if a run scored
lbb_runs_summary <- lbb_with_events |>
  group_by(game_pk, about.inning, about.halfInning, fielding_team) |>
  summarise(
    runs_scored = if (first(about.halfInning) == "top") {
      max(result.awayScore) - min(result.awayScore)
    } else {
      max(result.homeScore) - min(result.homeScore)
    },
    .groups = "drop"
  ) |>
  mutate(
    run_scored_flag = ifelse(runs_scored > 0, 1, 0)
  )

# Step 2: Calculate % of leadoff walk innings where at least one run scored
team_run_score_rate <- lbb_runs_summary |>
  group_by(fielding_team) |>
  summarise(
    total_leadoff_walk_innings = n(),
    innings_with_run_scored = sum(run_scored_flag),
    run_score_percentage = innings_with_run_scored / total_leadoff_walk_innings * 100
  )

team_mapping <- tibble::tibble(
  full_name = c(
    "Atlanta Braves", "Arizona Diamondbacks", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs", "Cincinnati Reds", 
    "Cleveland Guardians", "Colorado Rockies", "Chicago White Sox", "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
    "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers", "Minnesota Twins", "MLB", 
    "New York Mets", "New York Yankees", "Athletics", "Philadelphia Phillies", "Pittsburgh Pirates", 
    "San Diego Padres", "Seattle Mariners", "San Francisco Giants", "St. Louis Cardinals", "Tampa Bay Rays", 
    "Texas Rangers", "Toronto Blue Jays", "Washington Nationals"
  ),
  abbr = c(
    "ATL", "AZ", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU",
    "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "MLB", "NYM", "NYY", "OAK", "PHI",
    "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH"
  )
)

# Put dataframes into a named list
dfs <- list(
  team_run_score_rate = team_run_score_rate,
  team_leadoff_walks = team_leadoff_walks,
  team_runs_allowed = team_runs_allowed,
  league_summary = league_summary
)

# Run abbreviation replacement across all of them
dfs_updated <- lapply(dfs, function(df) {
  df %>%
    left_join(team_mapping, by = c("fielding_team" = "full_name")) %>%
    mutate(fielding_team = abbr) %>%
    select(-abbr)
})

# Reassign updated dataframes back to their original names
team_run_score_rate <- dfs_updated$team_run_score_rate
team_leadoff_walks <- dfs_updated$team_leadoff_walks
team_runs_allowed <- dfs_updated$team_runs_allowed
league_summary <- dfs_updated$league_summary

league_summary <- league_summary |>
  bind_rows(
    tibble(
      fielding_team = "MLB",
      total_leadoff_walks = 15.9,
      total_runs_allowed = 12.33
    )
  )


mean(team_run_score_rate$total_leadoff_walk_innings)
mean(team_run_score_rate$innings_with_run_scored)
mean(team_run_score_rate$run_score_percentage)

team_run_score_rate <- team_run_score_rate |>
  bind_rows(
    tibble(
      fielding_team = "MLB",
      total_leadoff_walk_innings = 15.9,
      innings_with_run_scored = 6.067,
      run_score_percentage = 36.981
    )
  ) |>
  mutate(run_score_percentage = format(round(run_score_percentage, 2), nsmall = 2))
  
team_run_score_rate$run_score_percentage <- as.numeric(team_run_score_rate$run_score_percentage)

#creating visuals
ggplot(league_summary, aes(total_leadoff_walks, total_runs_allowed)) +
  geom_mlb_logos(aes(team_abbr = fielding_team), width = 0.05) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Leadoff Walks vs Runs Allowed",
    x = "Total Leadoff Walks Issued",
    y = "Total Runs Allowed Post-Leadoff Walk" 
  )

ggplot(team_run_score_rate, aes(total_leadoff_walk_innings, innings_with_run_scored)) +
  geom_mlb_logos(aes(team_abbr = fielding_team), width = 0.05) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Total Innings In Which At Least One Run Scored",
    x = "Total Leadoff Walks Issued",
    y = "Total Innings with >= 1 Run Scored" 
  )

ggplot(team_run_score_rate, aes(x = reorder(fielding_team, -run_score_percentage), y = run_score_percentage)) +
  geom_col(aes(color = fielding_team, fill = fielding_team), width = 0.5) +
  scale_color_mlb(type = "secondary") +
  scale_fill_mlb(alpha = 0.4) +
  geom_text(aes(label = format(round(run_score_percentage, 1), nsmall = 1)),
            vjust = -0.5,
            size = 3
            ) + 
  theme_minimal() +
  labs(
    title = "Run Score Percentage after Leadoff Walk by Team",
    x = "Team",
    y = "Run Score Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    legend.position = "none"
  )
