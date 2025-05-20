library(tidyverse)
library(baseballr)
library(ggplot2)
library(mlbplotR)
library(stringr)
library(tidyr)

#load in csv
pbp <- readRDS("pbp_progress.rds")

#fix letters
pbp <- pbp %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Ã±", "n")),
         across(where(is.character), ~ str_replace_all(., "Ã©", "e")),
         across(where(is.character), ~ str_replace_all(., "Ã³", "o")),
         across(where(is.character), ~ str_replace_all(., "Ã¡", "a")),
         across(where(is.character), ~ str_replace_all(., "Ãº", "u")),
         across(where(is.character), ~ str_replace_all(., "Ã", "i")))

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

#by team, obtain inning leadoff event counts (specific events)
lec <- first_event_in_inning |>
  count(game_pk, game_date, fielding_team, result.event, matchup.pitcher.fullName, sort = TRUE) |>
  pivot_wider(
    names_from = result.event,
    values_from = n,
    values_fill = 0  # fill missing combinations with 0
  ) |>
  arrange(fielding_team, game_date, matchup.pitcher.fullName)

#create Leadoff_Innings column
lec$Leadoff_Innings <- rowSums(lec[ , !(names(lec) %in% c("game_pk", "game_date", "fielding_team", "matchup.pitcher.fullName"))])

#batting average against addition
lec <- lec |>
  mutate(AB = Leadoff_Innings -  (Walk + `Intent Walk` + `Hit By Pitch` + `Sac Bunt` + `Catcher Interference`),
         H = Single + Double + Triple + `Home Run`) |>
  group_by(matchup.pitcher.fullName) |>
  mutate(
    cAB   = cumsum(AB),
    cH    = cumsum(H),
    c1B   = cumsum(Single),
    c2B   = cumsum(Double),
    c3B   = cumsum(Triple),
    cHR   = cumsum(`Home Run`),
    cBB   = cumsum(Walk),
    cIBB  = cumsum(`Intent Walk`),
    cHBP  = cumsum(`Hit By Pitch`),
    cSO   = cumsum(Strikeout),
    cGO   = cumsum(Groundout),
    cFO   = cumsum(Flyout),
    cPO   = cumsum(`Pop Out`),
    cLD   = cumsum(Lineout),
    cLO_BF = cumsum(Leadoff_Innings),
    
    BAA  = round(cH / cAB, 3),
    OBP  = round((cH + cBB + cIBB + cHBP) / (cAB + cBB + cIBB + cHBP), 3),
    SLG  = round((c1B + 2 * c2B + 3 * c3B + 4 * cHR) / cAB, 3),
    OPS  = round(OBP + SLG, 3),
    wOBA = round((.697 * cBB + .728 * cHBP + .891 * c1B + 
                     1.268 * c2B + 1.607 * c3B + 2.072 * cHR) /
                    (cAB + cBB + cHBP), 3),
    
    K_pct  = round(cSO / cLO_BF * 100, 2),
    BB_pct = round(cBB / cLO_BF * 100, 2),
    HR_pct = round(cHR / cLO_BF * 100, 2),
    GO_pct = round(cGO / cLO_BF * 100, 2),
    FB_pct = round((cFO + cPO) / cLO_BF * 100, 2),
    LD_pct = round(cLD / cLO_BF * 100, 2)
  ) |>
  ungroup()


#Team LEC

# Summarize per team per game
team_game <- lec |>
  group_by(fielding_team, game_pk, game_date) |> 
  summarise(
    AB             = sum(AB),
    H              = sum(H),
    Single         = sum(Single),
    Double         = sum(Double),
    Triple         = sum(Triple),
    `Home Run`     = sum(`Home Run`),
    Walk           = sum(Walk),
    `Intent Walk`  = sum(`Intent Walk`),
    `Hit By Pitch` = sum(`Hit By Pitch`),
    Strikeout      = sum(Strikeout),
    Groundout      = sum(Groundout),
    Flyout         = sum(Flyout),
    `Pop Out`      = sum(`Pop Out`),
    Lineout        = sum(Lineout),
    Leadoff_Innings = sum(Leadoff_Innings),
    .groups = "drop"
  )

# Add cumulative columns
tlec <- team_game |>
  arrange(fielding_team, game_date) |>  
  group_by(fielding_team) |>
  mutate(
    G = row_number(),
    
    cAB   = cumsum(AB),
    cH    = cumsum(H),
    c1B   = cumsum(Single),
    c2B   = cumsum(Double),
    c3B   = cumsum(Triple),
    cHR   = cumsum(`Home Run`),
    cBB   = cumsum(Walk),
    cIBB  = cumsum(`Intent Walk`),
    cHBP  = cumsum(`Hit By Pitch`),
    cSO   = cumsum(Strikeout),
    cGO   = cumsum(Groundout),
    cFO   = cumsum(Flyout),
    cPO   = cumsum(`Pop Out`),
    cLD   = cumsum(Lineout),
    cLO_BF = cumsum(Leadoff_Innings),
    
    BAA  = round(cH / cAB, 3),
    OBP  = round((cH + cBB + cIBB + cHBP) / (cAB + cBB + cIBB + cHBP), 3),
    SLG  = round((c1B + 2 * c2B + 3 * c3B + 4 * cHR) / cAB, 3),
    OPS  = round(OBP + SLG, 3),
    wOBA = round((.697 * cBB + .728 * cHBP + .891 * c1B +
                         1.268 * c2B + 1.607 * c3B + 2.072 * cHR) /
                        (cAB + cBB + cHBP), 3),
    
    K_pct  = round(cSO / cLO_BF * 100, 2),
    BB_pct = round(cBB / cLO_BF * 100, 2),
    HR_pct = round(cHR / cLO_BF * 100, 2),
    GO_pct = round(cGO / cLO_BF * 100, 2),
    FB_pct = round((cFO + cPO) / cLO_BF * 100, 2),
    LD_pct = round(cLD / cLO_BF * 100, 2)
  ) |>
  ungroup()

lec <- lec |>
  group_by(matchup.pitcher.fullName) |>
  mutate(G = row_number()) |>
  ungroup()

#ADD in MLB as cumulative league average
lec_mlb <- lec |>
  group_by(game_date) |>
  summarise(
    AB = sum(AB),
    H = sum(H),
    Single = sum(Single),
    Double = sum(Double),
    Triple = sum(Triple),
    `Home Run` = sum(`Home Run`),
    Walk = sum(Walk),
    `Intent Walk` = sum(`Intent Walk`),
    `Hit By Pitch` = sum(`Hit By Pitch`),
    Strikeout = sum(Strikeout),
    Groundout = sum(Groundout),
    Flyout = sum(Flyout),
    `Sac Bunt` = sum(`Sac Bunt`),
    `Pop Out` = sum(`Pop Out`),
    Lineout = sum(Lineout),
    `Field Error` = sum(`Field Error`),
    `Bunt Groundout` = sum(`Bunt Groundout`),
    `Fielders Choice Out` = sum(`Fielders Choice Out`),
    `Bunt Pop Out` = sum(`Bunt Pop Out`),
    `Double Play` = sum(`Double Play`),
    `Catcher Interference` = sum(`Catcher Interference`),
    Leadoff_Innings = sum(Leadoff_Innings),
    .groups = "drop"
  ) |>
  mutate(
    cAB = cumsum(AB),
    cH = cumsum(H),
    c1B = cumsum(Single),
    c2B = cumsum(Double),
    c3B = cumsum(Triple),
    cHR = cumsum(`Home Run`),
    cBB = cumsum(Walk),
    cIBB = cumsum(`Intent Walk`),
    cHBP = cumsum(`Hit By Pitch`),
    cSO = cumsum(Strikeout),
    cGO = cumsum(Groundout),
    cFO = cumsum(Flyout),
    cPO = cumsum(`Pop Out`),
    cLD = cumsum(Lineout),
    cLO_BF = cumsum(Leadoff_Innings)
  ) |>
  mutate(
    BAA = round(cH / cAB, 3),
    OBP = round((cH + cBB + cIBB + cHBP) / (cAB + cBB + cIBB + cHBP), 3),
    SLG = round((c1B + 2 * c2B + 3 * c3B + 4 * cHR) / cAB, 3),
    OPS = round(OBP + SLG, 3),
    wOBA = round((.697 * cBB + .728 * cHBP + .891 * c1B +
                    1.268 * c2B + 1.607 * c3B + 2.072 * cHR) /
                   (cAB + cBB + cHBP), 3),
    K_pct  = round(cSO / cLO_BF * 100, 2),
    BB_pct = round(cBB / cLO_BF * 100, 2),
    HR_pct = round(cHR / cLO_BF * 100, 2),
    GO_pct = round(cGO / cLO_BF * 100, 2),
    FB_pct = round((cFO + cPO) / cLO_BF * 100, 2),
    LD_pct = round(cLD / cLO_BF * 100, 2),
    
    matchup.pitcher.fullName = "MLB",
    fielding_team = "MLB",
    G = row_number()
  )



lec <- bind_rows(lec, lec_mlb)
tlec <- bind_rows(tlec, lec_mlb) |>
  select(-46:-53)

#Save
write.csv(tlec, "tlec.csv", row.names = FALSE)

write.csv(lec, "lec.csv", row.names = FALSE)
