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
  count(fielding_team, result.event, matchup.pitcher.fullName, sort = TRUE) |>
  pivot_wider(
    names_from = result.event,
    values_from = n,
    values_fill = 0  # fill missing combinations with 0
  ) |>
  arrange(fielding_team, matchup.pitcher.fullName)

#create Leadoff_Innings column
lec$Leadoff_Innings <- rowSums(lec[ , !(names(lec) %in% c("fielding_team", "matchup.pitcher.fullName"))])

#batting average against addition
lec <- lec |>
  mutate(
    AB = Leadoff_Innings -  (Walk + `Intent Walk` + `Hit By Pitch` + `Sac Bunt` 
                   + `Catcher Interference`),
    
    H = Single + Double + Triple + `Home Run`,
                   
    BAA = H / AB,

    OBP = (H + Walk + `Intent Walk` + `Hit By Pitch`) / 
          (AB + Walk + `Intent Walk` + `Hit By Pitch`),
    
    SLG = (Single + (2 * Double) + (3 * Triple) + 4 * `Home Run`) / AB,
    
    OPS = OBP + SLG,
    
    wOBA = ((.697 * Walk) + (.728 * `Hit By Pitch`) + (.891 * Single) + 
              (1.268 * Double) + (1.607 * Triple) + (2.072 * `Home Run`)) /
              (AB + Walk + `Hit By Pitch`),
    
    K_pct = Strikeout / Leadoff_Innings * 100,
    
    BB_pct = Walk / Leadoff_Innings * 100,
    
    HR_pct = `Home Run` / Leadoff_Innings * 100,
    
    GO_pct = Groundout / Leadoff_Innings * 100,
    
    FB_pct = (Flyout + `Pop Out`) / Leadoff_Innings * 100,
    
    LD_pct = Lineout / Leadoff_Innings * 100
            
  )

#Team LEC
tlec <- lec |>
  group_by(fielding_team) |>
  summarise(
    Leadoff_BF = sum(Leadoff_Innings),
    
    AB = Leadoff_BF - (sum(Walk) + sum(`Intent Walk`) + sum(`Hit By Pitch`) + sum(`Sac Bunt`) 
                       + sum(`Catcher Interference`)),
    
    H = sum(Single) + sum(Double) + sum(Triple) + sum(`Home Run`),
    
    BAA = H / AB,
    
    OBP = (H + sum(Walk) + sum(`Intent Walk`) + sum(`Hit By Pitch`)) / 
      (AB + sum(Walk) + sum(`Intent Walk`) + sum(`Hit By Pitch`)),
    
    SLG = (sum(Single) + (2 * sum(Double)) + (3 * sum(Triple)) + 4 * sum(`Home Run`)) / AB,
    
    OPS = OBP + SLG,
    
    wOBA = ((.697 * sum(Walk)) + (.728 * sum(`Hit By Pitch`)) + (.891 * sum(Single)) + 
              (1.268 * sum(Double)) + (1.607 * sum(Triple)) + (2.072 * sum(`Home Run`))) /
      (AB + sum(Walk) + sum(`Hit By Pitch`)),
    
    K_pct = sum(Strikeout) / Leadoff_BF * 100,
    
    BB_pct = sum(Walk) / Leadoff_BF * 100,
    
    HR_pct = sum(`Home Run`) / Leadoff_BF * 100,
    
    GO_pct = sum(Groundout) / Leadoff_BF * 100,
    
    FB_pct = (sum(Flyout) + sum(`Pop Out`)) / Leadoff_BF * 100,
    
    LD_pct = sum(Lineout) / Leadoff_BF * 100
  )


#Save
write.csv(tlec, "tlec.csv", row.names = FALSE)

write.csv(lec, "lec.csv", row.names = FALSE)
