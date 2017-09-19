library(RCurl)
library(data.table)
library(tidyverse)
library(nflscrapR)

## Read in game outcome data

outcome_2013 <- season_games(2013)
outcome_2014 <- season_games(2014)
outcome_2015 <- season_games(2015)
outcome_2016 <- season_games(2016)

## Read in NFL data

nfl_2016 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/data/season_play_by_play/pbp_2016.csv")
nfl_2015 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/data/season_play_by_play/pbp_2015.csv")
nfl_2014 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/data/season_play_by_play/pbp_2014.csv")
nfl_2013 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/data/season_play_by_play/pbp_2013.csv")
nfl_2016_csv <- read.csv(text = nfl_2016, stringsAsFactors = F) 
nfl_2015_csv <- read.csv(text = nfl_2015, stringsAsFactors = F) 
nfl_2014_csv <- read.csv(text = nfl_2014, stringsAsFactors = F) 
nfl_2013_csv <- read.csv(text = nfl_2013, stringsAsFactors = F)
head(nfl_2016_csv)
## Manually Created Formation Data

setwd('~/Documents/Fantasy/NFL/modeling')
formation <- read.csv('formation_data.csv') %>%
  filter(Formation != 'PUNT' & Formation != 'FIELD GOAL')

## Create stacked files to calc win probabilities.

nfl_win <- as.data.frame(rbind(outcome_2013, rbind(outcome_2014, rbind(outcome_2015, outcome_2016)))) %>% ## Add in 2015/2016 when APIs cooperate
  mutate(home_win = ifelse(homescore > awayscore, 1, 0),
         away_win = ifelse(awayscore > homescore, 1, 0)
         ) %>%
  select(GameID, home, away, home_win, away_win)

home_win <- nfl_win %>%
  select(GameID, home_win) %>%
  mutate(GameID = as.numeric(GameID))

away_win <- nfl_win %>%
  select(GameID, away_win) %>%
  mutate(GameID = as.numeric(GameID))

## Stack data & set up dataframe

nfl <- as.data.frame(rbind(nfl_2016_csv, rbind(nfl_2015_csv, rbind(nfl_2014_csv, nfl_2013_csv))))
nfl_trans <- nfl %>%
  filter(PlayAttempted == 1) %>%
  filter(PlayType %in% c('Pass', 'Run', 'Sack', 'Punt', 'Spike', 'Field Goal', 'QB Kneel')) %>%
  filter(!is.na(down)) %>%
  mutate(posteam = ifelse(posteam == 'JAX', 'JAC', posteam),
         DefensiveTeam = ifelse(DefensiveTeam == 'JAX', 'JAC', DefensiveTeam),
         off_team = paste0(posteam, "_", Season),
         def_team = paste0(DefensiveTeam, "_", Season),
         HomeTeam = ifelse(HomeTeam == 'JAX', 'JAC', HomeTeam),
         AwayTeam = ifelse(AwayTeam == 'JAX', 'JAC', AwayTeam),
         defense_to_pre = ifelse(DefensiveTeam == AwayTeam, AwayTimeouts_Remaining_Pre, HomeTimeouts_Remaining_Pre),
         mins = ifelse(TimeSecs > 2700, floor(TimeSecs / 60) - 45,
                ifelse(TimeSecs <= 2700 & TimeSecs > 1800, floor(TimeSecs / 60) - 30,
                ifelse(TimeSecs <= 1800 & TimeSecs > 900, floor(TimeSecs / 60) - 15,
                ifelse(TimeSecs <= 900, floor(TimeSecs / 60), 0)))),
         secs = TimeSecs %% 60) %>%
  select(GameID, Date, Drive, qtr, down, ydstogo, TimeUnder, PlayType, yrdline100, Touchdown, FirstDown, posteam, DefensiveTeam, PassOutcome, PassLength,
         AirYards, YardsAfterCatch, PassLocation, RunLocation, RushAttempt, RunGap, InterceptionThrown, Reception, Fumble, Sack, PosTeamScore, DefTeamScore,
         HomeTeam, AwayTeam, mins, secs, PosTeamScore, DefTeamScore, Yards.Gained, Season, posteam_timeouts_pre, defense_to_pre, off_team, def_team) %>%
  left_join(formation, by = c('GameID' = 'GameID', 'qtr' = 'qtr', 'mins' = 'mins', 'secs' = 'secs')) %>%
  left_join(home_win, by = c('GameID' = 'GameID')) %>%
  left_join(away_win, by = c('GameID' = 'GameID')) %>%
  mutate(win = ifelse(HomeTeam == posteam, home_win, away_win)) %>%
  select(-GameID, -mins, -secs, -home_win, -away_win)

nfl_trans <- nfl_trans %>%
  mutate(posteam = as.character(posteam),
         Date = as.Date(Date, format = '%Y-%m-%d'),
         DefensiveTeam = as.character(DefensiveTeam),
         HomeTeam = as.character(HomeTeam),
         AwayTeam = as.character(AwayTeam),
         dist_length = ifelse(ydstogo > 7, 'long',
                     ifelse(ydstogo <= 7 & ydstogo > 3, 'intermediate', 'short')),
         two_min = ifelse(TimeUnder <= 2 & (qtr == 2 | qtr == 4), 'two_minute', 'regular'),
         score_diff = ifelse(PosTeamScore - DefTeamScore < -8, 'multi_score_down',
                      ifelse(PosTeamScore - DefTeamScore < 0 & PosTeamScore - DefTeamScore >= -8, 'one_score_down',
                      ifelse(PosTeamScore == DefTeamScore, 'tied',
                      ifelse(PosTeamScore - DefTeamScore > 0 & PosTeamScore - DefTeamScore <= 8, 'one_score_up',
                             'multi_score_up')))),
         score_diff_raw = PosTeamScore - DefTeamScore,
         team_status = ifelse(posteam == HomeTeam, 1, 0),
         field_status = ifelse(yrdline100 < 20, 'inside_own_20',
                        ifelse(yrdline100 >= 21 & yrdline100 < 50, 'own_20_49',
                        ifelse(yrdline100 >= 50 & yrdline100 < 80, 'opp_50_21',
                        'redzone'))),
         play_type = PlayType,
         NONE = 'NONE',
         play_location = ifelse(is.na(PassLocation), RunLocation, PassLocation),
         turnover = ifelse(InterceptionThrown > 0 | Fumble > 0, 'Turnover', 'No Turnover')) %>%
  select(-PlayType, -PosTeamScore, -DefTeamScore, -AirYards) %>%
  select(posteam, NONE, everything())

## Save for visualization

setwd('~/Documents/Fantasy/NFL')
write.csv(nfl_trans, 'nfl_model_2013_2016.csv', row.names = F)

## NFL drive stuff

## Summarize by play & other stuff

# ## Not using because not relevant for real time updates
# 
# nfl_summ <- nfl_trans %>%
#   filter(play_type %in% c('Pass', 'Run')) %>%
#   filter(!is.na(Formation)) %>%
#   mutate(pass = ifelse(play_type == 'Pass', 1, 0),
#          run = ifelse(play_type == 'Run', 1, 0)) %>%
#   group_by(Date, off_team, down, Formation) %>%
#     summarise(passes = sum(pass),
#               runs = sum(run)) %>%
#   as.data.frame()
# 
# ## Defenses
# 
# nfl_defs <- nfl_trans %>%
#   filter(play_type %in% c('Pass', 'Run')) %>%
#   filter(!is.na(Formation)) %>%
#   mutate(pass = ifelse(play_type == 'Pass', 1, 0),
#          run = ifelse(play_type == 'Run', 1, 0)) %>%
#   group_by(Date, def_team, down, Formation) %>%
#   summarise(passes = sum(pass),
#             runs = sum(run)) %>%
#   as.data.frame()
# 
# ## find unique date team combos
# 
# nfl_groups <- nfl_summ %>%
#   group_by(Date, off_team, down, Formation) %>%
#   summarise(dum = n()) %>%
#   as.data.frame() %>%
#   select(-dum) %>%
#   arrange(off_team, Date)
# 
# nfl_def <- nfl_defs %>%
#   group_by(Date, def_team, down, Formation) %>%
#   summarise(dum = n()) %>%
#   as.data.frame() %>%
#   select(-dum) %>%
#   arrange(def_team, Date)
# 
# ## For loop for every date - team/year combo for offense & defense
# ## Super inefficient but gets job done - avoid running
# 
# nfl_groups$passes <- 0
# nfl_groups$runs <- 0
# nfl_def$passes <- 0
# nfl_def$runs <- 0
# i <- 1
# for(i in 1:nrow(nfl_groups)) {
#   f <- filter(nfl_summ, off_team == nfl_groups[i,2])
#   f <- filter(f, Date < nfl_groups[i,1])
#   f <- filter(f, down == nfl_groups[i,3])
#   f <- filter(f, Formation == nfl_groups[i,4])
#   nfl_groups[i,5] <- sum(f$passes) / (sum(f$passes) + sum(f$runs))
#   nfl_groups[i,6] <- sum(f$runs) / (sum(f$passes) + sum(f$runs))
# }
# 
# i <- 1
# for(i in 1:nrow(nfl_def)) {
#   f <- filter(nfl_summ, off_team == nfl_def[i,2])
#   f <- filter(f, Date < nfl_def[i,1])
#   f <- filter(f, down == nfl_def[i,3])
#   f <- filter(f, Formation == nfl_def[i,4])
#   nfl_def[i,5] <- sum(f$passes) / (sum(f$passes) + sum(f$runs))
#   nfl_def[i,6] <- sum(f$runs) / (sum(f$passes) + sum(f$runs))
# }
# 
# nfl_groups$passes <- nfl_groups$passes / (nfl_groups$passes + nfl_groups$runs)
# nfl_groups$runs <- nfl_groups$runs / (nfl_groups$passes + nfl_groups$runs)
# nfl_groups[is.na(nfl_groups)] <- 0
# nfl_def$passes <- nfl_def$passes / (nfl_def$passes + nfl_def$runs)
# nfl_def$runs <- nfl_def$runs / (nfl_def$passes + nfl_def$runs)
# nfl_def[is.na(nfl_def)] <- 0
# nfl_def$passes_def <- nfl_def$passes
# nfl_def$runs_def <- nfl_def$runs
# nfl_def <- nfl_def %>%
#   select(-passes, -runs)
# 
# ## join summaries back w/ full data set
#   
# nfl_trans <- nfl_trans %>%
#   left_join(nfl_groups, by = c('off_team' = 'off_team', 'down' = 'down', 
#                                'Formation' = 'Formation', 'Date' = 'Date')) %>%
#   left_join(nfl_def, by = c('def_team' = 'def_team', 'down' = 'down', 
#                             'Formation' = 'Formation', 'Date' = 'Date')) %>%
#   select(-Date)



