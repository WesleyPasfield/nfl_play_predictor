
## Play type - includes direction of play
library(DescTools)

nfl_model_play <- nfl_trans %>%
  mutate(dv = paste0(play_type, "_", play_location),
         Formation = factor(Formation)) %>%
  select(-Reception, -Fumble, -Sack, -turnover, -NONE, -Touchdown,
         -FirstDown, -PassOutcome, -PassLength, -YardsAfterCatch, -PassLocation,
         -RunLocation, -RushAttempt, -RunGap, -InterceptionThrown, -play_type, -play_location,
         -Yards.Gained, -posteam, -DefensiveTeam, -Season, -Date, -HomeTeam, -AwayTeam, -Drive, -win) %>%
  filter(dv %in% c('Pass_right', 'Pass_left', 'Pass_middle', 'Run_right', 'Run_left', 'Run_middle'))

## create lookup for teams for future eval
## Run XGBoost model predicting type & direction likelihood based on scenario

## Pull in actual result data to try & predict outcome as well?
## Keep raw data rather than categorical for score_diff, dist_length? Both?

nfl_model_play <- nfl_model_play %>%
  mutate(off_team = as.numeric(as.factor(off_team)),
         def_team = as.numeric(as.factor(def_team)),
         Formation = as.numeric(as.factor(Formation)),
         dist_length = as.numeric(as.factor(dist_length)),
         two_min = as.numeric(as.factor(two_min)),
         score_diff = as.numeric(as.factor(score_diff)),
         field_status = as.numeric(as.factor(field_status)),
         dv = as.numeric(as.factor(dv)) - 1) %>%
  filter(!is.na(Formation))

library(xgboost)
library(Matrix)

## Make split index for train/test

train_index <- sample(1:nrow(nfl_model_play), nrow(nfl_model_play) * 0.7)

## Create modeling data sets

play_vars <- as.matrix(nfl_model_play[,-length(nfl_model_play)])
play_label <- nfl_model_play[,'dv']
play_matrix <- xgb.DMatrix(data = as.matrix(nfl_model_play), label = play_label)

## Split into train & test

# Train

play_train <- play_vars[train_index,]
play_train_label <- play_label[train_index]
play_train_matrix <- xgb.DMatrix(data = play_train, label = play_train_label)

# Test

play_test <- play_vars[-train_index,]
play_test_label <- play_label[-train_index]
play_test_matrix <- xgb.DMatrix(data = play_test, label = play_test_label)

## CV Fold & Model Parameters

numberOfClasses <- length(unique(nfl_model_play$dv))
xgb_params_play_me <- list("objective" = "multi:softprob",
                   "eval_metric" = "merror",
                   "num_class" = numberOfClasses,
                   "colsample_bytree" = 0.65,
                   "eta" = .04,
                   "max_depth" = 5,
                   "subsample" = .7)
nround_play    <- 200 # number of XGBoost rounds
cv.nfold_play  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model_play <- xgb.cv(params = xgb_params_play_me,
                   data = play_train_matrix, 
                   nrounds = nround_play,
                   nfold = cv.nfold_play,
                   verbose = FALSE,
                   prediction = TRUE)
cv_model_play[[1]]
OOF_prediction_play <- data.frame(cv_model_play$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = play_train_label + 1,
         pass_prob = X1 + X2 + X3,
         run_prob = X4 + X5 + X6,
         max_prob_bin = ifelse(pass_prob >= run_prob, 1, 0),
         label_bin = ifelse(play_train_label + 1 < 4, 1, 0))
head(OOF_prediction_play, 25)

library(caret)

confusionMatrix(factor(OOF_prediction_play$label),
                factor(OOF_prediction_play$max_prob)
)

confusionMatrix(factor(OOF_prediction_play$label_bin),
                factor(OOF_prediction_play$max_prob_bin)
)

model_play <- xgboost(data = play_train_matrix,
                          label = play_train_label,
                          eta = 0.04,
                          max_depth = 5,
                          subsample = 0.70,
                          "colsample_bytree" = 0.65,
                          nrounds = 250,
                          "objective" = "multi:softprob",
                   "eval_metric" = "merror",
                   "num_class" = numberOfClasses
)

play_importance_mat <- xgb.importance(colnames(play_vars),
                                     model = model_play)
play_importance_mat

setwd('~/Documents/Fantasy/NFL/modeling')
xgb.save(model_play, fname = 'play_direction_model.model')

## Play type - run or pass

nfl_model_play_bin <- nfl_trans %>%
  mutate(dv = play_type,
         Formation = factor(Formation)) %>%
  select(-Reception, -Fumble, -Sack, -turnover, -NONE, -Touchdown,
         -FirstDown, -PassOutcome, -PassLength, -YardsAfterCatch, -PassLocation,
         -RunLocation, -RushAttempt, -RunGap, -InterceptionThrown, -play_type, -play_location,
         -Yards.Gained, -posteam, -DefensiveTeam, -Season, -Date, -Drive,
         -HomeTeam, - AwayTeam, -win) %>%
  filter(dv %in% c('Pass', 'Run'))

library(splitstackshape)

## Create lookup of teams for preds

team_lookup <- nfl_trans %>%
  select(off_team, Season) %>%
  mutate(off_team_lookup = off_team,
         off_team = as.numeric(as.factor(off_team))) %>%
  group_by(off_team_lookup, off_team) %>%
  summarise(dum = n()) %>%
  as.data.frame %>%
  select(-dum) 

team_lookup <- cSplit(team_lookup, "off_team_lookup", "_")
team_lookup <- team_lookup %>%
  mutate(off_team_lookup = paste0(off_team_lookup_1, "_", off_team_lookup_2)) %>%
  select(off_team_lookup, off_team, off_team_lookup_2,off_team_lookup_1) 
colnames(team_lookup)[3] <- 'season'
colnames(team_lookup)[4] <- 'team_name'

setwd('~/Documents/Fantasy/NFL/modeling')
write.csv(team_lookup,'team_lookup.csv', row.names = F)

## Create matrix ready variables for xgboost

nfl_model_play_bin <- nfl_model_play_bin %>%
           mutate(off_team = as.numeric(as.factor(off_team)),
                  def_team = as.numeric(as.factor(def_team)),
                  Formation = as.numeric(as.factor(Formation)),
                  dist_length = as.numeric(as.factor(dist_length)),
                  two_min = as.numeric(as.factor(two_min)),
                  score_diff = as.numeric(as.factor(score_diff)),
                  team_status = as.numeric(as.factor(team_status)),
                  field_status = as.numeric(as.factor(field_status)),
                  dv = as.numeric(as.factor(dv)) - 1) %>%
           filter(!is.na(Formation))

## Make split index for train/test

train_bin_index <- sample(1:nrow(nfl_model_play_bin), nrow(nfl_model_play_bin) * 0.7)

## Create modeling data sets

play_bin_vars <- as.matrix(nfl_model_play_bin[,-length(nfl_model_play_bin)])
play_bin_label <- nfl_model_play_bin[,'dv']
play_bin_matrix <- xgb.DMatrix(data = as.matrix(nfl_model_play_bin), label = play_bin_label)

## Split into train & test

# Train

play_bin_train <- play_bin_vars[train_bin_index,]
play_bin_train_label <- play_bin_label[train_bin_index]
play_bin_train_matrix <- xgb.DMatrix(data = play_bin_train, label = play_bin_train_label)

# Test

play_bin_test <- play_bin_vars[-train_bin_index,]
play_bin_test_label <- play_bin_label[-train_bin_index]
play_bin_test_matrix <- xgb.DMatrix(data = play_bin_test, label = play_bin_test_label)

## Mess w/ different eval functions

xgb_params_play_bin <- list("objective" = "binary:logistic",
                        "eval_metric" = "logloss",
                        "colsample_bytree" = 0.65,
                        "eta" = .05,
                        "max_depth" = 5,
                        "subsample" = .7)
nround_play_bin    <- 250 # number of XGBoost rounds
cv.nfold_play_bin  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model_play_bin <- xgb.cv(params = xgb_params_play_bin,
                        data = play_bin_train_matrix, 
                        nrounds = nround_play_bin,
                        nfold = cv.nfold_play_bin,
                        verbose = FALSE,
                        prediction = TRUE)
cv_model_play_bin[[1]]
OOF_prediction_play_bin <- data.frame(cv_model_play_bin$pred) %>%
  mutate(max_prob_bin = ifelse(cv_model_play_bin.pred >= .5, 1, 0),
         label_bin = play_bin_train_label)

confusionMatrix(factor(OOF_prediction_play_bin$label_bin),
                factor(OOF_prediction_play_bin$max_prob_bin)
)

## Feature importance

model_play_bin <- xgboost(data = play_bin_train,
                          label = play_bin_train_label,
                          max_depth = 5,
                          eta = 0.05,
                          subsample = 0.70,
                          colsample_bytree = 0.65,
                          nrounds = 250,
                          objective = 'binary:logistic',
                          eval_metric = 'logloss'
)

bin_importance_mat <- xgb.importance(colnames(play_bin_vars),
                                     model = model_play_bin)
bin_importance_mat

man_bin_test <- ifelse(predict(model_play_bin, play_bin_test_matrix) >= 0.5, 1, 0)
confusionMatrix(factor(play_bin_test_label),
                factor(man_bin_test), positive = '1')

setwd('~/Documents/Fantasy/NFL/modeling')
xgb.save(model_play_bin, fname = 'binary_play_type_model.model')

nfl_model_td <- nfl_trans %>%
  mutate(dv = Touchdown,
         Formation = factor(Formation)) %>%
  select(-Reception, -Fumble, -Sack, -turnover, -NONE, -Touchdown,
         -FirstDown, -PassOutcome, -PassLength, -YardsAfterCatch, -PassLocation,
         -RunLocation, -RushAttempt, -RunGap, -InterceptionThrown, -play_type, -play_location,
         -Yards.Gained, -posteam, -DefensiveTeam, -Season,  -Date, -Drive,
         -HomeTeam, -AwayTeam) %>%
  filter(Formation %in% c('UNDER CENTER', 'SHOTGUN', 'NO HUDDLE SHOTGUN', 'NO HUDDLE', 'WILDCAT'))

## Convert variables to numeric type

nfl_model_td <- nfl_model_td %>%
  mutate(off_team = as.numeric(as.factor(off_team)),
         def_team = as.numeric(as.factor(def_team)),
         Formation = as.numeric(as.factor(Formation)),
         dist_length = as.numeric(as.factor(dist_length)),
         two_min = as.numeric(as.factor(two_min)),
         score_diff = as.numeric(as.factor(score_diff)),
         team_status = as.numeric(as.factor(team_status)),
         field_status = as.numeric(as.factor(field_status)))

td_train_index <- sample(1:nrow(nfl_model_td), nrow(nfl_model_td) * 0.7)

## Create modeling data sets

td_vars <- as.matrix(nfl_model_td[,-length(nfl_model_td)])
td_label <- nfl_model_td[,'dv']
td_matrix <- xgb.DMatrix(data = as.matrix(nfl_model_td), label = td_label)

## Split into train & test

# Train

td_train <- td_vars[train_index,]
td_train_label <- td_label[train_index]
td_train_matrix <- xgb.DMatrix(data = td_train, label = td_train_label)

# Test

td_test <- td_vars[-train_index,]
td_test_label <- td_label[-train_index]
td_test_matrix <- xgb.DMatrix(data = td_test, label = td_test_label)

## Modeling

xgb_params_td <- list("objective" = "binary:logistic",
                      "eval_metric" = "logloss",
                      "colsample_bytree" = 0.5,
                      "eta" = .03,
                      "max_depth" = 6,
                      "subsample" = .7)
nround_td    <- 200 # number of XGBoost rounds
cv.nfold_td  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model_td <- xgb.cv(params = xgb_params_td,
                      data = td_train_matrix, 
                      nrounds = nround_td,
                      nfold = cv.nfold_td,
                      verbose = FALSE,
                      prediction = TRUE)
cv_model_td[[1]]

OOF_prediction_td <- data.frame(cv_model_td$pred) %>%
  mutate(max_prob_bin = ifelse(cv_model_td.pred >= .5, 1, 0),
         label_bin = td_train_label)

confusionMatrix(factor(OOF_prediction_td$label_bin),
                factor(OOF_prediction_td$max_prob_bin), positive = '1')

## Regular Model

model_play_td_fin <- xgboost(params = xgb_params_td,
                             data = td_matrix, 
                             nrounds = nround_td,
                             verbose = FALSE,
                             prediction = TRUE)

td_importance_mat <- xgb.importance(colnames(td_vars),
                                    model = model_play_td_fin)
td_importance_mat

setwd('~/Documents/Fantasy/NFL/modeling')
xgb.save(model_play_td_fin, fname = 'td_play_model.model')

## First down predictor

nfl_model_fd <- nfl_trans %>%
  mutate(dv = FirstDown,
         Formation = factor(Formation)) %>%
  select(-Reception, -Fumble, -Sack, -turnover, -NONE, -Touchdown,
         -FirstDown, -PassOutcome, -PassLength, -YardsAfterCatch, -PassLocation,
         -RunLocation, -RushAttempt, -RunGap, -InterceptionThrown, -play_type, -play_location,
         -Yards.Gained, -posteam, -DefensiveTeam, -Season,  -Date, -Drive,
         -HomeTeam, - AwayTeam) %>%
  filter(Formation %in% c('UNDER CENTER', 'SHOTGUN', 'NO HUDDLE SHOTGUN', 'NO HUDDLE', 'WILDCAT'))

## Convert variables to numeric type

nfl_model_fd <- nfl_model_fd %>%
  mutate(off_team = as.numeric(as.factor(off_team)),
         def_team = as.numeric(as.factor(def_team)),
         Formation = as.numeric(as.factor(Formation)),
         dist_length = as.numeric(as.factor(dist_length)),
         two_min = as.numeric(as.factor(two_min)),
         score_diff = as.numeric(as.factor(score_diff)),
         team_status = as.numeric(as.factor(team_status)),
         field_status = as.numeric(as.factor(field_status)))

fd_train_index <- sample(1:nrow(nfl_model_fd), nrow(nfl_model_fd) * 0.7)

## Create modeling data sets

fd_vars <- as.matrix(nfl_model_fd[,-length(nfl_model_fd)])
fd_label <- nfl_model_fd[,'dv']
fd_matrix <- xgb.DMatrix(data = as.matrix(nfl_model_fd), label = fd_label)

## Split into train & test

# Train

fd_train <- fd_vars[train_index,]
fd_train_label <- fd_label[train_index]
fd_train_matrix <- xgb.DMatrix(data = fd_train, label = fd_train_label)

# Test

fd_test <- fd_vars[-train_index,]
fd_test_label <- fd_label[-train_index]
fd_test_matrix <- xgb.DMatrix(data = fd_test, label = fd_test_label)

## Modeling

xgb_params_fd <- list("objective" = "binary:logistic",
                      "eval_metric" = "logloss",
                      "eta" = .03,
                      "colsample_bytree" = 0.5,
                      "max_depth" = 3,
                      "subsample" = .7)
nround_fd    <- 200 # number of XGBoost rounds
cv.nfold_fd  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model_fd <- xgb.cv(params = xgb_params_fd,
                      data = fd_matrix, 
                      nrounds = nround_fd,
                      nfold = cv.nfold_fd,
                      verbose = FALSE,
                      prediction = TRUE)
cv_model_fd[[1]]

OOF_prediction_fd <- data.frame(cv_model_fd$pred) %>%
  mutate(max_prob_bin = ifelse(cv_model_fd.pred >= .5, 1, 0),
         label_bin = fd_test_label)

confusionMatrix(factor(OOF_prediction_fd$label_bin),
                factor(OOF_prediction_fd$max_prob_bin), positive = '1')

## Regular Model

model_play_fd_fin <- xgboost(params = xgb_params_fd,
                             data = fd_train_matrix, 
                             nrounds = nround_fd,
                             verbose = FALSE,
                             prediction = TRUE)

fd_importance_mat <- xgb.importance(colnames(fd_vars),
                                    model = model_play_fd_fin)
fd_importance_mat

setwd('~/Documents/Fantasy/NFL/modeling')
xgb.save(model_play_fd_fin, fname = 'fd_play_model.model')

## Drive Model

nfl_drive_lookup <- nfl_trans %>%
  group_by(Date, off_team, Drive) %>%
  summarise(td_drive = sum(Touchdown)) %>%
  as.data.frame()

nfl_model_td_drive <- nfl_trans %>%
  left_join(nfl_drive_lookup, by = c('Date' = 'Date', 'off_team' = 'off_team', 'Drive' = 'Drive')) %>%
  mutate(td_drive = ifelse(td_drive > 1, 1, td_drive),
         dv = td_drive,
         Formation = factor(Formation)) %>%
  select(-Reception, -Fumble, -Sack, -turnover, -NONE, -Touchdown,
         -FirstDown, -PassOutcome, -PassLength, -YardsAfterCatch, -PassLocation,
         -RunLocation, -RushAttempt, -RunGap, -InterceptionThrown, -play_type, -play_location,
         -Yards.Gained, -posteam, -DefensiveTeam, -Season, -td_drive, -Date, -Drive,
         -HomeTeam, - AwayTeam) %>%
  filter(Formation %in% c('UNDER CENTER', 'SHOTGUN', 'NO HUDDLE SHOTGUN', 'NO HUDDLE', 'WILDCAT'))

## Convert variables to numeric type

nfl_model_td_drive <- nfl_model_td_drive %>%
  mutate(off_team = as.numeric(as.factor(off_team)),
         def_team = as.numeric(as.factor(def_team)),
         Formation = as.numeric(as.factor(Formation)),
         dist_length = as.numeric(as.factor(dist_length)),
         two_min = as.numeric(as.factor(two_min)),
         score_diff = as.numeric(as.factor(score_diff)),
         team_status = as.numeric(as.factor(team_status)),
         field_status = as.numeric(as.factor(field_status)))

## Training & Testing Sets

td_drive_train_index <- sample(1:nrow(nfl_model_td_drive), nrow(nfl_model_td_drive) * 0.7)

## Create modeling data sets

td_drive_vars <- as.matrix(nfl_model_td_drive[,-length(nfl_model_td_drive)])
td_drive_label <- nfl_model_td_drive[,'dv']
td_drive_matrix <- xgb.DMatrix(data = as.matrix(nfl_model_td_drive), label = td_drive_label)

## Split into train & test

# Train

td_drive_train <- td_drive_vars[train_index,]
td_drive_train_label <- td_drive_label[train_index]
td_drive_train_matrix <- xgb.DMatrix(data = td_drive_train, label = td_drive_train_label)

# Test

td_drive_test <- td_drive_vars[-train_index,]
td_drive_test_label <- td_drive_label[-train_index]
td_drive_test_matrix <- xgb.DMatrix(data = td_drive_test, label = td_drive_test_label)

## Modeling

xgb_params_td_drive <- list("objective" = "binary:logistic",
                            "eval_metric" = "logloss",
                            "eta" = .04,
                            "colsample_bytree" = 0.65,
                            "max_depth" = 7,
                            "subsample" = .7)
nround_td_drive    <- 250 # number of XGBoost rounds
cv.nfold_td_drive  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model_td_drive <- xgb.cv(params = xgb_params_td_drive,
                            data = td_drive_train_matrix, 
                            nrounds = nround_td_drive,
                            nfold = cv.nfold_td_drive,
                            verbose = FALSE,
                            prediction = TRUE)
cv_model_td_drive[[1]]

OOF_prediction_td_drive <- data.frame(cv_model_td_drive$pred) %>%
  mutate(max_prob_bin = ifelse(cv_model_td_drive.pred >= .5, 1, 0),
         label_bin = td_drive_train_label)

confusionMatrix(factor(OOF_prediction_td_drive$label_bin),
                factor(OOF_prediction_td_drive$max_prob_bin), positive = '1')


## Model for Saving

model_drive_td_fin <- xgboost(params = xgb_params_td_drive,
                              data = td_drive_train_matrix, 
                              nrounds = nround_td_drive,
                              verbose = FALSE,
                              prediction = TRUE)

td_drive_importance_mat <- xgb.importance(colnames(td_drive_vars),
                                          model = model_drive_td_fin)
td_drive_importance_mat

setwd('~/Documents/Fantasy/NFL/modeling')
xgb.save(model_drive_td_fin, fname = 'td_drive_model.model')

## Win Probability

nfl_model_win <- nfl_trans %>%
  filter(!is.na(win)) %>% ## Remove once full 2015/2016 files are uploaded
  mutate(dv = win,
         Formation = factor(Formation)) %>%
  select(-Reception, -Fumble, -Sack, -turnover, -NONE, -Touchdown,
         -FirstDown, -PassOutcome, -PassLength, -YardsAfterCatch, -PassLocation,
         -RunLocation, -RushAttempt, -RunGap, -InterceptionThrown, -play_type, -play_location,
         -Yards.Gained, -posteam, -DefensiveTeam, -Season,  -Date, -Drive,
         -HomeTeam, -AwayTeam, -win) %>%
  filter(Formation %in% c('UNDER CENTER', 'SHOTGUN', 'NO HUDDLE SHOTGUN', 'NO HUDDLE', 'WILDCAT'))

## Convert variables to numeric type

nfl_model_win <- nfl_model_win %>%
  mutate(off_team = as.numeric(as.factor(off_team)),
         def_team = as.numeric(as.factor(def_team)),
         Formation = as.numeric(as.factor(Formation)),
         dist_length = as.numeric(as.factor(dist_length)),
         two_min = as.numeric(as.factor(two_min)),
         score_diff = as.numeric(as.factor(score_diff)),
         team_status = as.numeric(as.factor(team_status)),
         field_status = as.numeric(as.factor(field_status)))

win_train_index <- sample(1:nrow(nfl_model_win), nrow(nfl_model_win) * 0.7)

## Create modeling data sets

win_vars <- as.matrix(nfl_model_win[,-length(nfl_model_win)])
win_label <- nfl_model_win[,'dv']
win_matrix <- xgb.DMatrix(data = as.matrix(nfl_model_win), label = win_label)

## Split into train & test

# Train

win_train <- win_vars[win_train_index,]
win_train_label <- win_label[win_train_index]
win_train_matrix <- xgb.DMatrix(data = win_train, label = win_train_label)

# Test

win_test <- win_vars[-train_index,]
win_test_label <- win_label[-train_index]
win_test_matrix <- xgb.DMatrix(data = win_test, label = win_test_label)

## Modeling

xgb_params_win <- list("objective" = "binary:logistic",
                       "eval_metric" = "logloss",
                       "colsample_bytree" = 0.65,
                       "eta" = .03,
                       "max_depth" = 10,
                       "subsample" = .70)
nround_win    <- 300 # number of XGBoost rounds
cv.nfold_win  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model_win <- xgb.cv(params = xgb_params_win,
                       data = win_train_matrix, 
                       nrounds = nround_win,
                       nfold = cv.nfold_win,
                       verbose = FALSE,
                       prediction = TRUE)
cv_model_win[[1]]

OOF_prediction_win <- data.frame(cv_model_win$pred) %>%
  mutate(max_prob_bin = ifelse(cv_model_win.pred >= .5, 1, 0),
         label_bin = win_train_label)

confusionMatrix(factor(OOF_prediction_win$label_bin),
                factor(OOF_prediction_win$max_prob_bin), positive = '1')

## Regular Model

model_play_win_fin <- xgboost(params = xgb_params_win,
                              data = win_train_matrix, 
                              nrounds = nround_win,
                              verbose = FALSE,
                              prediction = TRUE)

win_importance_mat <- xgb.importance(colnames(win_vars),
                                     model = model_play_win_fin)
win_importance_mat

man_win_test <- ifelse(predict(model_play_win_fin, win_test_matrix) >= 0.5, 1, 0)
confusionMatrix(factor(win_test_label),
                factor(man_win_test), positive = '1')

setwd('~/Documents/Fantasy/NFL/modeling')
xgb.save(model_play_win_fin, fname = 'win_play_model.model')


## Creae dataset for evaluation visualization - convert to function in the future

  play_bin_eval_raw <- as.data.frame(play_bin_test)

  play_bin_eval_raw$pred <- ifelse(predict(model_play_bin, play_bin_test_matrix) >= 0.5, 1, 0)
  play_bin_eval_raw$label <- play_bin_test_label
  play_bin_eval_raw$score <- ifelse(play_bin_eval_raw$pred == play_bin_eval_raw$label, 1, 0)
  
  play_bin_eval <- play_bin_eval_raw %>%
    group_by(off_team) %>%
    summarise(perc_correct = sum(score) / n(),
              recs = n()) %>%
    as.data.frame() %>%
    left_join(team_lookup, by = 'off_team') %>%
    select(-off_team) %>%
    arrange(-perc_correct) %>%
    mutate(rank = row_number(),
           Label = ifelse(rank == 1, paste0(off_team_lookup, " ", round((perc_correct * 100),2),'%'),
                   ifelse(rank == 64, paste0(off_team_lookup, " ", round((perc_correct * 100),2),'%'),
                   ifelse(rank == 128, paste0(off_team_lookup, " ", round((perc_correct * 100),2),'%'),'')))
           )
  
  ## By Team

ggplot(data = play_bin_eval, aes(x = "Team Year Combinations", y = perc_correct), id.n = Inf) +
  geom_boxplot(colour = 'blue', size = 1.5) +
  geom_text(aes(label=Label,vjust=-0.5)) +
  ggtitle("Play Type Prediction Success Percentage") +
    theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_blank())

## By Down

play_bin_eval_down <- play_bin_eval_raw %>%
    group_by(down) %>%
    summarise(perc_correct = sum(score) / n(),
              recs = n()) %>%
    as.data.frame() %>%
    arrange(-perc_correct) %>%
    mutate(rank = row_number(),
           Label = ifelse(down == 1, paste0('1st Down', " ", round((perc_correct * 100),2),'%'),
                   ifelse(down == 2, paste0('2nd Down', " ", round((perc_correct * 100),2),'%'),
                   ifelse(down == 3, paste0('3rd Down'," ", round((perc_correct * 100),2),'%'),
                          paste0('4th Down'," ", round((perc_correct * 100),2),'%')))))

  ## By Down

ggplot(data = play_bin_eval_down, aes(x = down, y = perc_correct)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(aes(label=Label,vjust=-0.5)) +
  ggtitle("Play Type Prediction Success Percentage - By Down") +
  labs(xlab = 'Down', ylab = 'Percentage Correct') +
    theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11))

## Evaluate by Quarter

play_bin_eval_quarter <- play_bin_eval_raw %>%
    group_by(qtr) %>%
    summarise(perc_correct = sum(score) / n(),
              recs = n()) %>%
    as.data.frame() %>%
    arrange(-perc_correct) %>%
    mutate(rank = row_number(),
           Label = ifelse(qtr == 1, paste0('1st Q', " ", round((perc_correct * 100),2),'%'),
                   ifelse(qtr == 2, paste0('2nd Q', " ", round((perc_correct * 100),2),'%'),
                   ifelse(qtr == 3, paste0('3rd Q'," ", round((perc_correct * 100),2),'%'),
                          paste0('4th Q'," ", round((perc_correct * 100),2),'%')))))

  ## By Quarter

ggplot(data = play_bin_eval_quarter, aes(x = qtr, y = perc_correct)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  geom_text(aes(label=Label,vjust=-0.5)) +
  ggtitle("Play Type Prediction Success Percentage - By Quarter") +
  labs(xlab = 'Quarter', ylab = 'Percentage Correct') +
    theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_blank())

## Evaluate by Formation

play_bin_eval_formation <- play_bin_eval_raw %>%
    group_by(Formation) %>%
    summarise(perc_correct = sum(score) / n(),
              recs = n()) %>%
    as.data.frame() %>%
    arrange(-perc_correct) %>%
    mutate(rank = row_number(),
           Label = ifelse(Formation == 1, paste0('No Huddle', " ", round((perc_correct * 100),2),'%'),
                   ifelse(Formation == 2, paste0('No Huddle Shotgun', " ", round((perc_correct * 100),2),'%'),
                   ifelse(Formation == 3, paste0('Shotgun'," ", round((perc_correct * 100),2),'%'),
                          paste0('Under Center'," ", round((perc_correct * 100),2),'%')))))

  ## By Quarter

ggplot(data = filter(play_bin_eval_formation, Formation < 5), aes(x = Formation, y = perc_correct)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  geom_text(aes(label=Label,vjust=-0.5)) +
  ggtitle("Play Type Prediction Success Percentage - By Formation") +
  labs(xlab = 'Quarter', ylab = 'Percentage Correct') +
    theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11))






