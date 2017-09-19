library(dplyr)
library(shiny)
library(xgboost)
library(tidyr)
library(ggplot2)

options(shiny.sanitize.errors = FALSE)

## Load in from local directory (that houses server & R files)?

xgb_play_type <- xgb.load('binary_play_type_model.model')
xgb_play_direction <- xgb.load('play_direction_model.model')
xgb_fd <- xgb.load('fd_play_model.model')
xgb_td <- xgb.load('td_play_model.model')
xgb_td_drive <- xgb.load('td_drive_model.model')
xgb_win <- xgb.load('win_play_model.model')
team_lookup <- read.csv('team_lookup.csv', stringsAsFactors = F)

## Function to format data for predictions

predictor_nfl_play <- function(off_team, def_team, team_status, sd,
                          quarter, down, o_tos, d_tos, tu,
                          yard_line, yards_to_go, format, s, gtg) 
  {
  ## Convert formation to numeric 
  
  form <- 0
  if(format == 'NO HUDDLE'){
    form <- 1 
  } else if(format == 'NO HUDDLE SHOTGUN') {
    form <- 2
  } else if(format == 'SHOTGUN') {
    form <- 3
  } else if(format == 'UNDER CENTER') {
    form <- 4
  } else {
    form <- 5
  }
  
  ## Pull out numeric values of team names chosen
  
  off_team_j <- as.data.frame(matrix(c(off_team, def_team), nrow = 2), stringsAsFactors = F)
  colnames(off_team_j) = c('teams')
  off_team_j$teams <- as.character(off_team_j$teams)
  off_team_j <- left_join(off_team_j, team_lookup, by = c('teams' = 'off_team_lookup'))
  off_team_fin <- off_team_j[1,2]
  def_team_fin <- off_team_j[2,2]
  all_teams <- team_lookup
  all_teams$season <- as.numeric(all_teams$season)
  library(dplyr)
  all_teams <- filter(all_teams, season == s)
  all_teams <- as.numeric(all_teams$off_team)
  
  ## Convert all variable to numeric based on modeling transformation values
  
  dl <- as.numeric(ifelse(yards_to_go > 7, 2,
        ifelse(yards_to_go > 3 & yards_to_go <= 7, 1, 
               3)))
  tm <- as.numeric(ifelse(tu <= 2 & (quarter == 2 | quarter == 4), 2, 1))
  sd_bin <- as.numeric(ifelse(sd < -8, 1,
               ifelse(sd < 0 & sd >= -8, 3,
               ifelse(sd == 0, 5,
               ifelse(sd > 0 & sd <= 8, 4,
               2)))))
  fs <- as.numeric(ifelse(yard_line < 20, 1,
                 ifelse(yard_line >= 21 & yard_line < 50, 3,
                 ifelse(yard_line >= 50 & yard_line < 80, 2,
                 4))))
  quarter <- as.numeric(quarter)
  down <- as.numeric(down)
  sd <- as.numeric(sd)
  s <- ifelse(s == 2013, 1,
       ifelse(s == 2014, 2,
       ifelse(s == 2015, 3,
       ifelse(s == 2016/2017, 4, 4))))
  o_tos <- as.numeric(o_tos)
  d_tos <- as.numeric(d_tos)
  yards_to_go <- ifelse(gtg == FALSE, as.numeric(yards_to_go), as.numeric(yard_line))
  tu <- as.numeric(tu)
  yard_line <- as.numeric(yard_line)
  ts <- as.numeric(team_status)
  ## Create matrix of all teams for predictions
  prediction_input <- matrix(rep(c(quarter, down, yards_to_go, tu, yard_line, o_tos, d_tos,
                        off_team_fin, def_team_fin, form, dl, tm, sd_bin, sd, ts, fs),32),nrow=32, ncol=16, byrow = T)
  prediction_input[,8] <- all_teams
  
  
  colnames(prediction_input) <- c('qtr', 'down', 'ydstogo', 'TimeUnder', 'yrdline100', 'posteam_timeouts_pre',
                                  'defense_to_pre', 'off_team', 'def_team', 'Formation', 'dist_length', 'two_min',
                                  'score_diff', 'score_diff_raw', 'team_status', 'field_status')
  
  ## Create matrix just for team selected
  
  team_preds <- matrix(prediction_input[prediction_input[,'off_team'] == off_team_fin,], nrow = 1, ncol = 16)
  
  ## Individual team predictions
  
  td_pred <- predict(xgb_td, team_preds)
  td_drive_pred <- predict(xgb_td_drive, team_preds)
  fd_pred <- predict(xgb_fd, team_preds)
  play_type_pred <- predict(xgb_play_type, team_preds)
  play_direction_pred <- predict(xgb_play_direction, team_preds)
  win_pred <- predict(xgb_win, team_preds)
  
  ## Rescale play direction preds based on difference between play type & direction sums by play type
  
  play_direction_mult <- play_type_pred / sum(play_direction_pred[4:6])
  play_direction_mult_2 <- (1-play_type_pred) / sum(play_direction_pred[1:3])
  for(i in 4:6) {
    play_direction_pred[i] <- play_direction_pred[i] * play_direction_mult
  }
  for(i in 1:3) {
    play_direction_pred[i] <- play_direction_pred[i] * play_direction_mult_2
  }
  
  fin <- as.data.frame(matrix(c(td_pred, td_drive_pred, fd_pred, play_type_pred, play_direction_pred,win_pred), nrow = 1, ncol = 11), stringsAsFactors = F)
  colnames(fin) <- c('TD_Play', 'TD_Drive', 'FD_Play', 'Run', 'Pass_Left', 'Pass_Middle', 'Pass_Right', 'Run_Left', 'Run_Middle', 'Run_Right', 'Win_Prob')
  fin$Pass <- 1 - fin$Run
  fin <- round(fin, 4)
  fin$team <- off_team_fin
  
  ## Create logic for probabilities of TD on Play, First Down on Play, TD on drive & Points from TD on drive
  
  if(down == 4 & yard_line < 10 & yards_to_go == yard_line) {
      text <- paste0(
                 "Probability of a First Down on this Play:                 ", fin$FD_Play * 100, '%', "\n",
                 "Probability of a TD on this Drive:                        ", fin$TD_Drive * 100,'%',"\n",
                 "Expected Points from a TD from this Drive:                ", round(fin$TD_Drive * 6.942, 2),"\n",
                 off_team, ' chance of winning: ',fin$Win_Prob * 100, '% ', def_team, ' chance of winning: ', round((1-fin$Win_Prob) * 100,2),'%',"\n")
  } else {
      text <- paste0(
                 "Probability of a TD on this Play:               ", fin$TD_Play * 100,'%',"\n",
                 "Probability of a First Down on this Play:       ", fin$FD_Play * 100, '%', "\n",
                 "Probability of a TD on this Drive:              ", fin$TD_Drive * 100,'%',"\n",
                 "Expected Points from a TD from this Drive:      ", round(fin$TD_Drive * 6.942, 2),"\n",
                 off_team, ' chance of winning: ',fin$Win_Prob * 100, '% ', def_team, ' chance of winning: ', round((1-fin$Win_Prob) * 100,2),'%',"\n")
  }

  ## Create pie chart showing play type
  
  fin_all <- select(fin, Run, Pass) %>%
    gather(key = Play_Type, value = Percentage_Chance) %>%
    mutate(pos = cumsum(Percentage_Chance)- Percentage_Chance/2) ## Varible for plot label positioning
  
  library(ggplot2)

  p <- ggplot(fin_all,  aes(x = '', y = Percentage_Chance, fill = Play_Type)) +
    geom_bar(width = 1, stat = 'identity') +
    geom_text(aes(x= '', y=pos, label = paste0(Play_Type, " ", round((Percentage_Chance * 100),4),"%")), size=6) +
    ggtitle("Percentage Chance of Pass vs. Run") + 
    theme(plot.title = element_text(hjust = 0.5),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 11))
  
  pie <- p + coord_polar("y", start = 0)
  
  ## Create chart for pass & run by direction
  
  fin_direction <- fin %>%
    select(Pass_Left, Pass_Middle, Pass_Right, Run_Left, Run_Middle, Run_Right) %>%
    gather(key = Play_Direction, value = Percentage_Chance) %>%
    mutate(Play_Type = ifelse(Play_Direction %in% c('Pass_Right', 'Pass_Left', 'Pass_Middle'), 'Pass', 'Run')) %>%
    arrange(Play_Direction)
  
  p2 <- ggplot(fin_direction, aes(x = reorder(Play_Direction, Percentage_Chance), y = Percentage_Chance, fill = Play_Type)) +
          geom_bar(stat = 'identity') +
          geom_text(aes(x = Play_Direction, label = paste0(Play_Direction, " ", round((Percentage_Chance * 100),4),"%")), position = position_stack(vjust = 0.5), size = 4) +
          coord_flip() +
          ggtitle("Percentage Chance of Play Type & Direction") +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 11))
  
  ## Create boxplot comparing all teams
  
  ## All Team Preds
  
  play_type_pred_all <- predict(xgb_play_type, prediction_input)
  td_drive_pred_all <- predict(xgb_td_drive, prediction_input)
  
  ## Create data frame of teams & predicted outcome 

  play_type_teams <- as.data.frame(matrix(c(play_type_pred_all, prediction_input[,8]), nrow = 32, ncol = 2, byrow = F), stringsAsFactors = F) %>%
    left_join(select(team_lookup, off_team, team_name), by = c('V2' = 'off_team')) %>%
    arrange(-V1) %>%
    mutate(rank = row_number(),
           Pass = round(1 - V1, 4),
           V1 = round(V1, 4),
           Label = ifelse(V2 == off_team_fin, paste0(team_name, " ", round((Pass*100),4),'%'),
                   ifelse(rank == 1, paste0(team_name, " ", round((Pass*100),4),'%'),
                   ifelse(rank == 32, paste0(team_name, " ", round((Pass*100),4),'%'), '')))) %>%
    select(team_name, Pass, V1, Label)
  
  colnames(play_type_teams) <- c('Team', 'Pass', 'Run', 'Label')
  
  bp <- ggplot(data = play_type_teams, aes(x = "Pass Prediction by Team", y = Pass), id.n = Inf) +
  geom_boxplot(colour = 'red', size = 1) +
  geom_text(aes(label=Label,vjust=-0.5), size = 5) + 
  ggtitle("Boxplot of Percentage Chance of Passing by Team") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_blank()) +
  labs(ylab = 'Pass Likelihood')
  ##geom_point(colour = 'black', size = 2)
  
  ## Same process for TD likelihood
  
  td_drive_teams <- as.data.frame(matrix(c(td_drive_pred_all, prediction_input[,8]), nrow = 32, ncol = 2, byrow = F), stringsAsFactors = F) %>%
    left_join(select(team_lookup, off_team, team_name), by = c('V2' = 'off_team')) %>%
    arrange(-V1) %>%
    mutate(rank = row_number(),
           V1 = round(V1, 4),
           Label = ifelse(V2 == off_team_fin, paste0(team_name, " ", round((V1*100),4),'%'),
                   ifelse(rank == 1, paste0(team_name, " ", round((V1*100),4),'%'),
                   ifelse(rank == 32, paste0(team_name, " ", round((V1*100),4),'%'), '')))) %>%
    select(team_name, V1, Label)
  
  colnames(td_drive_teams) <- c('Team', 'TD_Likelihood', 'Label')

  
  bp2 <- ggplot(data = td_drive_teams, aes(x = "Likelihood of a TD on this Drive", y = TD_Likelihood), id.n = Inf) +
  geom_boxplot(colour = 'red', size = 1) +
  geom_text(aes(label=Label,vjust=-0.5), size = 5) + 
  ggtitle("Boxplot of Likelihood of a TD on this Drive by Team") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_blank())
  ##geom_point(colour = 'black', size = 2)
    
  ## Put all outputs into a list for display
  
  l <- list()
  l[[1]] <- pie
  l[[2]] <- p2
  l[[3]] <- text
  l[[4]] <- bp
  l[[5]] <- bp2
         
  return(l)
}

server <- function(input,output,session){

    selectedData3 <- reactive({
    poss <- ''
  if(input$team_ball == 'Home') {
    poss <- input$home_team
  } else {
    poss <- input$away_team
  }
  team <- if(input$season == '2016/2017') {
    paste0(poss,"_",'2016') 
  } else {
    paste0(poss,"_",input$season)
  }
  sea <- if(input$season == '2016/2017') {
    '2016'
    } else {
    c(input$season)
    }
  dow <- c(input$down)
  qua <- c(input$quarter)
  gtg <- c(input$gtg)
  def <- ''
  if(input$team_ball == 'Home') {
    def <- input$away_team
    } else {
    def <- input$home_team
    }
  dt <- if(input$season == '2016/2017') {
    paste0(def,"_",'2016') 
    } else {
      paste0(def,"_",input$season)
    }
  ts <- 0
  if(input$team_ball == 'Home') {
    ts <- 1
    } else {
    ts <- 0
    }
  forma <- c(input$formations)
  yds <- c(input$yds_to_go)
  yl <- c(input$yard_line)
  sdiff <- 0
  if(input$team_ball == 'Home'){
    sdiff <- input$home_score - input$away_score
  } else {
    sdiff <- input$away_score - input$home_score
  }
  tu <- c(input$mins)
  oto <- 0
  if(input$team_ball == 'Home'){
    oto <- input$home_to
  } else {
    oto <- input$away_to
  }
  dto <- 0
  if(input$team_ball == 'Home'){
    dto <- input$away_to
  } else {
    dto <- input$home_to
  }
  
  parameters <- c(team, dt, ts, sdiff, qua, dow, oto, dto, tu, yl, yds, forma, sea, gtg)

  return(predictor_nfl_play(team, dt, ts, sdiff, qua, dow, oto, dto, tu, yl, yds, forma, sea, gtg))
  })

  output$text <- renderText({ return(selectedData3()[[3]])
  })
  output$plot1 <- renderPlot({ return(selectedData3()[[1]])
  })
  output$plot2 <- renderPlot({ return(selectedData3()[[2]])
  })
  output$plot3 <- renderPlot({ return(selectedData3()[[4]])
  })
  output$plot4 <- renderPlot({ return(selectedData3()[[5]])
  })
}
