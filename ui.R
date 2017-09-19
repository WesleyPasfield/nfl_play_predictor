library(dplyr)
library(shiny)
library(xgboost)
library(tidyr)
library(ggplot2)

options(shiny.sanitize.errors = FALSE)

team_lookup <- read.csv('team_lookup.csv', stringsAsFactors = F)

shinyUI(fluidPage(
  sidebarPanel(
    tags$style(type='text/css', ".well { line-height: 0.05; font-size: 11px;} 
              .selectize-dropdown { line-height: 7px; }
              .selectize-input { height: 34px; }
              .irs-with-grid { height:40px; }"), 
    #tags$style(type='text/css', ".control-label { line-height: 0.05; font-size: 11px;}"
    #),
    align = 'left',
    h5('Create Scenario by Changing Inputs Below', align = 'center'),
           selectInput('season', 'Season - 2017 will be broken out separately in the future',
                       choices = c(2013,2014,2015,'2016/2017'), selected = '2016/2017'),
           selectInput('home_team', 'Home Team',
                       choices = unique(team_lookup$team_name), selected = 'OAK'),
           selectInput('away_team', 'Away Team',
                       choices = unique(team_lookup$team_name), selected = 'DEN'),
           sliderInput('home_score', 'Home Team Score',
                        min = 0, max = 60, value = 0, step = 1, round = T),
           sliderInput('away_score', 'Away Team Score',
                        min = 0, max = 60, value = 0, step = 1, round = T),
           selectInput('home_to', 'Home Timeouts Left', 
                       choices = c(0,1,2,3), selected = 3),
           selectInput('away_to', 'Away Timeouts Left', 
                       choices = c(0,1,2,3), selected = 3),
           selectInput('team_ball', 'Team with Ball', 
                       choices = c('Home', 'Away'), selected = 'Home'),
           selectInput('quarter', 'Quarter',
                       choices = c(1,2,3,4), selected = 1),
           sliderInput('mins', 'Minutes left in Quarter',
                       min = 0, max = 15, value = 15, step = 1, round = T),
           sliderInput('yard_line', 'Yard Line - 51-100 is own territory',
                       min = 0, max = 100, value = 50, step = 1, round = T),
           selectInput('gtg', 'Is this a goal-to-go play?',
                        choices = c(TRUE, FALSE), selected = FALSE),
           selectInput('down', 'Down',
                       choices = c(1,2,3,4), selected = 1),
           sliderInput('yds_to_go', 'Yards to Go - ignored in goal to go situations',
                       min = 0, max = 30, value = 10, step = 1, round = T),
           selectInput('formations', 'Offensive Formation',
                      choices = c('UNDER CENTER', 'SHOTGUN', 'NO HUDDLE SHOTGUN', 'NO HUDDLE'),
                      selected = 'SHOTGUN')
                       ), 
  mainPanel(h4('Expected Play Selection & Outcome for Chosen Scenario', align = 'center'),
            verbatimTextOutput('text'),
            fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput('plot1'),plotOutput('plot2'))
            ),
            hr(),
            h4('Comparison vs. All Other Offensive Teams', align = 'center'),
            fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput('plot3'),plotOutput('plot4'))
            ))
  )
)
