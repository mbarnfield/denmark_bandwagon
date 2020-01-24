# Denmark

library(shiny)
library(tidyverse)
library(gmodels)
library(miceadds)
library(estimatr)
library(randomNames)
library(extraDistr)
library(skellam)

# load data ----
experiment <- readRDS("data/experiment.rds")
profiles <- readRDS("data/profiles.rds")

# Source helper functions -----
source("helpers.R")

# app -----------------------------------
ui <- fluidPage( # -----------------
                 titlePanel("Bandwagon effect conjoint analysis: Denmark"),
                 tags$h4(
                   "A conjoint analysis experiment, simulating a Danish Folketing election. Scroll down to see, download, and model your data."
                 ),
                 br(),
                 br(),
                 tags$h2("Experiment"),
                 br(),
                 selectInput("preference", "Which party would you vote for in an election tomorrow?",
                             c("SD", "Venstre", "DPP", "SL", "SF")),
                 br(),
                 tags$h3("Candidates"),
                 br(),
                 tabPanel(
                   "Parties",
                   tags$h5(
                     "Imagine you are voting again in the 2019 Folketing election, and you are choosing between the following parties. Which would you vote for, if at all?"),
                   fluidRow(
                     column(width = 4, DT::dataTableOutput("cand1"),
                            align = "center"
                     ),
                     column(width = 4, DT::dataTableOutput("cand2"),
                            align = "center"
                     ),
                     column(width = 4, DT::dataTableOutput("cand3"),
                            align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(
                       width = 4,
                       actionButton(inputId = "one", label = "Vote for Party One"),
                       align = "center"
                     ),
                     column(
                       width = 4,
                       actionButton(inputId = "two", label = "Vote for Party Two"),
                       align = "center"
                     ),
                     column(
                       width = 4,
                       actionButton(inputId = "three", label = "Vote for Party Three"),
                       align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(width = 4, DT::dataTableOutput("cand4"),
                            align = "center"
                     ),
                     column(width = 4, DT::dataTableOutput("cand5"),
                            align = "center"
                     ),
                     column(
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       width = 4,
                       actionButton(inputId = "abstain", label = "I would not turn out to vote"),
                       align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(
                       width = 4,
                       actionButton(inputId = "four", label = "Vote for Party Four"),
                       align = "center"
                     ),
                     column(
                       width = 4,
                       actionButton(inputId = "five", label = "Vote for Party Five"),
                       align = "center"
                     )
                   )),
                 br(),
                 tags$h2("Data"),
                 br(),
                 tags$h3("Your Choices"),
                 br(),
                 br(),
                 DT::dataTableOutput("choice"),
                 br(),
                 tags$h3("Download"),
                 radioButtons("filetype", "File type:",
                              choices = c("csv", "tsv")),
                 br(),
                 downloadButton('downloadData', 'Download'),
                 br(),
                 tags$h2("Model"),
                 br(),
                 tags$h3("Your AMCEs"),
                 tags$h5(
                   "Note: please make several choices before running model. It will crash with fewer than two choices made, and will not produce confidence intervals before a minimum of four choices are made (and may often require even more than that -- it depends how many features you have)."
                 ),
                 br(),
                 actionButton(inputId = "model", label = "Run model"),
                 br(),
                 br(),
                 plotOutput("amce_plot", height = 1200),
                 br(),
                 br(),
                 tags$h3("Switching AMCEs - indicates presence of bandwagon effects and strategic voting."),
                 tags$h5(
                   "Note: this model requires you to make a lot of choices, especially if you are rarely switching from your preferred party."
                 ),
                 br(),
                 actionButton(inputId = "switch_model", label = "Run model"),
                 br(),
                 br(),
                 plotOutput("switch_plot", height = 1200)
)

# ----------------------
server <- function(input, output) {
  
  amce_r <-
    reactiveValues(
      amces = data.frame(
        data.frame(
          outcome = 0,
          statistic = "amce",
          feature = "all",
          level = "all",
          estimate = 0,
          std.error = 0,
          t = 0,
          p = 0,
          lower = 0,
          upper = 0,
          r_squared = 0
        )),
      switches = data.frame(
        data.frame(
          outcome = 0,
          statistic = "amce",
          feature = "all",
          level = "all",
          estimate = 0,
          std.error = 0,
          t = 0,
          p = 0,
          lower = 0,
          upper = 0,
          r_squared = 0
        )))
  
  row_num_1 <- 1
  row_num_2 <- 2
  row_num_3 <- 3
  row_num_4 <- 4
  row_num_5 <- 5
  
  rv_shown <- reactiveValues(
    data1 = profiles[row_num_1,],
    data2 = profiles[row_num_2,],
    data3 = profiles[row_num_3,],
    data4 = profiles[row_num_4,],
    data5 = profiles[row_num_5,]
  )
  
  rv_hidden <- reactiveValues(
    data1 = experiment[row_num_1,],
    data2 = experiment[row_num_2,],
    data3 = experiment[row_num_3,],
    data4 = experiment[row_num_4,],
    data5 = experiment[row_num_5,]
  )
  
  col_no <- reactiveValues(col_no = sample(ncol(profiles[1, ])))
  
  choice <- reactiveValues(choice = data.frame(),
                           for_model = data.frame(),
                           for_switch_model = data.frame())
  
  
  observeEvent(input$one, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data1 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"])}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"])},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"])},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"])},
      {rv_hidden$data5 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 5,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from = sum(switched_from)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data5 <- experiment[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data5 <- profiles[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$two, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data2 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"])},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"])}, 
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"])},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"])},
      {rv_hidden$data5 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 5,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from = sum(switched_from)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data5 <- experiment[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data5 <- profiles[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$three, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data3 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"]
                                  )},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"])}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"])},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"])},
      {rv_hidden$data5 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 5,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from = sum(switched_from)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data5 <- experiment[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data5 <- profiles[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$four, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data4 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"])},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"])}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"])},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"])},
      {rv_hidden$data5 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 5,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from = sum(switched_from)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data5 <- experiment[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data5 <- profiles[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$five, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data5 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 5,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data5[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data5[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data5[, "dn_emphasised"])},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data5[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data5[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data5[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data5[, "dn_emphasised"]
                                  )}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data5[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data5[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data5[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data5[, "dn_emphasised"]
                                  )},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data5[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data5[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data5[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data5[, "dn_emphasised"]
                                  )},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data5[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data5[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data5[, "sn_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data5[, "dn_emphasised"]
                                  )}) %>% 
        mutate(switched = sum(switched),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from = sum(switched_from)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data5 <- experiment[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data5 <- profiles[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$abstain, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 1,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_from = 0,
                                  switched_to_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_dynamic_emph = 0
                                  )}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 2,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_from = 0,
                                  switched_to_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_dynamic_emph = 0
                                  )},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 3,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_from = 0,
                                  switched_to_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_dynamic_emph = 0
                                  )},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 4,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_from = 0,
                                  switched_to_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_dynamic_emph = 0
                                  )},
      {rv_hidden$data5 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$five + input$abstain,
                                  profile_no = 5,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_from = 0,
                                  switched_to_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_dynamic_emph = 0
                                  )}) %>% 
        mutate(switched = sum(switched),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from = sum(switched_from)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_hidden$data5 <- experiment[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    rv_shown$data5 <- profiles[row_num_5 + input$one*5 + input$two*5 + input$three*5 + input$four*5 + input$five*5 + input$abstain*5,]
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$model, {
    choice$for_model <- choice$choice %>%
      mutate(
        sn_emphasised = as_factor(case_when(
          sn_emphasised == 1 ~ "Static national emphasised",
          TRUE ~ "Static national not emphasised"
        )),
        dn_emphasised = as_factor(case_when(
          dn_emphasised == 1 ~ "Dynamic national emphasised",
          TRUE ~ "Dynamic national not emphasised"
        )),
        party = as_factor(party),
        polls = as_factor(paste0("National polls: ", polls, "%")),
        change = as_factor(paste0("Change in national polls: ", change)),
        sn_interaction = interaction(polls, sn_emphasised),
        dn_interaction = interaction(change, dn_emphasised)
      ) %>%
      select(
        party,
        polls,
        change,
        sn_emphasised,
        dn_emphasised,
        sn_interaction,
        dn_interaction,
        chosen
      ) 
    
    choice$for_model[, "polls"] <- factor(
      choice$for_model[, "polls"],
      levels = c(
        "National polls: 2%",
        "National polls: 5%",
        "National polls: 10%",
        "National polls: 15%",
        "National polls: 20%",
        "National polls: 25%"
      )
    )
    
    
    choice$for_model[, "change"] <- factor(
      choice$for_model[, "change"],
      levels = c(
        "Change in national polls: 0",
        "Change in national polls: -10",
        "Change in national polls: -7",
        "Change in national polls: -5",
        "Change in national polls: -2",
        "Change in national polls: 2",
        "Change in national polls: 5",
        "Change in national polls: 7",
        "Change in national polls: 10"
      )
    )
    
    # run function on data
    amce_r$amces <- amce_rsq(choice$for_model,
                             chosen ~ party + polls + change + sn_emphasised + dn_emphasised + sn_interaction + dn_interaction
    )
  })
  
  observeEvent(input$switch_model, {
    choice$for_switch_model <- choice$choice %>%
      filter(voted == 1) %>%
      mutate(
        switched_to_emph = as_factor(case_when(
          switched_to_emph == 1 ~ "Switched-to party polling emphasised",
          TRUE ~ "Switched-to party polling not emphasised")),
        switched_to_dynamic_emph = as_factor(case_when(
          switched_to_dynamic_emph == 1 ~ "Switched-to party change in polling emphasised",
          TRUE ~ "Switched-to party change in polling not emphasised")),
        party = as_factor(party),
        switched_to = as_factor(paste0("Switched-to party polling: ", switched_to, "%")),
        switched_from = as_factor(paste0("Switched-from party polling: ", switched_from, "%")),
        switched_to_dynamic = as_factor(paste0("Switched-to party change in polling: ", switched_to_dynamic)),
        switched_from_dynamic = as_factor(paste0("Switched-from party change in polling: ", switched_from_dynamic)),
        sn_interaction = interaction(polls, sn_emphasised),
        dn_interaction = interaction(change, dn_emphasised)
      ) %>%
      select(switched_to,
             switched_from,
             switched_to_dynamic,
             switched_from_dynamic,
             switched_to_emph,
             switched_to_dynamic_emph,
             switched) 
    
    choice$for_switch_model[, "switched_to"] <- factor(
      choice$for_switch_model[, "switched_to"],
      levels = c(
        "Switched-to party polling: 2%",
        "Switched-to party polling: 5%",
        "Switched-to party polling: 10%",
        "Switched-to party polling: 15%",
        "Switched-to party polling: 20%",
        "Switched-to party polling: 25%"
      )
    )
    
    choice$for_switch_model[, "switched_from"] <- factor(
      choice$for_switch_model[, "switched_from"],
      levels = c(
        "Switched-from party polling: 2%",
        "Switched-from party polling: 5%",
        "Switched-from party polling: 10%",
        "Switched-from party polling: 15%",
        "Switched-from party polling: 20%",
        "Switched-from party polling: 25%"
      )
    )
    
    choice$for_switch_model[, "switched_to_dynamic"] <- factor(
      choice$for_switch_model[, "switched_to_dynamic"],
      levels = c(
        "Switched-to party change in polling: 0",
        "Switched-to party change in polling: -10",
        "Switched-to party change in polling: -7",
        "Switched-to party change in polling: -5",
        "Switched-to party change in polling: -2",
        "Switched-to party change in polling: 2",
        "Switched-to party change in polling: 5",
        "Switched-to party change in polling: 7",
        "Switched-to party change in polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_from_dynamic"] <- factor(
      choice$for_switch_model[, "switched_from_dynamic"],
      levels = c(
        "Switched-from party change in polling: 0",
        "Switched-from party change in polling: -10",
        "Switched-from party change in polling: -7",
        "Switched-from party change in polling: -5",
        "Switched-from party change in polling: -2",
        "Switched-from party change in polling: 2",
        "Switched-from party change in polling: 5",
        "Switched-from party change in polling: 7",
        "Switched-from party change in polling: 10"
      )
    )

    
    # run function on data
    amce_r$switches <- amce_rsq(choice$for_switch_model,
                                switched ~ switched_to + switched_from +
                                  switched_to_dynamic + switched_from_dynamic +
                                  switched_to_emph + switched_to_dynamic_emph
    )
  })
  
  output$cand1 <- DT::renderDataTable({
    t(rv_shown$data1[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate One", ""), rownames = rep(""))
  
  output$cand2 <- DT::renderDataTable({
    t(rv_shown$data2[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Two", ""), rownames = rep(""))
  
  output$cand3 <- DT::renderDataTable({
    t(rv_shown$data3[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Three", ""), rownames = rep(""))
  
  output$cand4 <- DT::renderDataTable({
    t(rv_shown$data4[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Four", ""), rownames = rep(""))
  
  output$cand5 <- DT::renderDataTable({
    t(rv_shown$data5[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Five", ""), rownames = rep(""))
  
  output$choice <- DT::renderDataTable({
    choice$choice %>% arrange(desc(contest_no))
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("choices", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(choice$choice, file, sep = sep,
                  row.names = FALSE)
    })
  
  output$amce_plot <- renderPlot({
    ggplot(amce_r$amces, aes(estimate, level, colour = feature)) +
      geom_vline(linetype = "dashed", alpha = 0.5, xintercept = 0) +
      ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75)) +
      labs(x = "Change: Pr(Chosen Candidate)", y = "") +
      theme_minimal()
  })
  
  output$switch_plot <- renderPlot({
    ggplot(amce_r$switches, aes(estimate, level, colour = feature)) +
      geom_vline(linetype = "dashed", alpha = 0.5, xintercept = 0) +
      ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75)) +
      labs(x = "Change: Pr(Chosen Candidate)", y = "") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
