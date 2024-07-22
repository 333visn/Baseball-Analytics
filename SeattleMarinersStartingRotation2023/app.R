# Files to run for app.R to work: setup.R, geom_strikezone.R (user should not have to run RotationData23.R for app.R to work)
library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

Sea <- read_csv("data/Mariners2023Rotation.csv")

Sea <- Sea %>%
  mutate(p_throws = fct_recode(stand, "Right" = "R", "Left" = "L")) %>%
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH", "Slider" = "SL", "4-Seam" = "FF", "Sinker" = "SI", 
                                 "Curveball" = "CU", "Cutter" = "FC", "Splitter" = "FS", "Sweeper" = "ST", "Knuckle Curve" = "KC",
                                 "Knuckle Ball" = "KN", "Screwball" = "SC", "Slurve" = "SV", "IDK" = "", "Pitchout" = "PO", "Unknown Pitch" = "NA")) %>%
  mutate(type = fct_recode(type, "Strike" = "S", "Ball" = "B", "Ball in Play" = "X")) %>%
  mutate(events = fct_recode(events, "No event" = "", "Strikeout" = "strikeout", "Field out" = "field_out", "Single" = "single", "Force out" = "force_out",
                             "Field error" = "field_error", "Double" = "double", "Home run" = "home_run",
                             "Sac fly" = "sac_fly", "Double Play" = "grounded_into_double_play", "Hit by Pitch" = "hit_by_pitch")) %>%
  filter(pitch_type != "NA") %>%
  filter(game_type == "R")

Raleigh <-  Sea %>%
  filter(description %in% c("ball", "called_strike", "blocked_ball")) %>%
  mutate("catcher_name" = case_when(fielder_2 == 663728 ~  "Raleigh, Cal", fielder_2 == 620443 ~ "Torrens, Luis",
                                    fielder_2 == 657247 ~ "O'Keefe, Brian", fielder_2 == 608596 ~ "Murphy, Tom"))


ui <- fluidPage(
  titlePanel("Seattle Mariners Starting Pitching 2023"),
  navbarPage("Tabs",
             theme = shinytheme("flatly"),
              tabPanel("Strikezone Grid Map", sidebarLayout(
              sidebarPanel(
                selectInput("player_name", "Player Name", choices = unique(Sea$player_name), selected = unique(Sea$player_name)),
                #sliderInput("inning", "Inning", min = 1, max = 9, value = 1, step = 1, ticks = FALSE),
                checkboxGroupInput("inning", "Inning", choices = c(1:9), selected = c(1:9)),
                #sliderInput("outs_when_up", "Outs in Inning", min = 0, max = 2, value = 0, step = 1, ticks = FALSE),
                checkboxGroupInput("outs_when_up", "Outs in Inning", choices = c(0:2), selected = c(0:2)),
                #selectInput("stand", "Batter Stance", choices = unique(Sea$stand), selected = unique(Sea$stand)),
                checkboxGroupInput("stand", "Batter Stance", choices = unique(Sea$stand), selected = unique(Sea$stand))
              ),
              mainPanel(
                plotOutput("scatterPlot", height = "500")
              )
            )),
            tabPanel("Pitch Arsenal",
              sidebarPanel(
                selectInput("player_name1", "Player Name", choices = unique(Sea$player_name), selected = unique(Sea$player_name)),
                checkboxGroupInput("pitch_type1", "Pitch Types", choices = unique(Sea$pitch_type), selected = unique(Sea$pitch_type))
              ), 
              mainPanel(plotOutput("velocityPlot", height = "500")),
              sidebarPanel(
                selectInput("pitchfn", "Select Stat", choices = c("WOBA", "BABIP"),  selected = "WOBA")),
              mainPanel(plotOutput("wobaPlot", height = "500"), tableOutput("wobaTable"))
            ),
            tabPanel("Catcher Impact", 
              sidebarPanel(
                selectInput("catcher_name", "Catcher Name", choices = unique(Raleigh$catcher_name), selected = "Cal Raleigh"),
                selectInput("player_name2", "Player Name", choices = unique(Raleigh$player_name), selected = unique(Raleigh$player_name)),
                checkboxGroupInput("description", "Description", choices = unique(Raleigh$description), selected = unique(Raleigh$description))
                ),
              mainPanel(plotOutput("catcherPlot")), 
              sidebarPanel(selectInput("CatcherRepLabel", "Number or Percentage", choices = c("Number", "Percentage"), selected = "Number")),
              mainPanel(plotOutput("catcherPie"), plotOutput("catcherTable"))
            ),
            tabPanel("League Comparison", sidebarLayout(
              sidebarPanel(
                selectInput("stat_type", "Select Stat", choices = c("Runs Prevented Above Average", "Runs Prevented Above Replacement", 
                                                                    "Innings Pitched (Starting)", "Expected Runs Allowed", "ERA+", 
                                                                    "xRA With Defense", "Defensive WAR"), selected = "Runs above average"),
              ),
              mainPanel(plotOutput("warPlot", height = "500"))
            )),
            tabPanel("About", mainPanel(includeMarkdown("about.Rmd"))))
)

server <- function(input, output) {
  
  SeaPitches <- reactive({
    subset(subset(subset(subset(Sea, stand %in% input$stand), inning %in% input$inning), outs_when_up %in% input$outs_when_up), player_name %in% input$player_name)
  })
  
  SeaVelo <- reactive({
    subset(subset(Sea, pitch_type %in% input$pitch_type1), player_name %in% input$player_name1)
  })
  
  SeaCatchers <- reactive({
    subset(subset(subset(Raleigh, description %in% input$description), player_name %in% input$player_name2), catcher_name %in% input$catcher_name)
  })
  
  MLBpitchstat1 = bwar_pit23$runs_above_avg
  
  MLBPitcherStat <- reactive({
    if (input$stat_type == "Runs Prevented Above Average") {
      MLBpitchstat1 = bwar_pit23$runs_above_avg
    } else if (input$stat_type == "Innings Pitched (Starting)"){
      MLBpitchstat1 = bwar_pit23$IPouts_start
    } else if (input$stat_type == "Expected Runs Allowed") {
      MLBpitchstat1 = bwar_pit23$xRA
    } else if (input$stat_type == "ERA+") {
      MLBpitchstat1 = bwar_pit23$ERA_plus
    } else if (input$stat_type == "xRA With Defense") {
      MLBpitchstat1 = bwar_pit23$xRA_def_pitcher
    } else if (input$stat_type == "Defensive WAR") {
      MLBpitchstat1 = bwar_pit23$dWAR
    } else {
      MLBpitchstat1 = bwar_pit23$runs_above_rep
    }
    return(MLBpitchstat1)
  })
  
  CatcherRepDescription = paste(round(CatcherReps$reps / sum(CatcherReps$reps) * 100, 2), "%")
  
  CatcherRepLabel <- reactive({
    if (input$CatcherRepLabel == "Percentage") {
      CatcherRepDescription = paste(round(CatcherReps$reps / sum(CatcherReps$reps) * 100, 2), "%")
    } else {
      CatcherRepDescription = CatcherReps$reps
    }
  })
  
  Pitchfn <- wobaAgainst$WOBA
  PitchFn <- reactive({
    if (input$pitchfn == "WOBA") {
      Pitchfn <- wobaAgainst$WOBA
    } else {
      Pitchfn <- wobaAgainst$BABIP
    }
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(SeaPitches(), aes(x = plate_x, y = plate_z, color = type)) +
      geom_point(size = 10) +
      labs(x = NULL, y = NULL, title = "Strikezone Grid") +
      scale_color_manual(values = c("Strike" = "firebrick2", "Ball" = "seagreen4", "Ball in Play" = "navyblue")) +
      coord_fixed(ratio = 0.75) +
      xlim(-3, 3) +
      ylim(-2, 6) +
      geom_strikezone()
  })
  
  output$velocityPlot <- renderPlot({
    ggplot(data = SeaVelo(), aes(x = inning, y = release_speed, fill = pitch_type)) +
      geom_point(aes(color = pitch_type)) +
      labs(x = "Inning", y = "Release Speed (mph)", title = "Pitcher Velocity and Selection Chart") +
      scale_x_continuous(breaks=seq(0,9,by=1)) +
      ylim(70, 105)
  })
  
  output$catcherPlot <- renderPlot({
    ggplot(SeaCatchers(), aes(x = plate_x, y = plate_z, color = description)) +
      geom_point(size = 10) +
      labs(x = NULL, y = NULL, title = "Strikezone Grid") +
      scale_color_manual(values = c("ball" = "seagreen4", "blocked_ball" = "navyblue", "called_strike" = "firebrick2")) +
      xlim(-3, 3) +
      ylim(-2, 6) +
      coord_fixed(ratio = 0.75)+
      geom_strikezone()
  })
  
  output$catcherTable <- renderPlot({
    ggplot(catcher_dwar23, aes(x = team_ID, y = dWAR,fill = ifelse(team_ID == "SEA", "Seattle Mariners", "Other MLB teams"))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("navyblue", "seagreen4")) +
      xlab("Team") +
      theme(legend.position = "none") +
      ylim(-5, 5) +
      coord_flip() +
      labs(title = "MLB Catcher Defensive WAR 2023")
  })
  
  output$catcherPie <- renderPlot({
    ggplot(CatcherReps, aes (x="", y = reps, fill = factor(catcher_name))) + 
      geom_col(position = 'stack', width = 1) +
      geom_text(aes(label = CatcherRepLabel(), x = 1.3),
                position = position_stack(vjust = 0.5)) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      scale_fill_brewer(palette = "Paired") +
      labs(fill = "catcher_name",
           x = NULL,
           y = NULL,
           title = "Amount of Pitches thrown by Catcher") + 
      coord_polar("y")
  })

  
  output$warPlot <- renderPlot({
    ggplot(bwar_pit23, aes(x = MLBPitcherStat(), y = WAR)) +
      geom_point() +
      geom_label(aes(label = team_ID, fill = ifelse(team_ID == "SEA", "Seattle Mariners", "Other MLB teams"))) +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("ghostwhite", "seagreen4")) +
      xlab(input$stat_type) +
      labs(title = paste("MLB Starting Pitching WAR vs", input$stat_type))+
      ylab("Starting Pitching WAR (Minimum 10 starts)")
  })
  
  output$wobaPlot <- renderPlot({
    ggplot(wobaAgainst, aes(x = player_name, y = PitchFn(), fill = pitch_name)) +
      geom_bar(stat = "identity") +
      labs(title = "WOBA and BABIP per Pitch Type") +
      ylab(input$pitchfn) + 
      xlab("Pitcher Name")
  })
  
  output$wobaTable <- renderTable({
    wobaAgainst
  })
}  

shinyApp(ui, server)