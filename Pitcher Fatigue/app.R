library(shiny)
library(tidyverse)
library(zoo)
library(bslib)


ui <- fluidPage(
  
  titlePanel("Pitcher Fatigue Dashboard"),
  
  theme = bs_theme(preset = "cosmo"),
  input_dark_mode(id = "dark_mode_toggle"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pitcher", "Pitcher:", choices = pitcher_list),
      
      uiOutput("game_picker"),
      
      selectInput("pitch_class", "Pitch Class:",
                  choices = pitch_class_list,
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Velocity", plotOutput("veloPlot")),
        tabPanel("Spin Rate", plotOutput("spinPlot")),
        tabPanel("Command", plotOutput("commandPlot")),
        tabPanel("Release Point Drift", plotOutput("rpPlot")),
        tabPanel("Release Point Heatmap", plotOutput("rpHeatmap")),
        tabPanel("Drift by Pitch Type", plotOutput("rpByTypePlot")),
        tabPanel("Fatigue Index", plotOutput("fatiguePlot"),
                 verbatimTextOutput("fatigueEvents"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Games available for selected pitcher ----
  output$game_picker <- renderUI({
    req(input$pitcher)
    games <- df %>% filter(Pitcher == input$pitcher) %>% pull(GameDate) %>% unique()
    selectInput("game", "Game Date:", choices = sort(games))
  })
  
  # ---- FILTERED DATA REACTIVE ----
  gameData <- reactive({
    req(input$pitcher, input$game)
    
    g <- df %>%
      filter(Pitcher == input$pitcher,
             GameDate == input$game)
    
    if (input$pitch_class != "All") {
      g <- g %>% filter(PitchClass == input$pitch_class)
    }
    
    g
  })
  
  # ---- VELO PLOT ----
  output$veloPlot <- renderPlot({
    gameData() %>%
      ggplot(aes(pitch_in_outing, RelSpeed)) +
      geom_point(alpha = 0.4) +
      geom_smooth(se = FALSE) +
      labs(title = "Velocity Over Outing",
           x = "Pitch Number",
           y = "Velocity (mph)")
  })
  
  # ---- SPIN PLOT ----
  output$spinPlot <- renderPlot({
    req(gameData()$SpinRate)
    
    gameData() %>%
      ggplot(aes(pitch_in_outing, SpinRate)) +
      geom_point(alpha = 0.4) +
      geom_smooth(se = FALSE) +
      labs(title = "Spin Rate Over Outing",
           x = "Pitch Number",
           y = "Spin Rate (rpm)")
  })
  
  # ---- COMMAND PLOT (location variability) ----
  output$commandPlot <- renderPlot({
    gameData() %>%
      ggplot(aes(pitch_in_outing, loc_sd)) +
      geom_line() +
      geom_smooth(se = FALSE) +
      labs(title = "Command Deterioration (Location Variability)",
           x = "Pitch Number",
           y = "Location Variability (SD)")
  })
  
  # ---- RELEASE POINT DRIFT PLOT ----
  output$rpPlot <- renderPlot({
    gameData() %>%
      ggplot(aes(pitch_in_outing, rel_drift)) +
      geom_line() +
      geom_smooth(se = FALSE) +
      labs(
        title = "Release Point Drift (Fatigue Indicator)",
        x = "Pitch Number",
        y = "Drift from Average Release Point (ft)"
      )
  })
  
  output$rpHeatmap <- renderPlot({
    gameData() %>%
      ggplot(aes(RelSide, RelHeight, fill = pitch_in_outing)) +
      geom_bin2d(bins = 30) +
      scale_fill_viridis_c() +
      labs(
        title = "Release Point Heatmap",
        x = "Horizontal Release (ft)",
        y = "Vertical Release (ft)",
        fill = "Pitch #"
      )
  })
  
  output$rpByTypePlot <- renderPlot({
    gameData() %>%
      ggplot(aes(pitch_in_outing, rel_dist, color = PitchClass)) +
      geom_line(alpha = 0.7) +
      geom_smooth(se = FALSE) +
      labs(
        title = "Release Point Drift by Pitch Type",
        x = "Pitch Number",
        y = "Drift (ft)",
        color = "Pitch Type"
      )
  })
  
  output$fatiguePlot <- renderPlot({
    gameData() %>%
      ggplot(aes(pitch_in_outing, fatigue_index)) +
      geom_line(color = "blue") +
      geom_smooth(se = FALSE) +
      labs(
        title = "Fatigue Index (Combined Metrics)",
        x = "Pitch Number",
        y = "Fatigue Index (z-score sum)"
      )
  })
  
  output$fatigueEvents <- renderPrint({
    x <- gameData()$fatigue_index
    spikes <- which(diff(x) > 0.75)
    
    if (length(spikes) == 0) {
      cat("No fatigue spikes detected.")
    } else {
      cat("Fatigue spikes detected at pitches:\n")
      print(spikes + 1)
    }
  })
  
  
}

shinyApp(ui, server)
