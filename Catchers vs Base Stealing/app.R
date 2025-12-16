library(tidyverse)
library(shiny)
library(DT)
library(bslib)

ui <- fluidPage(
  
  titlePanel("Catcher vs Base Stealer Dashboard"),
  
  theme = bs_theme(preset = "pulse"),
  input_dark_mode(id = "dark_mode_toggle"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("catcher", "Select Catcher:", choices = NULL, multiple = TRUE),
      selectInput("pitchclass", "Pitch Class (FB/BRK/OFF):", choices = NULL, multiple = TRUE),
      selectInput("count", "Numeric Count:", choices = NULL, multiple = TRUE),
      selectInput("group", "Count Group (Ahead / Behind / Even):", choices = NULL, multiple = TRUE),
      
      checkboxGroupInput("bools", "Pitch Booleans:",
                         choices = c("OutsideZone", "Swing", "Chase"),
                         selected = NULL),
      
      hr(),
      helpText("Pitch location & pop-time visualizations available in separate tabs.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        tabPanel("CS% Bar Chart", plotOutput("cs_plot")),
        tabPanel("Attempts vs CS%", plotOutput("scatter_plot")),
        tabPanel("Pitch Location", plotOutput("location_heatmap")),
        tabPanel("Throw Metrics", plotOutput("throw_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  sb_data <- reactive({
    sb <- prep_sb_data(df)
    
    updateSelectInput(session, "catcher", choices = sort(unique(sb$Catcher)))
    updateSelectInput(session, "pitchclass", choices = sort(unique(sb$PitchClass)))
    updateSelectInput(session, "count", choices = sort(unique(sb$count)))
    updateSelectInput(session, "group", choices = sort(unique(sb$group)))
    
    sb
  })
  
  filtered <- reactive({
    sb <- sb_data()
    
    if (!is.null(input$catcher)) {
      sb <- sb %>% filter(Catcher %in% input$catcher)
    }
    if (!is.null(input$pitchclass)) {
      sb <- sb %>% filter(PitchClass %in% input$pitchclass)
    }
    if (!is.null(input$count)) {
      sb <- sb %>% filter(count %in% input$count)
    }
    if (!is.null(input$group)) {
      sb <- sb %>% filter(group %in% input$group)
    }
    if (!is.null(input$bools)) {
      for (b in input$bools) {
        sb <- sb %>% filter(.data[[b]] == TRUE)
      }
    }
    
    sb
  })
  
  summary_stats <- reactive({
    filtered() %>%
      group_by(Catcher) %>%
      summarize(
        Attempts = n(),
        SB_Allowed = sum(SB_Success),
        CS = sum(SB_Fail),
        CS_Rate = round(mean(SB_Fail) * 100, 1),
        
        Avg_PopTime = mean(PopTime, na.rm = TRUE),
        Avg_ThrowSpeed = mean(ThrowSpeed, na.rm = TRUE),
        Avg_Exchange = mean(ExchangeTime, na.rm = TRUE),
        Avg_TimeToBase = mean(TimeToBase, na.rm = TRUE)
      )
  })
  
  output$summary_table <- renderDT(summary_stats())
  
  output$cs_plot <- renderPlot({
    summary_stats() %>%
      ggplot(aes(x = reorder(Catcher, -CS_Rate), y = CS_Rate)) +
      geom_col() +
      coord_flip() +
      labs(title = "Catcher Caught-Stealing Rate", x = "Catcher", y = "CS%")
  })
  
  output$scatter_plot <- renderPlot({
    summary_stats() %>%
      ggplot(aes(x = Attempts, y = CS_Rate, label = Catcher)) +
      geom_point(size = 3) +
      geom_text(nudge_y = 1.5) +
      labs(title = "Steal Attempts vs CS%", x = "Attempts", y = "CS%")
  })
  
  output$location_heatmap <- renderPlot({
    filtered() %>%
      ggplot(aes(x = pfxx, y = pfxz)) +
      stat_density2d_filled(alpha = 0.8) +
      labs(title = "Pitch Location Heatmap (on SB Attempts)", x = "Horizontal Break", y = "Vertical Break")
  })
  
  output$throw_plot <- renderPlot({
    filtered() %>%
      ggplot(aes(x = PopTime, y = ThrowSpeed)) +
      geom_point(alpha = 0.6) +
      labs(title = "Pop Time vs Throw Speed", x = "Pop Time (sec)", y = "Throw Speed (mph)")
  })
}

shinyApp(ui, server)