library(shiny)
library(baseballr)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(ggpubr)


# Schmitt: 669477

schmitt = read.csv(file = "data/caseyschmitt2023stats.csv")

ui <- fluidPage(
  titlePanel("Casey Schmitt Pitch Analysis"),
  navbarPage("Tabs",
    theme = shinytheme("united"),
    tabPanel("Pitch-by-Pitch Analysis",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("pitch_type", "Pitch Types", choices = unique(schmitt$pitch_type), selected = unique(schmitt$pitch_type)),
        checkboxGroupInput("events", "Events", choices = unique(schmitt$events), selected = unique(schmitt$events))
      ),
      mainPanel(
        plotOutput("scatterPlot", height = "500"),
        plotOutput("eventPlot", height = "500")
      )
    )
  ),
    tabPanel("Situational Performance", mainPanel(
      plotOutput("situationalPlot") 
    )),
    tabPanel("Swing Analysis", mainPanel(
      plotOutput("swing_miss_chart") 
    )),
    tabPanel("Table", dataTableOutput("schmitt_table")),
    tabPanel("About", mainPanel(includeMarkdown("about.Rmd")))
  )
)

server <- function(input, output) {
  # Table
  output$schmitt_table <- renderDataTable({
    caseyschmitt2023stats
  })
  
  SchmittPitches <- reactive({
    subset(schmitt, pitch_type %in% input$pitch_type)
  })
  SchmittEvents <- reactive({
    subset(schmitt, events %in% input$events)
  })
  output$scatterPlot <- renderPlot({
    ggplot(SchmittPitches(), aes(x = plate_x, y = plate_z, color = type)) +
      geom_point(size = 10) +
      labs(x = NULL, y = NULL, title = "Pitch Location Scatter Plot") +
      scale_color_manual(values = c("Strike" = "red", "Ball" = "green", "X" = "blue")) +
      coord_fixed(ratio = 0.75)+
      geom_strikezone()
      })
  output$eventPlot <- renderPlot({
    ggplot(SchmittEvents(), aes(x = plate_x, y = plate_z, color = events)) +
      geom_point(size = 10) +
      labs(x = NULL, y = NULL, title = "Event Location Scatter Plot") +
      geom_strikezone() +
      scale_color_manual(values = c("Strikeout" = "red", "Home run" = "yellow", "Walk" = "blue", "Single" = "seagreen", "Double" = "green", "Triple" = "lightgreen",
                                    "No event" = "black", "Field error" = "purple", "Field out" = "darkorange", "Force out" = "orange",
                                    "Sac fly" = "aquamarine1", "Double Play" = "chocolate", "Hit by Pitch" = "darkblue"))  # Set custom colors for each event
  })

# Situational performance

schmitt_sitution <- schmitt %>%
  mutate(slugging = case_when(events == "Single" ~ 1, events == "Double" ~ 2, events == "Triple" ~ 3, events == "Home Run" ~ 4, .default = 0)) %>%
  mutate(at_bats = sum(case_when(events == c("Single", "Double", "Triple", "Home Run", "Strikeout", "Field out", "Double Play", "Force out") ~ 1, .default = 0)),
         hits = ifelse(events %in% c("Single", "Double", "Triple", "Home Run"), 1, 0),
         batting_average = hits / at_bats,
         slugging_percentage = slugging / at_bats,
         runners_on_base = ifelse(!is.na(on_3b) | !is.na(on_2b) | !is.na(on_1b), "Runners on Base", "Bases Empty")) %>%
  group_by(inning, runners_on_base) %>%
  summarize(avg_batting = mean(batting_average) * 100,
            avg_slugging = mean(slugging_percentage) * 100) %>%
  filter(!is.na(avg_batting))

# Create the situational plot
output$situationalPlot <- renderPlot({
  ggplot(schmitt_sitution, aes(x = inning, fill = runners_on_base)) +
    geom_bar(aes(y = avg_batting), stat = "identity", position = "dodge") +
    #geom_bar(aes(y = avg_slugging), stat = "identity", position = "dodge", alpha = 0.5) +
    labs(title = "Situational Performance",
         x = "Outs When Up",
         y = "Average") +
    scale_fill_manual(values = c("black", "darkorange3"), labels = c("Runners on Base", "Bases Empty")) +
    theme_minimal()
  })

# Swing rate charts
swing_miss_data <- schmitt %>%
  group_by(pitch_type) %>%
  summarise(total_swings = sum(description %in% c("foul", "hit_into_play", "swinging_strike")),
            total_misses = sum(description == "swinging_strike"),
            swing_miss_rate = total_misses / total_swings) %>%
  arrange(desc(swing_miss_rate))

output$swing_miss_chart <- renderPlot({
  ggplot(swing_miss_data, aes(x = swing_miss_rate * 100, y = reorder(pitch_type, -swing_miss_rate))) +
    geom_bar(stat = "identity", fill = "darkorange3", color = "black") +
    coord_flip() +
    labs(title = "Swing and Miss Rate by Pitch Type",
         x = "Pitch Type",
         y = "Swing and Miss Rate (%)") +
    theme_minimal()
})

}
shinyApp(ui, server)
