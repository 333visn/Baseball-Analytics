library(shiny)
library(baseballr)
library(dplyr)
library(ggplot2)
library(shinythemes)

webb = read.csv(file = "data/loganwebb2022stats.csv")
ui <- navbarPage(theme = shinytheme("yeti"), title = "Logan Webb Release Speeds 2022",
      tabPanel("Release Speeds Visual",
      sidebarLayout(
      sidebarPanel(
      selectInput(inputId = "inning", label = "Inning", choices = c("All", 1:9)),
      selectInput(inputId = "outs", label = "Outs", choices = c("All", 0:2)),
      selectInput(inputId = "stance", label = "Batter Side", choices = c("All", "Right", "Left")),
      selectInput(inputId = "pitch_type", label = "Pitch Type", choices = c("All", "Sinker", "4-Seam", "Changeup", "Slider"))),
      mainPanel(
      plotOutput(outputId = "speed_plot"),
      tabsetPanel(
        tabPanel("Fastest pitches in given scenario",
                 fluidRow(column(12, tableOutput("table"))))),
      tabPanel(
        titlePanel(title = "About"),
        mainPanel(includeMarkdown("about.Rmd"))
      )))))

server <- function(input, output) {
  webb62 <- reactive({
    webb_arsenal <- webb
    webb_arsenal <- pitcher_arsenal(webb_arsenal, input)
    return(webb_arsenal)
  })
  output$speed_plot <- renderPlot({
    ggplot(data = webb62(), aes(x = inning, y = release_speed)) +
      geom_point(color = "orange") +
      labs(x = "Inning", y = "Release Speed (mph)") +
      ylim(78, 96)
  })
  output$table <- renderTable({
    return(webb %>% 
    select(home_team, away_team, description, release_speed, 
           outs_when_up, inning, pitch_type, stand, type) %>%
      arrange(desc(release_speed)) %>% 
      head(n = 15))
  })
}

shinyApp(ui, server)