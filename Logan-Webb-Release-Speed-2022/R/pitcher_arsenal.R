pitcher_arsenal = function(data, input) {
  if (input$inning != "All") {
    data <- data %>% filter(inning == input$inning)
  }
  if (input$outs != "All") {
    data <- data %>% filter(outs_when_up == input$outs)
  }
  if (input$stance != "All") {
    data <- data %>% filter(stand == input$stance)
  }
  if (input$pitch_type != "All") {
    data <- data %>% filter(pitch_type == input$pitch_type)
  }
  return(data)
}
