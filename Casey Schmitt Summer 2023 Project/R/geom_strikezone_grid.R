strikezone_grid <- reactive({
  data.frame(x = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
             y = c(1, 2, 3, 1, 2, 3, 4, 1, 2, 3),
             zone = c("Bottom Left", "Mid Left", "Top Left",
                      "Bottom Mid", "Mid Mid", "Top Mid",
                      "Bottom Right", "Mid Right", "Top Right"))
})