shinyServer(function(input, output) {
  
  # load packages
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  
  # plot parameter
  theme <- theme_gray()
  
  # read data
  sims <- read_csv("sims.csv")
  
  # collapse all stats into single column
  sims_tall <- gather(sims, stat, value, ev:mse)
  
  output$main_plot <- renderPlot(
{
  stat_subset_data <- subset(sims_tall, stat == input$stat)
  ymin <- with(stat_subset_data, min(value))
  ymax <- with(stat_subset_data, max(value))
  subset_data <- subset(sims_tall, k == input$k & 
                          b0 == input$b0 & 
                          stat == input$stat)
  print(glimpse(subset_data))
  ggplot(subset_data, aes(x = n, y = value, color = method)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = "Sample Size") +
    labs(color = "Method") +
    labs(y = "Statistic")
}
  )
})