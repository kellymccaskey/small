shinyUI(pageWithSidebar(
  headerPanel('Monte Carlo Simulation Results'),
  sidebarPanel(
  radioButtons("stat", label = "Statistic:",
               choices = c("Expected Value" = "ev", 
                           "Bias" = "bias",
                           "Percent Bias" = "percent_bias", 
                           "Variance" = "var",
                           "Mean-Squared Error" = "mse"), 
               selected = "percent_bias"),
  radioButtons("k", label = "Number of Variables:",
               choices = c(3, 6, 9)),
  radioButtons("b0", label = "Intercept:",
               choices = c(-1, -0.5, 0))
  ),
  mainPanel(
    plotOutput("main_plot")
    )
))
