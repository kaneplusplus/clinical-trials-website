library(shiny)
library(shinyBS)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Effect Size Calculator"),

  # Need to add tooltips for descriptions of inputs.

  # Pass syntax: 1 to by 0.4
   
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    textInput("mu1", "Random Arm Treatment 1 Mean Response (mu1):", 
      value="2 to 10"),
    textInput("mu2", "Random Arm Treatment 2 Mean Response (mu2):", 
      value="2"),
    textInput("mu11", "Choice Arm Treatment 1 Mean Response (mu11):", 
      value="2"),
    textInput("mu22", "Choice Arm Treatment 2 Mean Response (mu22):", 
      value="2"),
    textInput("phi", "Proportion of Patients Preferring Treatment 1 (phi):",
      value="0.4 to 0.6 by 0.1"),
    bsTooltip("mu1", "The mean response of the patients receiving treatment 1 in the random arm"),
    bsTooltip("mu2", "The mean response of the patients receiving treatment 2 in the random arm"),
    bsTooltip("mu11", "The mean response of the patients receiving treatment 1 in the choice arm"),
    bsTooltip("mu22", "The mean response of the patients receiving treatment 2 in the choice arm."),
    bsTooltip("phi", "The proportion of patients preferring treatment 1."),
    downloadButton('downloadData', 'Download')
  ),
      
  # Show a plot of the generated distribution
  mainPanel(
    #trelliscopeOutput("effect_viz"),
    dataTableOutput("effect_size")
  )

))

