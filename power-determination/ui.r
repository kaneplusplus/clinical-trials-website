library(shiny)
library(shinyBS)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Selection-Effect Power Calculator"),

  # Need to add tooltips for descriptions of inputs.

  # Pass syntax: 1 to 3 by 0.2
   
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    textInput("sample_size", "Sample Size:", value="10 to 60 by 2"),
    bsTooltip("sample_size", "The range of sample sizes.", "top",
      options(container="list")),
    numericInput("pref_effect", "Preference Effect:", value = 1),
    bsTooltip("pref_effect", "The overall study preference effect.",
              "top", options(container="list")),
    numericInput("selection_effect", "Selection Effect:", value = 2),
    bsTooltip("selection_effect", "The overall study selection effect.",
              "top", options(container="list")),
    numericInput("treatment_effect", "Treatment Effect:", value = 3),
    bsTooltip("treatment_effect", "The effect size of the treatment arm.",
              "top", options(container="list")),
    textInput("sigma2", "Within-Stratum Variances:", value="1"),
    bsTooltip("sigma2",
              paste("The within-stratum variances. There should be one",
                    "variance value per stratum (separated by commas).", "top",
              options(container="list"))),
    textInput("pref_prop", "Preference Proportion:", value = "0.5"),
    bsTooltip("pref_prop",
              paste("The proportion of patients preferring treatment 1",
                    "within each stratum. There should be one proportion",
                    "per stratum.", "top", options(container="list"))),
    textInput("choice_prop", "Choice Proportion:", value = "0.5"),
    bsTooltip("choice_prop",
              paste("The proportion of patients in the choice arm.",
                    "top", options(container="list"))),
    textInput("stratum_prop", "Stratum Proportion:", value = "1"),
    bsTooltip("stratum_prop",
              paste("The proportion of patients in each stratum.",
                    "Each value should be comma separated."),
                    "top", options(container="list")),
    sliderInput("alpha", "Type-1 Error Rate:", min=0.01, max=0.5, value=0.05),
    bsTooltip("alpha", "The desired type I error rate.", "top",
              options(container="list")),
    downloadButton('downloadData', 'Download')
  ),
      
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("line_graph"),
    dataTableOutput("sample_size")
  )

))

