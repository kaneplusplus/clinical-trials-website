library(shiny)
library(shinyBS)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Sample-Size Calculator"),

  # Need to add tooltips for descriptions of inputs.

  # Pass syntax: 1 to by 0.4
   
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
#    sliderInput("num_strata", "Number of Strata:", min = 1, max = 10, 
#                value = 1),
#    bsTooltip("num_strata", "If this is 1, then the design is unstratified.",
#      "top", options(container="list")),
#    textInput("power", "Power:", value = "0.6 to 0.95 by 0.05"),
    numericInput("power", "Power:", value = 0.8, min = 0.1, max = 0.99, 
                 step = 0.01),
    bsTooltip("power", "The study power."),
    textInput("pref_effect", "Preference Effect", value = ".5 to 10 by 0.5"),
    bsTooltip("pref_effect", "The overall study preference effect.", 
              "top", options(container="list")),
    textInput("selection_effect", "Selection Effect", value = "3"),
    bsTooltip("selection_effect", "The overall study selection effect.", 
              "top", options(container="list")),
    textInput("treatment_effect", "Treatment Effect", value = "3"),
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
    textInput("stratum_prop", "Stratum Proportion:", value = "1"),
    bsTooltip("stratum_prop", 
              paste("The proportion of patients in each stratum.",
                    "Each value should be comma separated."),
                    "top", options(container="list")),
    sliderInput("alpha", "Type-1 Error Rate:", min=0.01, max=0.5, value=0.05),
    bsTooltip("alpha", "The desired type I error rate.", "top", 
              options(container="list")),
    downloadButton('downloadData', 'Download')
    # It would be nice to get rid of tooltips when the user doesn't want
    # them.
    #checkboxInput("tooltip_checkbox", label="Include help tooltips", 
    #              value=FALSE),
  ),
      
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("line_graph"),
    dataTableOutput("sample_size")
  )

))

