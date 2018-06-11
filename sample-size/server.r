library(rbokeh)
library(preference)
library(ggplot2)
library(tibble)

source("util.r")

parse_sequence_text = function(x) {
  suppressWarnings({ret <- as.numeric(x)})
  if (is.na(ret)) {
    # Try to parse it as a sequence.

    to_by <- regexpr("\\d+\\.?\\d*\\s?to\\s?\\d+\\.?\\d*\\s+by", x)
    to <- regexpr("\\d+\\.?\\d*\\s?to\\s?\\d+\\.?\\d*", x)
    if (to_by != -1) {
      # It is a "to-by" statement?
      vals <- as.numeric(unlist(strsplit(x, "to|by")))
      ret <- seq(from=vals[1], to=vals[2], by=vals[3])
    } else if (to != -1) {
      # It is a "to" statement?    
      vals <- as.numeric(unlist(strsplit(x, "to")))
      ret <- seq(from=vals[1], to=vals[2], by=1)
    } else {
      # We can't parse it.
      ret <- NA
    }
  }
  ret
}

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  # Transform the inputs into the parameters that will go to the
  # pt_from_power function.
  get_inputs <- reactive({
    ret <- list(power = input$power,
         pref_effect = parse_sequence_text(input$pref_effect),
         selection_effect = parse_sequence_text(input$selection_effect),
         treatment_effect = parse_sequence_text(input$treatment_effect),
         sigma2 = as.numeric(unlist(strsplit(input$sigma2, ","))),
         pref_prop = as.numeric(unlist(strsplit(input$pref_prop, ","))),
         stratum_prop = as.numeric(unlist(strsplit(input$stratum_prop, ","))),
         alpha = input$alpha)
  })

  get_strat_selection <- reactive({ 
    params <- get_inputs()
    preference::pt_from_power(
      power = params$power,
      pref_effect = params$pref_effect,
      selection_effect = params$selection_effect,
      treatment_effect = params$treatment_effect,
      sigma2 = params$sigma2,
      pref_prop = params$pref_prop,
      stratum_prop = params$stratum_prop,
      alpha = params$alpha)
  })

  output$sample_size <- renderDataTable({
    x <- get_strat_selection()
  })

  output$line_graph <- renderPlot({
    ss <- get_strat_selection()
    effect_lengths <- c(length(unique(ss$pref_effect)), 
                        length(unique(ss$selection_effect)),
                        length(unique(ss$treatment_effect)))
    if (sum(effect_lengths > 1) > 1 || sum(effect_lengths) == 3) {
      error("Visualizations are shown when either preference or",
            " selection or treatment effects varies.")
    } else {
      names(ss)[c(2, 4, 6)] <- c("Preference", "Selection", "Treatment")
      gather(ss, key = `Sample Size Type`, value = `Sample Size`, 
             c("pref_ss", "selection_ss", "treatment_ss")) %>% 
        as_tibble %>% 
        gather(key = `Effect Type`, value = `Effect Size`, 
               c("Preference", "Selection", "Treatment")) %>%
        ggplot(aes(y = `Effect Size`, x = `Sample Size`, group = `Effect Type`,
                   color = `Effect Type`)) + geom_line() + theme_minimal()
    }
#    pt_plot(get_strat_selection())
  })

  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("preference-sample-size-report", "zip", sep=".")
    },
    content <- function(fname) {
      doc_template <- readLines("ssc.rmd")
      current_wd <- getwd()
      tmpdir <- tempdir()
      setwd(tempdir())
      params <- get_inputs()
      df <- get_strat_selection()
      df_dest <- "preference-sample-size.dput"
      saveRDS(df, file=df_dest)
      report_dest <- "preference-sample-size-report.docx"
      browser()
      rmd_content <- create_strat_selection_report(doc_template, params, 
                                                   df_dest)
      tf <- paste0(tempfile(), ".rmd")
      writeLines(rmd_content, tf)
      render(tf, output_file=report_dest)
      ret <- zip(zipfile=fname, files=c(df_dest, report_dest))
      setwd(current_wd)
      ret
    },
    contentType = "application/zip"
  )

})

