library(preference)
library(ggplot2)
library(tidyr)
library(tibble)

source("util.r")

parse_sequence_text = function(x) {
  suppressWarnings({ret = as.numeric(x)})
  if (is.na(ret)) {
    # Try to parse it as a sequence.

    to_by = regexpr("\\d+\\.?\\d*\\s?to\\s?\\d+\\.?\\d*\\s+by", x)
    to = regexpr("\\d+\\.?\\d*\\s?to\\s?\\d+\\.?\\d*", x)
    if (to_by != -1) {
      # It is a "to-by" statement?
      vals = as.numeric(unlist(strsplit(x, "to|by")))
      ret = seq(from=vals[1], to=vals[2], by=vals[3])
    } else if (to != -1) {
      # It is a "to" statement?    
      vals = as.numeric(unlist(strsplit(x, "to")))
      ret = seq(from=vals[1], to=vals[2], by=1)
    } else {
      # We can't parse it.
      ret = NA
    }
  }
  ret
}

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  get_inputs = reactive({
    list(
      sample_size = parse_sequence_text(input$sample_size),
      pref_effect = input$pref_effect,
      selection_effect = input$selection_effect,
      treatment_effect = input$treatment_effect,
      sigma2 = as.numeric(unlist(strsplit(input$sigma2, ","))),
      pref_prop = as.numeric(unlist(strsplit(input$pref_prop, ","))),
      choice_prop = as.numeric(unlist(strsplit(input$choice_prop, ","))),
      stratum_prop = as.numeric(unlist(strsplit(input$stratum_prop, ","))),
      alpha = input$alpha)
  })

  get_power = reactive({ 
    params = get_inputs()
    pt_from_ss(ss = params$sample_size, 
               pref_effect = params$pref_effect,
               selection_effect = params$selection_effect,
               treatment_effect = params$treatment_effect,
               sigma2 = params$sigma2,
               pref_prop = params$pref_prop,
               choice_prop = params$choice_prop,
               stratum_prop = params$stratum_prop,
               alpha = params$alpha)
  })

  output$sample_size = renderDataTable({
    get_power()
  })

  output$line_graph = renderPlot({
    df <- get_power()
    if (nrow(df) < 2) {
      NULL
    } else {
      dfs <- df[,c(1, 13:15)] %>% as_tibble
      names(dfs) <- c("Sample Size", "Treatment", "Selection", "Preference")
      gather(dfs, key = "Type", value = "Power", 2:4) %>%
        ggplot(aes(x = `Sample Size`, y = Power, group = Type, color = Type)) +
        geom_line() + theme_minimal()
  
    }
  })

  output$downloadData = downloadHandler(
    filename = function() {
      paste("selection-power-report", "zip", sep=".")
    },
    content = function(fname) {
      doc_template = readLines("ssc.rmd")
      current_wd = getwd()
      tmpdir = tempdir()
      setwd(tempdir())

      params = get_inputs()
      df <- data.frame(list(
        sample_size = rep(params$sample_size, each=length(params$delta_nu)),
        delta_nu = rep(params$delta_nu, length(params$delta_nu))))
      power <- rep(NA, nrow(df))
      for (i in 1:nrow(df)) {
        power[i] <- round(selection_power(df$sample_size[i], params$phi,
          params$sigma2, params$delta_pi, df$delta_nu[i], params$alpha,
          params$theta, params$xi, params$num_strata), digits=3)
      }
      df$power <- power
      names(df) = c("Sample Size", "Selection Effect", "Power")

      df_path = "selection-power.csv"
      write.csv(df, df_path, row.names=FALSE)
      report_dest = "selection-power-report.docx"
      rmd_content = create_strat_power_report(doc_template, params, df_path)
      tf = paste0(tempfile(), ".rmd")
      writeLines(rmd_content, tf)
      render(tf, output_file=report_dest)
      ret = zip(zipfile=fname, files=c(df_path, report_dest))
      setwd(current_wd)
      ret
    },
    contentType = "application/zip"
  )

})

