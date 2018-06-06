library(preference)
library(ggplot2)

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
    delta_pi = parse_sequence_text(input$delta_pi)
    delta_nu = parse_sequence_text(input$delta_nu)
    sample_size = parse_sequence_text(input$sample_size)
    # Turn the text inputs into vectors of numeric values.
    phi = as.numeric(gsub(" ", "", unlist(strsplit(input$phi, ","))))
    sigma2 = as.numeric(gsub(" ", "", unlist(strsplit(input$sigma2, ","))))
    xi = as.numeric(gsub(" ", "", unlist(strsplit(input$xi, ","))))
    nr = length(delta_pi) * length(delta_nu)

    list(sample_size=sample_size, phi=phi, sigma2=sigma2, 
         delta_pi=delta_pi, delta_nu=delta_nu, alpha=input$alpha, 
         theta=input$theta, xi=xi, num_strata=input$num_strata)
  })

  get_power = reactive({ 
    params = get_inputs()
    df <- data.frame(list(
      sample_size = rep(params$sample_size, each=length(params$delta_nu)),
      delta_nu = rep(params$delta_nu, length(params$sample_size))))
    power <- rep(NA, nrow(df))
    for (i in 1:nrow(df)) {
      power[i] <- round(selection_power(df$sample_size[i], params$phi,
        params$sigma2, params$delta_pi, df$delta_nu[i], params$alpha,
        params$theta, params$xi, params$num_strata), digits=3)
    }
    df$power <- power
    names(df) = c("Sample Size", "Selection Effect", "Power")
    df
  })

  output$sample_size = renderDataTable({
    get_power()
  })

  output$line_graph = renderPlot({
    params = get_inputs()
    df = get_power()
    df[,1] <- factor(df[, 1])
    ggplot(df, aes(x=`Selection Effect`, y=Power, group=`Sample Size`,
      color=`Sample Size`)) + geom_line()
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

