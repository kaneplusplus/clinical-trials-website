library(rbokeh)
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
    # Turn the text inputs into vectors of numeric values.
    phi = as.numeric(gsub(" ", "", unlist(strsplit(input$phi, ","))))
    sigma2 = as.numeric(gsub(" ", "", unlist(strsplit(input$sigma2, ","))))
    xi = as.numeric(gsub(" ", "", unlist(strsplit(input$xi, ","))))
    nr = length(delta_pi) * length(delta_nu)

    list(power=input$power, phi=phi, sigma2=sigma2, delta_pi=delta_pi,
         delta_nu=delta_nu, alpha=input$alpha, theta=input$theta,
         xi=xi, num_strata=input$num_strata)
  })

  get_strat_selection = reactive({ 
    params = get_inputs()
    ret = preference:::create_strat_selection_df(params)
    ret
  })

  output$sample_size = renderDataTable({
    x = get_strat_selection()
    names(x) = c("Preference Effect", "Selection Effect", "Sample Size")
    x
  })

  output$line_graph = renderPlot({
    params = get_inputs()
    df = get_strat_selection()
    ret = preference:::create_strat_selection_plot(params, df) 
    ret
  })

  output$downloadData = downloadHandler(
    filename = function() {
      paste("stratified-selection-report", "zip", sep=".")
    },
    content = function(fname) {
      doc_template = readLines("ssc.rmd")
      current_wd = getwd()
      tmpdir = tempdir()
      setwd(tempdir())
      params = get_inputs()
      df = preference:::create_strat_selection_df(params)
      df_path = "stratified-selection-sample-size.csv"
      write.csv(df, df_path)
      report_dest = "stratified-selection-sample-report.docx"
      rmd_content = create_strat_selection_report(doc_template, params, df_path)
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

