library(rbokeh)
library(preference)
library(ggplot2)
library(foreach)
library(tidyverse)
source("util.r")
#library(trelliscopejs)

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
    mu1 <-  parse_sequence_text(input$mu1)
    mu2 <- parse_sequence_text(input$mu2)
    mu11 <- parse_sequence_text(input$mu11)
    mu22 <- parse_sequence_text(input$mu22)
    phi <- parse_sequence_text(input$phi)
    nstrata <- input$nstrata
    list(mu1=mu1, mu2=mu2, mu11=mu11, mu22=mu22, phi=phi, nstrata=nstrata)
  })

  get_effects = reactive({ 
    
    params = get_inputs()
    ret = foreach(m1=params$mu1, .combine=rbind) %:% 
      foreach(m2=params$mu2, .combine=rbind) %:%
      foreach(m11=params$mu11, .combine=rbind) %:% 
      foreach(m22=params$mu22, .combine=rbind) %:% 
      foreach(p=params$phi, .combine=rbind) %do% {
        effects = effects_from_means(m1, m2, m11, m22, p, params$nstrata)
        c(m1, m2, m11, m22, p, round(effects$treatment, 2), 
          round(effects$selection, 2), round(effects$preference, 2), 
          params$nstrata)
      }
    ret = as.data.frame(ret)
    colnames(ret) = c("mu1", "mu2", "mu11", "mu22", "phi", "treatment",
      "selection", "preference", "nstrata")
    ret
  })

  output$effect_size <-  renderDataTable({
    get_effects()
  })

  output$downloadData <- downloadHandler(
    filename=function() {
      paste("effect-size-report", "zip", sep=".")
    },
    content=function(fname) {
      doc_template = readLines("effect-size-template.rmd")
      current_wd = getwd()
      tmpdir = tempdir() 
      setwd(tempdir())
      params = get_inputs()
      table_dest <- "effect-sizes.csv"
      report_dest <- "effect-size-report.docx"
      rmd_content <- create_effect_size_report(doc_template, params)
      tf <- paste0(tempfile(), ".rmd")
      writeLines(rmd_content, tf)
      # Create the document from Rmarkdown 
      render(tf, output_file=report_dest)

      # Create the effect size table and dump it to csv.
      effect_size_table <- foreach(m1=mu1, .combine=rbind) %:%
        foreach(m2=mu2, .combine=rbind) %:%
        foreach(m11=mu11, .combine=rbind) %:%
        foreach(m22=mu22, .combine=rbind) %:%
        foreach(p=phi, .combine=rbind) %do% {
          effects <- effects_from_means(m1, m2, m11, m22, p)
          c(m1, m2, m11, m22, p, round(effects$treatment, 2),
            round(effects$selection, 2), round(effects$preference, 2),
            nstrata)
        }
      effect_size_table <- as.data.frame(effect_size_table)
      colnames(effect_size_table) <- c("mu1", "mu2", "mu11", "mu22", "phi",
        "treatment", "selection", "preference", "nstrata")
      write.csv(effect_size_table, table_dest, row.names=FALSE)

      ret = zip(zipfile=fname, files=c(report_dest, table_dest))
      setwd(current_wd)
      ret
    },
    contentType="application/zip"
  )

#  output$effect_viz = renderTrelliscope({
#    my_viz = function(xs, symbol, removes) {
#      xs %>% gather(Effect, Value, delta_tau:delta_pi) %>% 
#        ggplot(aes_string(x=symbol, y="Value")) + 
#          geom_line() + facet_grid(Effect ~ .)
#
#    }
#    vary_param = get_inputs()$vary_param
#    x = get_effects()
#    
#    group_by_names = colnames(x)[(1:5)[-which(colnames(x) == vary_param)]]
#    group_by_symbols = lapply(group_by_names, as.symbol)
#    xn = x %>% group_by_(.dots=group_by_symbols) %>% nest %>% 
#      mutate(effect_viz= map(data, 
#        function(x) my_viz(x, vary_param, group_by_names))) %>%
#      trelliscope(name="Effect Sizes", nrow=1, ncol=2, panel_col="effect_viz")
#  })

})

