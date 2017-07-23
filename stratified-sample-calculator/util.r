library(stringr)
library(preference)
library(rmarkdown)

cond_to_string = function(x) {
  if (length(x) > 1) {
    paste("c(", toString(x), ")")
  } else {
    toString(x)
  }
}
create_strat_selection_report = function(doc_template, params, df_path) {
  doc = doc_template %>% 
    str_replace_all("POWER", cond_to_string(params$power)) %>%
    str_replace_all("ALPHA", cond_to_string(params$alpha)) %>%
    str_replace_all("THETA", cond_to_string(params$theta)) %>%
    str_replace_all("DELTA_PI", cond_to_string(params$delta_pi)) %>%
    str_replace_all("DELTA_NU", cond_to_string(params$delta_nu)) %>%
    str_replace_all("PHI", cond_to_string(params$phi)) %>%
    str_replace_all("SIGMA2", cond_to_string(params$sigma2)) %>%
    str_replace_all("XI", cond_to_string(params$xi)) %>%
    str_replace_all("NSTRATA", cond_to_string(params$num_strata)) %>%
    str_replace_all("PATH_TO_DF", paste0('"', df_path, '"'))
  doc
}

