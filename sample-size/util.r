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
create_strat_selection_report = function(doc_template, params, path_to_df) {
  doc_template %>% 
    str_replace_all("POWER", cond_to_string(params$power)) %>%
    str_replace_all("PREF_EFFECT", cond_to_string(params$pref_effect)) %>%
    str_replace_all("SELECTION_EFFECT", 
                    cond_to_string(params$selection_effect)) %>%
    str_replace_all("TREATMENT_EFFECT", 
                    cond_to_string(params$treatment_effect)) %>%
    str_replace_all("SIGMA2", cond_to_string(params$sigma2)) %>%
    str_replace_all("PREF_PROP", cond_to_string(params$pref_prop)) %>%
    str_replace_all("STRATUM_PROP", cond_to_string(params$stratum_prop)) %>%
    str_replace_all("ALPHA", cond_to_string(params$alpha)) %>%
    str_replace_all("PATH_TO_DF", cond_to_string(path_to_df))
}

