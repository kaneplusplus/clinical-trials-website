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

create_effect_size_report <- function(doc_template, params) {
  doc_template  %>% 
    str_replace_all("MU11", cond_to_string(params$mu11)) %>%
    str_replace_all("MU22", cond_to_string(params$mu22)) %>%
    str_replace_all("MU1", cond_to_string(params$mu1)) %>%
    str_replace_all("MU2", cond_to_string(params$mu2)) %>%
    str_replace_all("PHI", cond_to_string(params$phi)) %>%
    str_replace_all("NSTRATA", cond_to_string(params$nstrat)) 
}
