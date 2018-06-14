library(preference)
library(foreach)
library(tidyverse)

mu1 = 1:10
mu2 = 1:10
mu11 = 2
mu22 = 2
phi = seq(0.4, 0.6, 0.1)
vary_param = "mu1"

x = foreach(m1=mu1, .combine=rbind) %:%
  foreach(m2=mu2, .combine=rbind) %:%
  foreach(m11=mu11, .combine=rbind) %:%
  foreach(m22=mu22, .combine=rbind) %:%
  foreach(p=phi, .combine=rbind) %do% {
    effects = calc_effects(m1, m2, m11, m22, p)
    c(m1, m2, m11, m22, p, effects$delta_tau, effects$delta_nu,
    effects$delta_pi)
  }
x = as.data.frame(x)
colnames(x) = c("mu1", "mu2", "mu11", "mu22", "phi", "delta_tau",
  "delta_nu", "delta_pi")

my_viz = function(xs, symbol, removes) {
  xs %>% gather(Effect, Value, delta_tau:delta_pi) %>%
    ggplot(aes_string(x=symbol, y="Value")) +
      geom_line() + facet_grid(Effect ~ .)

}

group_by_names = colnames(x)[(1:5)[-which(colnames(x) == vary_param)]]
group_by_symbols = lapply(group_by_names, as.symbol)
xn = x %>% group_by_(.dots=group_by_symbols) %>% nest %>%
  mutate(effect_viz= map(data,
    function(x) my_viz(x, vary_param, group_by_names))) %>%
  trelliscope(name="Effect Sizes", nrow=1, ncol=2, panel_col="effect_viz")

xn
