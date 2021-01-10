library(tidyverse)
library(ggplot2)
library(tibble)
library(emdbook)
library(brms)
library(rstan)

load("d_lohi_full.rda")


df <- d_lohi


df_summ <- df %>% mutate(iv2=case_when(task=="low"~log10(iv),
                                       TRUE~iv)) %>%
  group_by(task) %>%
  mutate(iv3=(iv2-mean(iv2))/sd(iv2),
         task=as.factor(task))


dfit <- df_summ %>% filter(task=="low") %>% select(tNo, context=cond, dv, subject, task, iv= iv3)

fit_bern <- brm(
  bf(dv ~ (1 - exp(-(iv/alpha)^beta)),
     alpha ~ 0 + context + (0 + context || subject),
     beta ~ 0 + context + (0 + context || subject),
     nl = TRUE),
  data = dfit, 
  family = bernoulli("identity"), 
  prior = c(
    prior(normal(0, 1), nlpar = "alpha"),
    prior(normal(0, 1), nlpar = "beta", lb = 0), 
    prior(normal(0, .1), nlpar = "alpha", class = "sd", group = "subject"),
    prior(normal(0, .1), nlpar = "beta", class = "sd", group = "subject") 
  ),
  inits = 0,
  cores = 4
)

save(fit_bern, file="bernfit_100121.rds",compress="xz")

