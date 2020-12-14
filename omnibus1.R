library(tidyverse)
library(ggplot2)
library(tibble)
library(emdbook)
library(brms)
library(rstan)
library(brms)

load("dfall_ivdv.rda")


df <- dfall

df_summ <- df %>% group_by(subject, context, task,iv) %>%
  summarise(z=sum(1-dv), n=length(dv), p=z/n) %>%
  mutate(iv2=(iv-mean(iv))/sd(iv),
         task=as.factor(task))


fit_lowhigh <- brm(
  bf(z | trials(n) ~ Phi((iv2 - alpha) / beta), 
     alpha ~ 0 + context + (0 + context || subject/task), 
     beta ~ 0 + context + (0 + context || subject/task), 
     nl = TRUE),
  data = df_summ, family = binomial("identity"), 
  prior = c(
    prior(normal(0, 1), nlpar = "alpha"),
    prior(normal(0, 1), nlpar = "beta", lb = 0), 
    prior(normal(0, .1), nlpar = "alpha", class = "sd", group = "subject"),
    prior(normal(0, .1), nlpar = "beta", class = "sd", group = "subject") 
  ),
  inits = 0,
)
