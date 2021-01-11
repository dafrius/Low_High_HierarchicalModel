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


dfit <- df_summ %>% filter(task=="upright") %>% select(tNo, context=cond, dv, subject, task, iv= iv3)
dfit <- dfit %>% group_by(context,subject,iv) %>%
  summarise(z=sum(1-dv), n=length(dv), p=z/n) 

dup <- d_lohi %>% filter(task=="upright")
dlow <- d_lohi %>% filter(task=="upright")

fit_up <- brm(
  bf(dv ~ (1 - lambda) * ( 1 - exp(-(iv/alpha)^beta) ),
     alpha ~ 1 + cond,
     beta ~ 1 + cond,
     lambda ~ 1 + cond,
     nl = TRUE),
  data = dup, 
  family = bernoulli("identity"), 
  prior = c(
    prior(beta(1, 1), nlpar = "lambda", lb = 0, ub = .1),
    prior(normal(4, 10), nlpar = "alpha", lb = 0, ub = 7),
    prior(normal(2, 10), nlpar = "beta")
  ),
  warmup=1000,
  iter=4000,
  inits = 0,
  cores = parallel::detectCores(),
  chains=4,
  control=list(adapt_delta=.99),
  sample_prior="yes"
)


save(fit_up, file="bernfitup_110121.rds",compress="xz")


fit_low <- brm(
  bf(dv ~ 0.5 + (1 - 0.5 - lambda) * ( 1 - exp(-(iv/alpha)^beta) ),
     alpha ~ 1 + cond,
     beta ~ 1 + cond,
     lambda ~ 1 + cond,
     nl = TRUE),
  data = dlow, 
  family = bernoulli("identity"), 
  prior = c(
    prior(beta(1, 1), nlpar = "lambda", lb = 0, ub = .1),
    prior(normal(4, 10), nlpar = "alpha", lb = 0, ub = 7),
    prior(normal(2, 10), nlpar = "beta")
  ),
  warmup=1000,
  iter=4000,
  inits = 0,
  cores = parallel::detectCores(),
  chains=4,
  control=list(adapt_delta=.99),
  sample_prior="yes"
)


save(fit_low, file="bernfitlow_110121.rds",compress="xz")


fit_both <- brm(
  bf(dv ~ (1 - lambda) * ( 1 - exp(-(iv/alpha)^beta) ),
     alpha ~ 1 + cond,
     beta ~ 1 + cond,
     lambda ~ 1 + cond,
     nl = TRUE),
  data = d_lohi, 
  family = bernoulli("identity"), 
  prior = c(
    prior(beta(1, 1), nlpar = "lambda", lb = 0, ub = .1),
    prior(normal(4, 10), nlpar = "alpha", lb = 0, ub = 7),
    prior(normal(2, 10), nlpar = "beta")
  ),
  warmup=1000,
  iter=4000,
  inits = 0,
  cores = parallel::detectCores(),
  chains=4,
  control=list(adapt_delta=.99),
  sample_prior="yes"
)


save(fit_both, file="bernfitlow_110121.rds",compress="xz")

d_z <- d_lohi %>% mutate(iv=case_when(task=="low" ~ log10(iv),
                                      TRUE ~ iv)) %>% group_by(task) %>%
  mutate(iv=scale(iv)) %>%
  mutate(iv=as.numeric(iv)) %>%
  mutate(iv=iv+3)



fit_both <- brm(
  bf(dv ~ (1 - lambda) * ( 1 - exp(-(iv/alpha)^beta) ),
     alpha ~ 1 + cond,
     beta ~ 1 + cond,
     lambda ~ 1 + cond,
     nl = TRUE),
  data = d_z, 
  family = bernoulli("identity"), 
  prior = c(
    prior(beta(1, 1), nlpar = "lambda", lb = 0, ub = .1),
    prior(normal(4, 10), nlpar = "alpha", lb = 0, ub = 7),
    prior(normal(2, 10), nlpar = "beta")
  ),
  warmup=1000,
  iter=4000,
  inits = 0,
  cores = parallel::detectCores(),
  chains=4,
  control=list(adapt_delta=.99),
  sample_prior="yes"
)


save(fit_both_z, file="bernfitlowz3_110121.rds",compress="xz")


