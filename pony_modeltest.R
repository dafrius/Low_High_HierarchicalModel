library(tidyverse)
library(ggplot2)
library(tibble)
library(emdbook)
library(brms)
library(rstan)
library(brms)
library(modelfree)
library(reshape)

load("d_lohi_full.rda")
load("hi_clean_full.rda")
load("lo_clean_full.rda")

df <- d_lohi

df_trns <- df %>% mutate(iv2=case_when(task=="low"~log10(iv),
                                       TRUE~iv)) %>%
  group_by(task) %>%
  mutate(iv3=(iv2-mean(iv2))/sd(iv2),
         task=as.factor(task))

df_summary <- df_trns %>% group_by(cond,subject,task,iv3) %>%
  summarise(z=sum(dv), n=length(dv), p=z/n)


df_1sub3cond <- df_summary %>% filter(subject=="LH01" | subject=="LH03",
                                      task=="upright")


fit_df1sub3cond1 <- brm(
  bf(z | trials(n) ~ Phi((iv3 - alpha) / beta), 
     alpha ~ 1 + cond + (1 + cond||subject),
     beta ~ 1 + cond + (1 + cond||subject),
     nl = TRUE),
  data = df_1sub3cond, 
  family = binomial("identity"), 
  prior = c(
    prior(normal(0, 1), nlpar = "alpha"),
    prior(lognormal(1, .3), nlpar = "beta", lb=0),
    prior(lognormal(.1, .3), nlpar = "alpha", class = "sd", group = "subject"),
    prior(lognormal(.1, .3), nlpar = "beta", class = "sd", group = "subject") 
  ),
  inits = 0,
  control = list(adapt_delta=.99)
)

save(fit_df1sub3cond1 ,file="fit_df1sub3cond1.rda", compress="xz")