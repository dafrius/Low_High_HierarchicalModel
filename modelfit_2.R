library(tidyverse)
library(ggplot2)
library(tibble)
library(emdbook)
library(brms)
library(rstan)
library(brms)

load("df_lowhigh.rda")


df <- df_lowhigh

# Log transforming low-level IV
df_lowtf <- df %>% filter(task=="low") %>% mutate(iv=log10(iv))

df_summ <- df %>% mutate(iv2=case_when(task=="low"~log10(iv),
                                       TRUE~iv)) %>%
  group_by(task) %>%
  mutate(iv3=(iv2-mean(iv2))/sd(iv2),
         task=as.factor(task),
         guess_needed=case_when(task=="low"~1,
                                TRUE~0))

df_summ2 <- df_summ %>% group_by(cond,subject,task,iv3) %>%
  summarise(z=sum(1-dv), n=length(dv), p=z/n) %>%
  mutate(guess_needed=case_when(task=="low"~1,
                                TRUE~0)) %>%
  rename(context=cond)
#dflow <- df_summ2 %>% filter(task=="low")
#dfhi <- df_summ2 %>% filter(task!="low")
#(guess*guess_needed) + ((1-guess*guess_needed) * Phi((ind.var. - alpha) / beta)

fit_lowhigh <- brm(
  bf(z | trials(n) ~ (.5*guess_needed) + ((1-.5*guess_needed) * Phi((iv3 - alpha) / beta)), 
     alpha ~ 0 + context:task + (0 + context:task | subject), 
     beta ~ 0 + context:task + (0 + context:task | subject), 
     nl = TRUE),
  data = df_summ2, family = binomial("identity"), 
  prior = c(
    prior(normal(0, 1), nlpar = "alpha"),
    prior(normal(0, 1), nlpar = "beta", lb = 0), 
    prior(normal(0, .1), nlpar = "alpha", class = "sd", group = "subject"),
    prior(normal(0, .1), nlpar = "beta", class = "sd", group = "subject") 
  ),
  inits = 0,
  chains = 12
)

save(fit_lowhigh, file="finalfit_251220.rda",compress="xz")

