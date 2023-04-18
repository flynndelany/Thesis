
library(piecewiseSEM)
library(tidyverse)
library(lme4)

sem.df <- read.csv("SEM_Data.csv")

#Site inclusion into SEM ----

sem.data.adj <- sem.df %>% filter(Treatment != "CB") %>%
  mutate(Site = case_when(Site == "FP" ~ 0,
                          Site == "LL" ~ 1),
         Treatment = case_when(Treatment == "SG" ~ 0,
                               Treatment == "CB" ~ 1,
                               Treatment == "OY" ~ 2))

sem <- psem(
  lmer(cover.div ~ Treatment + Site + (1|Survey), data = sem.data.adj),
  lmer(drift_mg_day ~ Treatment + Site + (1|Survey), data = sem.data.adj),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = sem.data.adj),
  lmer(Grazer_mg_day ~ drift_mg_day + fauna.div + cover.div + Treatment + Site + (1|Survey), data = sem.data.adj), 
  lmer(Epiphyte_mg_day ~ Treatment + Grazer_mg_day + (1|Survey), data = sem.data.adj), 
  lmer(sg_survival ~ fauna.div + Epiphyte_mg_day + Site + Treatment +(1|Survey), data = sem.data.adj),
  data = sem.data.adj
)

summary(sem)

#Far Pond only SEM ----

fp.sem.data <- sem.df %>% filter(Site == "FP" & Treatment != "CB") %>%
  mutate(Treatment = case_when(Treatment == "SG" ~ 0,
                               Treatment == "OY" ~ 1))

fp.sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = fp.sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = fp.sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(Grazer_mg_day ~ Treatment + drift_mg_day + fauna.div + cover.div + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(Epiphyte_mg_day ~ drift_mg_day + Grazer_mg_day + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(sg_survival ~ Treatment + fauna.div + Epiphyte_mg_day + (1|Survey), data = fp.sem.data),
  data = fp.sem.data
)

summary(fp.sem)

#Landscape Lab only SEM ----

ll.sem.data <- sem.df %>% filter(Site == "LL" & Treatment != "CB") %>%
  mutate(Treatment = case_when(Treatment == "SG" ~ 0,
                               Treatment == "OY" ~ 1))

ll.sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = ll.sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = ll.sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = ll.sem.data),
  lmer(Grazer_mg_day ~ drift_mg_day + fauna.div + cover.div + Treatment + (1|Survey), data = ll.sem.data), 
  lmer(Epiphyte_mg_day ~ fauna.div + Grazer_mg_day + Treatment + (1|Survey), data = ll.sem.data), #Treatment + fauna added from null
  lmer(sg_survival ~ fauna.div + Epiphyte_mg_day + (1|Survey), data = ll.sem.data),
  data = ll.sem.data
)

summary(ll.sem)
