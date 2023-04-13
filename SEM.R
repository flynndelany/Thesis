
# Gather Data--------------------------------------------------------------------

sem.data <- data.frame()

## Survey & Site & Block & Treatment

sem.data <- Cover %>% dplyr::select(Survey, Site, Block, Treatment) %>%
  mutate(Survey)

## Cover & Fauna Diversity

sem.data$cover.div <- LCBD.cover$LCBD

sem.data$fauna.div <- LCBD.fauna$LCBD

## Epifauna & Epiphyte & Drift Algae

grazers <- Dry_Amph %>%
  dplyr::select(Survey, Site, Block, Treatment, Dry_mg_day) %>%
  group_by(Survey, Site, Block) %>%
  summarise(Grazer_mg_day = mean(Dry_mg_day))

sem.data <- full_join(sem.data, grazers)

sem.phytes <- epiphytes.survey %>%
  dplyr::select(Survey, Site, Block, Treatment, Dry_mg_day) %>%
  group_by(Survey, Site, Block) %>%
  summarise(Epiphyte_mg_day = mean(Dry_mg_day))

sem.data <- full_join(sem.data, sem.phytes)

sem.drift <- drift %>%
  dplyr::select(Survey, Site, Block, Treatment, Dry_mg_day) %>%
  group_by(Survey, Site, Block) %>%
  summarise(drift_mg_day = mean(Dry_mg_day))

sem.data <- full_join(sem.data, sem.drift)

## Seagrass Survival

sem.survival <- survey.survival %>%
  dplyr::select(-A, -B, -C, -D, avg.survival) %>%
  mutate(sg_survival = avg.survival/30)

x <- survey.survival %>%
  filter(Site == "FP" & Treatment == "OY") %>%
  group_by(Survey) %>% 
  summarise(avg.survival = mean(avg.survival)) %>%
  mutate(lag.surv = lag(avg.survival, n = 1, order_by = Survey, default = 30), Site = "FP", Treatment = "OY")

y <- survey.survival %>%
  filter(Site == "FP" & Treatment == "SG") %>%
  group_by(Survey) %>% 
  summarise(avg.survival = mean(avg.survival)) %>%
  mutate(lag.surv = lag(avg.survival, n = 1, order_by = Survey, default = 30), Site = "FP", Treatment = "SG")

z <- survey.survival %>%
  filter(Site == "FP" & Treatment == "CB") %>%
  group_by(Survey) %>% 
  summarise(avg.survival = mean(avg.survival)) %>%
  mutate(lag.surv = lag(avg.survival, n = 1, order_by = Survey, default = 30), Site = "FP", Treatment = "CB")

fp.ss <- rbind(x,y,z)

x <- survey.survival %>%
  filter(Site == "LL" & Treatment == "OY") %>%
  group_by(Survey) %>% 
  summarise(avg.survival = mean(avg.survival)) %>%
  mutate(lag.surv = lag(avg.survival, n = 1, order_by = Survey, default = 30), Site = "LL", Treatment = "OY")

y <- survey.survival %>%
  filter(Site == "LL" & Treatment == "SG") %>%
  group_by(Survey) %>% 
  summarise(avg.survival = mean(avg.survival)) %>%
  mutate(lag.surv = lag(avg.survival, n = 1, order_by = Survey, default = 30), Site = "LL", Treatment = "SG")

z <- survey.survival %>%
  filter(Site == "LL" & Treatment == "CB") %>%
  group_by(Survey) %>% 
  summarise(avg.survival = mean(avg.survival)) %>%
  mutate(lag.surv = lag(avg.survival, n = 1, order_by = Survey, default = 30), Site = "LL", Treatment = "CB")

ll.ss <- rbind(x,y,z)

ss <- rbind(fp.ss, ll.ss) %>%
  dplyr::select(Survey, Site, Treatment, lag.surv)

sem.survival <- left_join(sem.survival, ss) %>%
  mutate(delta_survival = (avg.survival - lag.surv)/lag.surv)

sem.data <- left_join(sem.data, sem.survival)

sem.data <- na.omit(sem.data)

#SEM ---------------------------------------------------------------------------

sem.data.adj <- sem.data %>% filter(Treatment != "CB") %>%
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
  lmer(Epiphyte_mg_day ~ Grazer_mg_day + Treatment + (1|Survey), data = sem.data.adj), 
  lmer(delta_survival ~ fauna.div + Epiphyte_mg_day + Treatment + (1|Survey), data = sem.data.adj),
  data = sem.data.adj
)

summary(sem)

fp.sem.data <- sem.data %>% filter(Site == "FP" & Treatment != "CB") %>%
  mutate(Treatment = case_when(Treatment == "SG" ~ 0,
                               Treatment == "OY" ~ 1))

fp.sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = fp.sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = fp.sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(Grazer_mg_day ~ Treatment + drift_mg_day + fauna.div + cover.div + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(Epiphyte_mg_day ~ drift_mg_day + Grazer_mg_day + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(delta_survival ~ Treatment + fauna.div + Epiphyte_mg_day + (1|Survey), data = fp.sem.data),
  data = fp.sem.data
)

summary(fp.sem)

ll.sem.data <- sem.data %>% filter(Site == "LL" & Treatment != "CB") %>%
  mutate(Treatment = case_when(Treatment == "SG" ~ 0,
                               Treatment == "OY" ~ 1))

ll.sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = ll.sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = ll.sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = ll.sem.data),
  lmer(Grazer_mg_day ~ drift_mg_day + fauna.div + cover.div + Treatment + (1|Survey), data = ll.sem.data), 
  lmer(Epiphyte_mg_day ~ fauna.div + Grazer_mg_day + Treatment + (1|Survey), data = ll.sem.data), #Treatment + fauna added from null
  lmer(delta_survival ~ fauna.div + Epiphyte_mg_day + (1|Survey), data = ll.sem.data),
  data = ll.sem.data
)
summary(ll.sem)

# Dharma Test Plots

x <- lmer(cover.div ~ Treatment + (1|Survey), data = fp.sem.data)

plot(simulateResiduals(x))
