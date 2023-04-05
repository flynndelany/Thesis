
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

sem.data <- left_join(sem.data, sem.survival)

sem.data <- na.omit(sem.data)

#SEM ---------------------------------------------------------------------------

sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = sem.data),
  lmer(Grazer_mg_day ~ drift_mg_day + fauna.div + cover.div + Treatment + (1|Survey), data = sem.data), 
  lmer(Epiphyte_mg_day ~ drift_mg_day + Grazer_mg_day + Treatment + (1|Survey), data = sem.data), 
  lmer(sg_survival ~ drift_mg_day + fauna.div + Epiphyte_mg_day + Treatment + (1|Survey), data = sem.data),
  data = sem.data
)
summary(sem)

fp.sem.data <- sem.data %>% filter(Site == "FP")

fp.sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = fp.sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = fp.sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(Grazer_mg_day ~ drift_mg_day + fauna.div + cover.div + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(Epiphyte_mg_day ~ Grazer_mg_day + (1|Survey), data = fp.sem.data), ## good qq poor res vs pred
  lmer(sg_survival ~ drift_mg_day + fauna.div + Epiphyte_mg_day + Treatment + (1|Survey), data = fp.sem.data),
  data = fp.sem.data
)
summary(fp.sem)

ll.sem.data <- sem.data %>% filter(Site == "LL")

ll.sem <- psem(
  lmer(cover.div ~ Treatment + (1|Survey), data = ll.sem.data),
  lmer(drift_mg_day ~ Treatment + (1|Survey), data = ll.sem.data),
  lmer(fauna.div ~ cover.div + drift_mg_day + Treatment + (1|Survey), data = ll.sem.data),
  lmer(Grazer_mg_day ~ drift_mg_day + fauna.div + cover.div + (1|Survey), data = ll.sem.data), 
  lmer(Epiphyte_mg_day ~ Grazer_mg_day + (1|Survey), data = ll.sem.data), 
  lmer(sg_survival ~ drift_mg_day + fauna.div + Epiphyte_mg_day + Treatment + (1|Survey), data = ll.sem.data),
  data = ll.sem.data
)
summary(ll.sem)

# Dharma Test Plots

x <- lmer(cover.div ~ Treatment + (1|Survey), data = fp.sem.data)

plot(simulateResiduals(x))
