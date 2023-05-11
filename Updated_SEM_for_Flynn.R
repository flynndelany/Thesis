sem.df <- read.csv("D:/Projects/Blocks/Stats/Blokdt/SEM_Data.csv")

sem.data.adj <- sem.data %>% filter(Treatment != "CB") %>%
  mutate(Site = case_when(Site == "FP" ~ 0,
                          Site == "LL" ~ 1),
         Treatment = case_when(Treatment == "SG" ~ 0,
                               Treatment == "CB" ~ 1,
                               Treatment == "OY" ~ 2)) %>%
  
  # Introduce some transformations for model assumptions
  mutate(
    Epiphyte_mg_day_log = log10(Epiphyte_mg_day),
    drift_mg_day_log = log10(drift_mg_day)
  )

# Pulling out each model to run tests for assumptions
epiphyte_model <- lmer(Epiphyte_mg_day_log ~ Treatment +
                        # nesting survey with site as a random effect
                         (1|Site/Survey),
                       data = sem.data.adj)

# Check for homogeneity of variance
plot(epiphyte_model) # no wedging on log-transformed!
# Check for normality of residuals
hist(resid(epiphyte_model)) # meh, ok
# Check for collinearity
car::vif(epiphyte_model) # should be <= 2

drift_model <- lmer(drift_mg_day_log ~ Treatment + (1|Site/Survey), data = sem.data.adj)

plot(drift_model)
hist(resid(drift_model))

cover_model <- lmer(cover.div ~ Treatment + (1|Site/Survey), data = sem.data.adj)

plot(cover_model)
hist(resid(cover_model))

fauna_div_model <- lmer(fauna.div ~ cover.div + Treatment + (1|Site/Survey), data = sem.data.adj)

plot(fauna_div_model)
hist(resid(fauna_div_model))
car::vif(fauna_div_model)

fauna_biomass_model <- lmer(Grazer_mg_day ~ fauna.div + cover.div + Treatment + (1|Site/Survey), data = sem.data.adj)

plot(fauna_biomass_model)
hist(resid(fauna_biomass_model))
car::vif(fauna_biomass_model)

sg_model <- lmer(sg_survival ~ fauna.div + Epiphyte_mg_day_log + drift_mg_day_log + Treatment + avgHsat + (1|Site/Survey), data = sem.data.adj)

plot(sg_model)
hist(resid(sg_model))
car::vif(sg_model)


# Bring together in SEM
sem2 <- psem(epiphyte_model, drift_model, cover_model, fauna_div_model, fauna_biomass_model, sg_model,
             # Specify grazer-drift and grazer-epiphyte relationships as non-causal (double-headed arrows)
             Epiphyte_mg_day_log %~~% Grazer_mg_day,
             drift_mg_day_log %~~% Grazer_mg_day,
             data = sem.data.adj
             )

summary(sem2)
