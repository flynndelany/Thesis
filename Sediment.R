
## Porosity (Unit?) ------------------------------------------------------------




## LOI -------------------------------------------------------------------------
LOI_I <- read.csv("D:/Projects/Blocks/Data/LOI_Initial.csv") %>%
  dplyr::select(Site, Block=Treatment, Rep, LOI_mg) %>%
  mutate(Trial = "Initial")
LOI_F <- read.csv("D:/Projects/Blocks/Data/LOI_Final.csv") %>%
  dplyr::select(Site, Block=Treatment, Rep, LOI_mg) %>%
  mutate(Trial = "Final")

LOI <- rbind(LOI_I, LOI_F) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))


ggplot(LOI, aes(Treatment, LOI_mg, fill = Trial)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Site)

#Anova
lm.loi <- lm(data = LOI, LOI_mg ~ Treatment*Site*Trial)

plot(simulateResiduals(lm.loi)) #Passes - ish (calculate difference?)

anova(lm.loi)

#Post Hoc
em.loi <- emmeans(lm.loi, ~ Treatment * Site * Trial)

pairs(em.loi)

plot(em.loi, comparisons = T)

cld(em.loi)

## Grain Size (TBD) ------------------------------------------------------------