
## Porosity - Get particle density  --------------------------------------------




## LOI -------------------------------------------------------------------------
LOI_I <- read.csv("D:/Projects/Blocks/Data/LOI_Initial.csv") %>%
  dplyr::select(Site, Block=Treatment, Rep, LOI_mg)
LOI_F <- read.csv("D:/Projects/Blocks/Data/LOI_Final.csv") %>%
  dplyr::select(Site, Block=Treatment, Rep, LOI_mg)

I <- LOI_I %>% group_by(Site, Block) %>% summarise(LOI_I = mean(LOI_mg))
Fin <- LOI_F %>% group_by(Site, Block) %>% summarise(LOI_F = mean(LOI_mg))

LOI <- full_join(I, Fin) %>%
  mutate(delta_LOI_mg = LOI_F - LOI_I, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                             startsWith(Block, "O") == T ~ "OY",
                                                             startsWith(Block, "C") == T ~ "CB"))


ggplot(LOI, aes(Treatment, delta_LOI_mg)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Site)

#Anova
lm.loi <- lm(data = LOI_I, LOI_mg ~ Treatment*Site)

plot(simulateResiduals(lm.loi)) #Passes - ish (calculate difference?)

anova(lm.loi)

#Post Hoc
em.loi <- emmeans(lm.loi, ~ Treatment * Site)

pairs(em.loi)

plot(em.loi, comparisons = T)

cld(em.loi)

## Grain Size (TBD) ------------------------------------------------------------