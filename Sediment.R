
## Porosity - Get particle density  --------------------------------------------
Por_I <- read.csv("D:/Projects/Blocks/Data/Porosity_I.csv") %>%
  dplyr::select(Site, Treatment, Rep, Por_I = Porosity_percent) #%>% 
  group_by(Site, Treatment) %>%
  summarise(Por_I = mean(Porosity_percent))
Por_F <- read.csv("D:/Projects/Blocks/Data/Porosity_F.csv") %>%
  dplyr::select(Site, Treatment, Rep, Por_F = Porosity_percent) #%>% 
  group_by(Site, Treatment) %>%
  summarise(Por_F = mean(Porosity_percent))

Por <- full_join(Por_I, Por_F) %>%
  mutate(delta_Por = Por_F - Por_I, Treatment = case_when(startsWith(Treatment, "G") == T ~ "SG",
                                                          startsWith(Treatment, "O") == T ~ "OY",
                                                          startsWith(Treatment, "C") == T ~ "CB"))

ggplot(Por, aes(Treatment, delta_Por*100)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Site) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust = + 4),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 18)) +
  ylab(expression(paste("Delta Porosity (%)")))

ggplot(Por, aes(Treatment, Por_I*100)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Site) +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust = + 4),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 18)) +
  ylab(expression(paste("Porosity (%)"))) +
  ylim(25,40)

lm.pori <- lm(data = Por, Por_I ~ Treatment + Site)

plot(simulateResiduals(lm.pori)) #Passes

anova(lm.pori)

lm.por <- lm(data = Por, delta_Por ~ Treatment + Site)

plot(simulateResiduals(lm.por)) #Passes

anova(lm.por)
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
  facet_wrap(~Site) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust = + 4),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 18)) +
  ylab(expression(paste("Delta Organic Matter (mg)")))

ggplot(LOI, aes(Treatment, LOI_I)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Site) +
  ylim(0, 225) +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust = + 4),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 18)) +
  ylab(expression(paste("Sediment Organic Matter (mg)")))

#Anova
lm.loi <- lm(data = LOI, delta_LOI_mg ~ Treatment + Site)

plot(simulateResiduals(lm.loi)) #Passes - ish (calculate difference?)

anova(lm.loi)

lm.loii <- lm(data = LOI, LOI_I ~ Treatment + Site)

plot(simulateResiduals(lm.loii)) #Passes - ish (calculate difference?)

anova(lm.loii)

#Post Hoc
em.loi <- emmeans(lm.loi, ~ Treatment)

pairs(em.loi)

plot(em.loi, comparisons = T)

cld(em.loi)

## Grain Size (TBD) ------------------------------------------------------------