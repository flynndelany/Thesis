
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

Por$Treatment <- factor(Por$Treatment, c("SG", "CB", "OY"))

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
  ylab(expression(paste("Change in Porosity (%)")))

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
  ylab(expression(paste("Initial Porosity (%)"))) +
  ylim(25,40)

lm.pori <- lm(data = Por, Por_I ~ Treatment + Site)

plot(simulateResiduals(lm.pori)) #Passes

anova(lm.pori)

lm.por <- lm(data = Por, delta_Por ~ Treatment + Site)

plot(simulateResiduals(lm.por)) #Passes

anova(lm.por)
## LOI -------------------------------------------------------------------------
LOI_I <- read.csv("D:/Projects/Blocks/Data/LOI_Initial.csv") %>%
  dplyr::select(Site, Block, Rep, OM_perc)
LOI_F <- read.csv("D:/Projects/Blocks/Data/LOI_Final.csv") %>%
  dplyr::select(Site, Block, Rep, OM_perc)

I <- LOI_I %>% group_by(Site, Block) %>% summarise(LOI_I = mean(OM_perc))
Fin <- LOI_F %>% group_by(Site, Block) %>% summarise(LOI_F = mean(OM_perc))

LOI <- full_join(I, Fin) %>%
  mutate(delta_OM = LOI_F - LOI_I, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                             startsWith(Block, "O") == T ~ "OY",
                                                             startsWith(Block, "C") == T ~ "CB"))
LOI$Treatment <- factor(LOI$Treatment, c("SG", "CB", "OY"))

ggplot(LOI, aes(Treatment, delta_OM*100)) +
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
  ylab(expression(paste("Change in Organic Matter (%)")))

ggplot(LOI, aes(Treatment, LOI_I*100)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Site) +
  ylim(0,0.8) +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust = + 4),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 18)) +
  ylab(expression(paste("Initial Organic Matter (%)")))


#Anova
lm.loi <- lm(data = LOI, delta_OM ~ Treatment + Site)

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