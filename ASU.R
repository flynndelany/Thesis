

Survey <- c(1,2,3,4,5)
Days <- c(18,20,19,23,22)
Exposure <- data.frame(Survey, Days)

## Epifauna --------------------------------------------------------------------

#Wet Weights
read.csv("D:/Projects/Blocks/Data/Epifauna.csv") %>%
mutate(Wet_mg = Wet_g * 1000, Treatment = case_when(startsWith(Treatment, "G") == T ~ "SG",
                                                    startsWith(Treatment, "O") == T ~ "OY",
                                                    startsWith(Treatment, "C") == T ~ "CB")) %>%
ggplot(aes(x = Treatment, y = Wet_g, fill = Class)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(1)) +
  geom_point(position = position_dodge(1)) +
  facet_wrap(~Site) +
  scale_fill_discrete(labels = c("Amphipods", "Mollusks")) +
  theme_classic() +
  labs(fill = "Group")

#Just Amphipods (g/day)
Dry_Amph <- read.csv("D:/Projects/Blocks/Data/Epifauna.csv") %>%
  filter(Class == "Amp") %>%
  mutate(Dry_mg = Dry_g * 1000, Block = Treatment, Treatment = case_when(startsWith(Treatment, "G") == T ~ "SG",
                                                                         startsWith(Treatment, "O") == T ~ "OY",
                                                                         startsWith(Treatment, "C") == T ~ "CB")) %>%
  left_join(Exposure) %>%
  mutate(Dry_mg_day = Dry_mg/Days)



ggplot(Dry_Amph, aes(x = Treatment, y = Dry_mg_day)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(1)) +
  geom_point(position = position_dodge(1)) +
  facet_wrap(~Site) +
  theme_classic()

#Anova
lm.amph <- lm(data = Dry_Amph, Dry_g ~ Treatment + Site)

plot(simulateResiduals(lm.amph)) #Fails Levene Test

anova(lm.amph)

#Post Hoc
em.amph <- emmeans(lm.amph, ~ Treatment * Site)

pairs(em.amph)

plot(em.amph, comparisons = T)

cld(em.amph)

## Epiphytes (g/day) -------------------------------------------------------------------

epiphytes.survey <- read.csv("D:/Projects/Blocks/Data/EpiphytesASU.csv") %>%
  mutate(Dry_mg = Dry_g * 1000, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                      startsWith(Block, "O") == T ~ "OY",
                                                      startsWith(Block, "C") == T ~ "CB")) %>%
  left_join(Exposure) %>%
  mutate(Dry_mg_day = Dry_mg/Days)

ggplot(epiphytes.survey, aes(x = Treatment, y = Dry_mg_day)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

## Macroalgae (g/day) ------------------------------------------------------------------

drift <- read.csv("D:/Projects/Blocks/Data/Macroalgae.csv") %>%
  mutate(Dry_mg = Dry_g * 1000, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                      startsWith(Block, "O") == T ~ "OY",
                                                      startsWith(Block, "C") == T ~ "CB")) %>%
  left_join(Exposure) %>%
  mutate(Dry_mg_day = Dry_mg/Days)

ggplot(data = drift, aes(x = Treatment, y = Dry_mg_day)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

## Test Plots For Brad --------

zz <- epiphytes.survey %>%
  dplyr::select(Survey, Site, Block, Treatment, Rep, Epiphytes = Dry_mg) %>%
  mutate(Epiphytes = Epiphytes/14)

xx <- drift %>%
  dplyr::select(Survey, Site, Block, Treatment, Rep, DriftAlgae = Dry_mg)

yy <- Dry_Amph %>%
  dplyr::select(Survey, Site, Block, Treatment, Rep, Epifauna = Dry_mg)

ww <- survey.survival %>%
  dplyr::select(Survey, Site, Block, Treatment, SG = TotalSurvival)
  
zzz <- left_join(xx,yy)

yyy <- left_join(zz,yy)

www <- left_join(ww, yy)

ggplot(zzz, aes(Epifauna, DriftAlgae)) +
  geom_point() +
  labs(x = "Total Epifauna (mg)", y = "Total Drift Algae (mg)") +
  theme_classic()

summary(lm(Epifauna ~ DriftAlgae, data = zzz))

ggplot(yyy, aes(Epifauna, Epiphytes, color = Treatment)) +
  geom_point() +
  labs(x = "Total Epifauna (mg)", y = expression(paste("Total Epiphytes (mg "," cm"^-2," )"))) +
  theme_classic()

summary(lm(Epifauna ~ Epiphytes, data = yyy))

ggplot(www, aes(SG, Epifauna, color = Treatment)) +
  geom_point() +
  labs(x = "Surrounding SG", y = "Total Epifauna (mg)") +
  theme_classic()

summary(lm(Epifauna ~ SG, data = www))

#Cumulative plots

xx %>%
  group_by(Survey, Site, Treatment) %>%
  summarise(Drift_mean = mean(DriftAlgae)) %>% 
  group_by(Site, Treatment) %>%
  mutate(Drift_cum = cumsum(Drift_mean)) %>%
  ggplot(aes(Survey, Drift_cum)) +
  geom_line() +
  facet_grid(Site ~ Treatment) +
  theme_classic() +
  ylab("Cumulative Drift Algae (mg)")

zz %>%
  group_by(Survey, Site, Treatment) %>%
  summarise(phyte_mean = mean(Epiphytes)) %>% 
  group_by(Site, Treatment) %>%
  mutate(phyte_cum = cumsum(phyte_mean)) %>%
  ggplot(aes(Survey, phyte_cum)) +
  geom_line() +
  geom_hline(data = epitotal, aes(yintercept = Epiphyte), linetype = "dashed") +
  facet_grid(Site ~ Treatment) +
  theme_classic() +
  ylab(expression(paste("Cumulative ASU Epiphytes (mg", "  " ," cm"^-2, ")")))
