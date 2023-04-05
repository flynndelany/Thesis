

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
