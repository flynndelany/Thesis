
## Epifauna --------------------------------------------------------------------

#Wet Weights
read.csv("D:/Projects/Blocks/Data/Epifauna_Wet.csv") %>%
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

#Just Amphipods
Wet_Amph <- read.csv("D:/Projects/Blocks/Data/Epifauna_Wet.csv") %>%
  filter(Class == "Amp") %>%
  mutate(Survey = as.ordered(Survey), Wet_mg = Wet_g * 1000, Treatment = case_when(startsWith(Treatment, "G") == T ~ "SG",
                                                      startsWith(Treatment, "O") == T ~ "OY",
                                                      startsWith(Treatment, "C") == T ~ "CB"))

ggplot(Wet_Amph, aes(x = Treatment, y = Wet_g)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(1)) +
  geom_point(position = position_dodge(1)) +
  facet_wrap(~Site) +
  theme_classic()

#Anova
lm.amph <- lm(data = Wet_Amph, Wet_g ~ Treatment + Site)

plot(simulateResiduals(lm.amph)) #Passes

anova(lm.amph)

#Post Hoc
em.amph <- emmeans(lm.amph, ~ Treatment * Site)

pairs(em.amph)

plot(em.amph, comparisons = T)

cld(em.amph)

## Epiphytes -------------------------------------------------------------------

read.csv("D:/Projects/Blocks/Data/EpiphytesASU.csv") %>%
  mutate(Survey = as.ordered(Survey), Dry_mg = Dry_g * 1000, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                                                  startsWith(Block, "O") == T ~ "OY",
                                                                                  startsWith(Block, "C") == T ~ "CB")) %>%
  ggplot(aes(x = Treatment, y = Dry_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

## Macroalgae ------------------------------------------------------------------

read.csv("D:/Projects/Blocks/Data/Macroalgae.csv") %>%
  mutate(Survey = as.ordered(Survey), Dry_mg = Dry_g * 1000, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                                                   startsWith(Block, "O") == T ~ "OY",
                                                                                   startsWith(Block, "C") == T ~ "CB")) %>%
  ggplot(aes(x = Treatment, y = Dry_g, fill = Survey)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()
