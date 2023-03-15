library(tidyverse)

## Wet Weight

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

## Epiphytes

read.csv("D:/Projects/Blocks/Data/Epiphytes.csv") %>%
  mutate(Survey = as.ordered(Survey) ,Dry_mg = Dry_g * 1000, Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                                                                                  startsWith(Block, "O") == T ~ "OY",
                                                                                  startsWith(Block, "C") == T ~ "CB")) %>%
  ggplot(aes(x = Treatment, y = Dry_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()
   