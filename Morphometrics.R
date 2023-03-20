library(tidyverse)

Morph <- read.csv("D:/Projects/Blocks/Data/Morphometrics.csv") %>%
  mutate(ID = paste(Site, Block, Patch))

## Survival (Need to merge with Zeros)
Survival <- Morph %>%
  group_by(Site, Block, Patch) %>%
  summarise(Shoots = max(Sht)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"), Survival = Shoots/30)

ggplot(Survival, aes(x = Treatment, y = Survival)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

#Productivity


#LAI


#Canopy Height


#Biomass (Still being Weighed)