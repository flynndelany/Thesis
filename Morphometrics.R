library(tidyverse)

Morph <- read.csv("D:/Projects/Blocks/Data/Morphometrics.csv") %>%
  mutate(ID = paste(Site, Block, Patch))

Trans <- read.csv("D:/Projects/Blocks/Data/Transplantation.csv")
Trans <- Trans[-c(97:100),] %>%
  select(ID)

## Survival (Need to merge with Zeros)
Survival <- Morph %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = max(Sht)) %>%
  full_join(Trans) %>%
  mutate(Treatment = case_when(startsWith(substr(ID,4,6), "G") == T ~ "SG",
                               startsWith(substr(ID,4,6), "O") == T ~ "OY",
                               startsWith(substr(ID,4,6), "C") == T ~ "CB"), 
         Survival = case_when(is.na(Shoots) == F ~ Shoots/30,
                              is.na(Shoots) == T ~ 0), Site = substr(ID, 1, 2))

ggplot(Survival, aes(x = Treatment, y = Survival)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()
 
#Productivity (gram or area?) (deployment time?)
Productivity <- Morph %>%
  filter(New_mm != is.na(New_mm)) %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Growth = sum(New_mm)/sum(Total_mm)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(Productivity, aes(x = Treatment, y = Growth)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

#Secondary Shoots


#LAI
LAI <- Morph %>%
  mutate(LAI = Total_mm * Width_mm, NewLAI = New_mm * Width_mm) %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(LAI = sum(LAI), NewLAI = sum(NewLAI)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(LAI, aes(x = Treatment, y = LAI)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

#Canopy Height
Height <- Morph %>%
  filter()

#Biomass (Still being Weighed)



