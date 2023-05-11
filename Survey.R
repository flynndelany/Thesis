
## Faunal Diversity (added mud crab to 1 & 12 for test & Misc of all 1s) ------------------------
Fauna <- read.csv("D:/Projects/Blocks/Data/SurveyFauna.csv") %>%
  mutate(Treatment = case_when(startsWith(substr(ID,4,6), "G") == T ~ "SG",
                               startsWith(substr(ID,4,6), "O") == T ~ "OY",
                               startsWith(substr(ID,4,6), "C") == T ~ "CB"))

FaunaSum <- Fauna %>%
  group_by(Site, Treatment) %>%
  summarise(across(RockCrab:RockyBlenny, mean)) %>%
  pivot_longer(RockCrab:RockyBlenny, names_to = "Species", values_to = "CountMean")

FaunaSum$Treatment <- factor(FaunaSum$Treatment, c("SG", "CB", "OY"))

faunaSG <- FaunaSum %>% filter(Treatment == "SG") %>%
  ggplot(aes(x = reorder(Species, CountMean), CountMean, fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Species", y = "Count") +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Seagrass Species") +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80"))

faunaCB <- FaunaSum %>% filter(Treatment == "CB") %>%
  ggplot(aes(x = reorder(Species, CountMean), CountMean, fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Species", y = "Count") +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Control Block Species") +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80"))

faunaOY <- FaunaSum %>% filter(Treatment == "OY") %>%
ggplot(aes(x = reorder(Species, CountMean), CountMean, fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Species", y = "Count") +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Oyster Reef Species") +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80"))

ggarrange(faunaSG, faunaCB, faunaOY,
          labels=c("A", "B", "C"),
          ncol = 1, nrow = 3)

#Diversity
FaunaSimp <- read.csv("D:/Projects/Blocks/Data/Fauna_SimpsonD.csv") %>%
  mutate(SimpD = 1-SimpD) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))


prep.fauna <- Fauna[,8:ncol(Fauna)-1]
mtrx.fauna <- as.matrix(prep.fauna)

dist.fauna <- vegdist(mtrx.fauna)

LCBD.fauna <- LCBD.comp(dist.fauna, sqrt.D = T)

set.seed(666)
nmds.fauna <- metaMDS(mtrx.fauna)
plot(nmds.fauna, "sites")

nmds.fauna
scores.fauna <- as.data.frame(scores(nmds.fauna)$sites)
scores.fauna$Survey <- Fauna$Survey
scores.fauna$Site <- Fauna$Site
scores.fauna$Treatment <- Fauna$Treatment

avg.scores.fauna <- scores.fauna %>%
  group_by(Site, Treatment) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

ggplot(avg.scores.fauna, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 4,aes(shape = Treatment, colour = Site)) +
  geom_point(data = scores.fauna, aes(x = NMDS1, y = NMDS2, shape = Treatment, colour = Site), alpha =.5) +
  theme_classic() +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  scale_colour_manual(values = c("grey30", "grey60")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20,
                             t = 20),
        strip.text.x = element_text(size = 12))

#ANOSIM Test like in R 
anosim(mtrx.fauna, Fauna$Site, permutations = 9999, distance = "bray")
anosim(mtrx.fauna, Fauna$Treatment, permutations = 9999, distance = "bray")

#Simpsons Index - Almost got it (too many 1s for the SG Treatment)
FaunaDiversity <- 1-simpson.unb(mtrx.fauna)#Simpsons Compliment b/c Simpson D is lower with more diversity


Fauna.simp <- Fauna %>%
  mutate(CompSimpD = FaunaDiversity) %>%
  dplyr::select(Site, Treatment, CompSimpD)

ggplot(Fauna.simp, aes(x = Treatment, y = CompSimpD)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

## Block Cover Diversity -------------------------------------------------------

CoverSimp <- read.csv("D:/Projects/Blocks/Data/Cover_SimpsonD.csv") %>%
  mutate(SimpD = 1-SimpD_Cover) %>%
  group_by(Survey, Site, Block) %>%
  summarise(SimpD = mean(SimpD)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

Cover <- read.csv("D:/Projects/Blocks/Data/SurveyMacroalgae.csv") %>%
  dplyr::select(-Surveyer, -DateTime, -ID, -Q) %>%
  group_by(Survey, Site, Block) %>%
  summarise(across(everything(), mean)) %>%
  ungroup() %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

CoverSum <- Cover %>%
  group_by(Site, Treatment) %>%
  summarise(across(Ulva:Bryozoan, mean)) %>%
  pivot_longer(Ulva:Bryozoan, names_to = "Species", values_to = "CoverMean")

CoverSum$Treatment <- factor(FaunaSum$Treatment, c("SG", "CB", "OY"))

CoverSG <- CoverSum %>% filter(Treatment == "SG") %>%
  ggplot(aes(x = reorder(Species, CoverMean), CoverMean*100 , fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Species", y = "Percent Cover (%)") +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Seagrass Species") +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80"))

CoverCB <- CoverSum %>% filter(Treatment == "CB") %>%
  ggplot(aes(x = reorder(Species, CoverMean), CoverMean*100 , fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Species", y = "Percent Cover (%)") +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Control Block Species") +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80"))

CoverOY <- CoverSum %>% filter(Treatment == "OY") %>%
  ggplot(aes(x = reorder(Species, CoverMean), CoverMean*100, fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Species", y = "Percent Cover (%)") +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Oyster Reef Species") +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80"))

ggarrange(CoverSG, CoverCB, CoverOY,
          labels=c("A", "B", "C"),
          ncol = 1, nrow = 3)

prep.cover <- Cover[, 5:ncol(Cover) - 1]
mtrx.cover <- as.matrix(prep.cover)

dist.cover <- vegdist(mtrx.cover)

LCBD.cover <- LCBD.comp(dist.cover, sqrt.D = T)

nmds.cover <- metaMDS(mtrx.cover)
plot(nmds.cover)

scores.cover <- as.data.frame(scores(nmds.cover)$sites)
scores.cover$Survey <- Cover$Survey
scores.cover$Site <- Cover$Site
scores.cover$Treatment <- Fauna$Treatment

avg.scores.cover <- scores.cover %>%
  group_by(Site, Treatment) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

ggplot(avg.scores.cover, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 4,aes(shape = Treatment, colour = Site)) +
  geom_point(data = scores.cover, aes(x = NMDS1, y = NMDS2, shape = Treatment, colour = Site), alpha =.5) +
  theme_classic() +
  scale_colour_manual(values = c("grey30", "grey60")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20,
                             t = 20),
        strip.text.x = element_text(size = 12))

anosim(mtrx.cover, Cover$Site, permutations = 9999, distance = "bray")
anosim(mtrx.cover, Cover$Treatment, permutations = 9999, distance = "bray")
# Grass Survival -----

survey.survival <- read.csv("D:/Projects/Blocks/Data/SurveyGrass.csv") %>%
  dplyr::select(-Surveyer, -DateTime, -ID) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  filter(Survey < 6) %>%
  mutate(TotalSurvival = A + B + C + D)

ggplot(survey.survival, aes(Treatment, avg.survival, fill = Site)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~ Survey)