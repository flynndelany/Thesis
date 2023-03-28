
## Faunal Diversity (added mud crab to 1 & 12 for test & Misc of all 1s) ------------------------

Fauna <- read.csv("D:/Projects/Blocks/Data/SurveyFauna.csv") %>%
  mutate(Treatment = case_when(startsWith(substr(ID,4,6), "G") == T ~ "SG",
                               startsWith(substr(ID,4,6), "O") == T ~ "OY",
                               startsWith(substr(ID,4,6), "C") == T ~ "CB"))

prep.fauna <- Fauna[,8:ncol(Fauna)-1]
mtrx.fauna <- as.matrix(prep.fauna)

dist.fauna <- vegdist(mtrx.fauna)

set.seed(666)
nmds.fauna <- metaMDS(mtrx.fauna)
plot(nmds.fauna)

scores.fauna <- as.data.frame(scores(nmds.fauna)$sites)
scores.fauna$Survey <- Fauna$Survey
scores.fauna$Site <- Fauna$Site
scores.fauna$Treatment <- Fauna$Treatment

avg.scores.fauna <- scores.fauna %>%
  group_by(Site, Treatment) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

ggplot(adj.scores.fauna, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 4,aes(shape = Treatment, colour = Site)) +
  geom_point(data = scores.fauna, aes(x = NMDS1, y = NMDS2, shape = Treatment, colour = Site), alpha =.5) +
  theme_classic()

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

