
Morph <- read.csv("D:/Projects/Blocks/Data/Morphometrics.csv") %>%
  mutate(ID = paste(Site, Block, Patch))

Trans <- read.csv("D:/Projects/Blocks/Data/Transplantation.csv")
Trans <- Trans[-c(97:100),]
Trans <- Trans %>% transmute(ID)

## Survival --------------------------------------------------------------------

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

#Anova
lm.surv <- lm(data = Survival, Survival ~ Treatment + Site)

plot(simulateResiduals(lm.surv)) #Passes

anova(lm.surv)

#Post Hoc
em.surv <- emmeans(lm.surv, ~ Treatment * Site)

pairs(em.surv)

plot(em.surv, comparisons = T)

cld(em.surv)

## Productivity (gram or area) -------------------------------------------------
Productivity <- Morph %>%
  filter(New_mm != is.na(New_mm)) %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = n_distinct(Sht), GrowthRatio = sum(New_mm)/sum(Total_mm), GrowthRate = case_when(Site == "LL" ~ sum(New_mm)/8,
                                                                            Site == "FP" ~ sum(New_mm)/9)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  distinct() %>%
  mutate(GrowthRate = GrowthRate/Shoots) #Growth Rate is mm/(day*sht)

#Growth Rate Anova & Post Hoc
ggplot(Productivity, aes(x = Treatment, y = GrowthRate)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

lm.grate <- lm(data = Productivity, GrowthRate ~ Treatment + Site)

plot(simulateResiduals(lm.grate))

anova(lm.grate)

em.grate <- emmeans(lm.grate, ~ Treatment * Site)

pairs(em.grate)

plot(em.grate, comparisons = T)

cld(em.grate)

## Secondary Shoots ------------------------------------------------------------
Ordinal <- Morph %>%
  dplyr::select(ID, Site, Block, Patch, Sht, Parent)%>%
  distinct() %>%
  mutate(Ordinalsht=0)

for (i in Ordinal$ID) {
  z <- Ordinal %>%
    filter(ID == i)
  for (j in z$Sht) {
    x <- Ordinal %>%
      filter(ID == i & Sht == j)
    y <- Ordinal %>%
      filter(ID == i & !is.na(Parent))
    if (is.na(x$Parent)){
      Ordinal[Ordinal$ID == i & Ordinal$Sht == j, 7] = 1 
    }
    else if(x$Parent %in% y$Sht){
      Ordinal[Ordinal$ID == i & Ordinal$Sht == j, 7] = 3
    }
    else{
      Ordinal[Ordinal$ID == i & Ordinal$Sht == j, 7] = 2
    }
  }
}

OrdinalShoots <- Ordinal %>%
  group_by(Site, Block, Patch) %>%
  summarise(Primary = sum(Ordinalsht == 1), Secondary = sum(Ordinalsht == 2), Tertiary = sum(Ordinalsht == 3)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  pivot_longer(cols = c(Primary,Secondary,Tertiary), names_to = "OrdinalValue", values_to = "Count")
OrdinalShoots

ggplot(OrdinalShoots, aes(x = Treatment, y = Count)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(rows = vars(Site), cols = vars(OrdinalValue)) +
  theme_classic()

## LAI -------------------------------------------------------------------------
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

#Anova
lm.lai <- lm(data = LAI, LAI ~ Treatment + Site)

plot(simulateResiduals(lm.lai)) #Passes
 
anova(lm.lai)

#Post-hoc
em.lai <- emmeans(lm.lai, ~ Treatment * Site)

pairs(em.lai)

plot(em.lai, comparisons = T)

cld(em.lai)

## Canopy Height ---------------------------------------------------------------
Height <- Morph %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Canopy = max(Total_mm)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(Height, aes(x = Treatment, y = Canopy)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site) +
  theme_classic()

#Anova
lm.can <- lm(data = Height, Canopy ~ Treatment + Site)

plot(simulateResiduals(lm.can)) #Passes

anova(lm.can)

#Post-hoc
em.can <- emmeans(lm.can, ~ Treatment * Site)

pairs(em.can)

plot(em.can, comparisons = T)

cld(em.can)

## Biomass (Still being Weighed) -----------------------------------------------



