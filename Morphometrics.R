
Morph <- read.csv("D:/Projects/Blocks/Data/Morphometrics.csv") %>%
  mutate(ID = paste(Site, Block, Patch))

Trans <- read.csv("D:/Projects/Blocks/Data/Transplantation.csv")
Trans <- Trans[-c(97:100),]
Trans <- Trans %>% transmute(ID)

Morph2021 <- read.csv("D:/Projects/Blocks/Data/Morphometrics2021.csv") %>%
  mutate(ID = paste(Site, Block, Patch))
Trans2021 <- read.csv("D:/Projects/Blocks/Data/Transplantation2021.csv") %>%
  mutate(ID = paste(Site, Block, Patch)) %>%
  dplyr::select(ID)

## Survival (Graphs Complete) --------------------------------------------------------------------

Survival <- Morph %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = max(Sht)) %>%
  full_join(Trans) %>%
  mutate(Treatment = case_when(startsWith(substr(ID,4,6), "G") == T ~ "SG",
                               startsWith(substr(ID,4,6), "O") == T ~ "OY",
                              startsWith(substr(ID,4,6), "C") == T ~ "CB"), 
         Survival = case_when(is.na(Shoots) == F ~   signif(Shoots/30 * 100, digits = 5),
                              is.na(Shoots) == T ~ 0), Site = substr(ID, 1, 2))

Survival2021 <- Morph2021 %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = max(Shoot)) %>%
  full_join(Trans2021) %>%
  mutate(Treatment = case_when(startsWith(substr(ID,4,6), "G") == T ~ "SG",
                               startsWith(substr(ID,4,6), "O") == T ~ "OY",
                               startsWith(substr(ID,4,6), "C") == T ~ "CB"), 
         Survival = case_when(is.na(Shoots) == F ~ (Shoots/30 * 100),
                              is.na(Shoots) == T ~ 0), Site = substr(ID, 1, 2))

site22.labs <- c("Far Pond", "Landscape Lab")
names(site22.labs) <- c("FP", "LL")


ggplot(Survival, aes(x = Treatment, y = Survival)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  ylim(0,300) + 
  ylab("Survival (%)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  geom_hline(yintercept = 100, linetype = "dashed")

site21.labs <- c("Far Pond", "Landscape Lab", "Tiana Bay")
names(site21.labs) <- c("FP", "LL", "TB")

ggplot(Survival2021, aes(x = Treatment, y = Survival)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  ylim(0,3) + 
  ylab("Survival (%)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12))

#Anova
lm.surv <- lm(data = Survival, Survival ~ Site * Treatment)

plot(simulateResiduals(lm.surv)) #Passes

summary(aov(lm.surv))

Survival2021.adj <- Survival2021 %>% filter(Site != "TB")

lm.surv21 <- lm(data = Survival2021, Survival ~ Site * Treatment)

plot(simulateResiduals(lm.surv21)) #Failed

summary(aov(lm.surv21))

#Post Hoc
em.surv <- emmeans(lm.surv, ~ Treatment * Site)

pairs(em.surv)

plot(em.surv, comparisons = T)

cld(em.surv)

em.surv21 <- emmeans(lm.surv21, ~ Treatment * Site)

pairs(em.surv21)

plot(em.surv21, comparisons = T)

cld(em.surv21)

## Productivity (Graphs Complete) -------------------------------------------------
Productivity <- Morph %>%
  filter(New_mm != is.na(New_mm)) %>%
  mutate(New_lai = New_mm*Width_mm) %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = n_distinct(Sht),
            GrowthRate = case_when(Site == "LL" ~ (sum(New_lai))/(8*Shoots),
                                   Site == "FP" ~ sum(New_lai)/(9*Shoots))) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  distinct()

Productivity21 <- Morph2021 %>%
  filter(New_mm != is.na(New_mm)) %>%
  mutate(New_lai = New_mm*Width_mm) %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = n_distinct(Shoot),
            GrowthRate = case_when(Site == "LL" ~ (sum(New_lai))/(8*Shoots),
                                   Site == "FP" ~ sum(New_lai)/(9*Shoots))) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  distinct()

#Growth Rate Anova & Post Hoc
ggplot(Productivity, aes(x = Treatment, y = GrowthRate)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Productivity (mm"^" 2"," day"^-1," shoot"^-1, ")")))

ggplot(Productivity21, aes(x = Treatment, y = GrowthRate)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Productivity (mm"^" 2"," day"^-1," shoot"^-1, ")")))

lm.grate <- lm(data = Productivity, GrowthRate ~ Site * Treatment)

plot(simulateResiduals(lm.grate))

Anova(lm.grate, type = 2)

em.grate <- emmeans(lm.grate, ~ Site * Treatment)

pairs(em.grate)

plot(em.grate, comparisons = T)

cld(em.grate)

lm.grate21 <- lm(data = Productivity21, GrowthRate ~ Site * Treatment)
summary(lm.grate21)

plot(simulateResiduals(lm.grate21))

Anova(lm.grate21, type = 2)

em.grate21 <- emmeans(lm.grate21, ~ Site * Treatment)

pairs(em.grate21)

plot(em.grate21, comparisons = T)

cld(em.grate21)

## Secondary Shoots (Graphs Complete) ------------------------------------------------------------
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
  summarise(Primary = sum(Ordinalsht == 1), Secondary = sum(Ordinalsht == 2) + sum(Ordinalsht == 3)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  pivot_longer(cols = c(Primary,Secondary), names_to = "OrdinalValue", values_to = "Count")
head(OrdinalShoots)

ggplot(OrdinalShoots, aes(x = Treatment, y = Count)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(rows = vars(Site), cols = vars(OrdinalValue)) +
  theme_classic()

Branching <- OrdinalShoots %>%
  full_join(Survival) %>%
  filter(OrdinalValue == "Secondary") %>%
  dplyr::select(Site, Block, Patch, Treatment, SecondShts = Count, Shoots) %>%
  mutate(Branch = SecondShts/Shoots)

ggplot(Branching, aes(x = Treatment, y = Branch)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab("Proportion of Shoots as Lateral Branches") + ylim(0,1)

lm.branch <- lm(data = Branching, Branch ~ Site * Treatment)

plot(simulateResiduals(lm.branch)) #Passes

Anova(lm.branch, type = 2)

em.branch <- emmeans(lm.branch, ~ Treatment * Site)

pairs(em.branch)

plot(em.branch, comparisons = T)

cld(em.branch)

Ordinal21 <- Morph2021 %>%
  dplyr::select(ID, Site, Block, Patch, Shoot, Parent)%>%
  distinct() %>%
  mutate(Ordinalsht=0)

for (i in Ordinal21$ID) {
  z <- Ordinal21 %>%
    filter(ID == i)
  for (j in z$Shoot) {
    x <- Ordinal21 %>%
      filter(ID == i & Shoot == j)
    y <- Ordinal21 %>%
      filter(ID == i & !is.na(Parent))
    if (is.na(x$Parent)){
      Ordinal21[Ordinal21$ID == i & Ordinal21$Shoot == j, 7] = 1 
    }
    else if(x$Parent %in% y$Shoot){
      Ordinal21[Ordinal21$ID == i & Ordinal21$Shoot == j, 7] = 3
    }
    else{
      Ordinal21[Ordinal21$ID == i & Ordinal21$Shoot == j, 7] = 2
    }
  }
}

OrdinalShoots21 <- Ordinal21 %>%
  group_by(Site, Block, Patch) %>%
  summarise(Primary = sum(Ordinalsht == 1), Secondary = sum(Ordinalsht == 2) + sum(Ordinalsht ==3)) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  pivot_longer(cols = c(Primary,Secondary), names_to = "OrdinalValue", values_to = "Count")
head(OrdinalShoots21)

ggplot(OrdinalShoots21, aes(x = Treatment, y = Count)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(rows = vars(Site), cols = vars(OrdinalValue)) +
  theme_classic()

Branching21 <- OrdinalShoots21 %>%
  full_join(Survival2021) %>%
  filter(OrdinalValue == "Secondary") %>%
  dplyr::select(Site, Block, Patch, Treatment, SecondShts = Count, Shoots) %>%
  mutate(Branch = SecondShts/Shoots)

ggplot(Branching21, aes(x = Treatment, y = Branch)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab("Proportion of Shoots as Lateral Branches") +
  ylim(0,1)

lm.branch21 <- lm(data = Branching21, Branch ~ Site * Treatment)

plot(simulateResiduals(lm.branch21)) #Passes

Anova(lm.branch21, type = 2)

em.branch21 <- emmeans(lm.branch21, ~ Treatment * Site)

pairs(em.branch21)

plot(em.branch21, comparisons = T)

cld(em.branch21)

## LAI (Graphs Complete) -------------------------------------------------------------------------
LAI <- Morph %>%
  mutate(LAI = Total_mm * Width_mm, NewLAI = New_mm * Width_mm) %>%
  group_by(ID, Site, Block, Patch, Sht) %>%
  summarise(LAI = sum(LAI), NewLAI = sum(NewLAI)) %>%
  ungroup(Sht) %>%
  summarise(avgLAI = mean(LAI)/100, LAI = sum(LAI)/100) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(LAI, aes(x = Treatment, y = avgLAI)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Leaf Area (cm"^2," shoot"^-1, ")"))) + ylim(0,25)

#Anova
lm.lai <- lm(data = LAI, avgLAI ~ Treatment * Site)

plot(simulateResiduals(lm.lai)) #Passes
 
Anova(lm.lai, Type = 2)

#Post-hoc
em.lai <- emmeans(lm.lai, ~ Treatment)

pairs(em.lai)

plot(em.lai, comparisons = T)

cld(em.lai)

LAI21 <- Morph2021 %>%
  mutate(LAI = Total_mm * Width_mm, NewLAI = New_mm * Width_mm) %>%
  group_by(ID, Site, Block, Patch, Shoot) %>%
  summarise(LAI = sum(LAI), NewLAI = sum(NewLAI)) %>%
  ungroup(Shoot) %>%
  summarise(avgLAI = mean(LAI)/100, LAI = sum(LAI)/100) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(LAI21, aes(x = Treatment, y = avgLAI)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Leaf Area (cm"^2," shoot"^-1, ")"))) + ylim(0,25)

#Anova
lm.lai21 <- lm(data = LAI21, avgLAI ~ Treatment * Site)

plot(simulateResiduals(lm.lai21)) #Passes

Anova(lm.lai21, type = 2)

#Post-hoc
em.lai21 <- emmeans(lm.lai21, ~ Treatment * Site)

pairs(em.lai21)

plot(em.lai21, comparisons = T)

cld(em.lai21)

## Canopy Height (Graphs Complete) ---------------------------------------------------------------
Height <- Morph %>%
  group_by(ID, Site, Block, Patch, Sht) %>%
  summarise(Canopy = max(Total_mm)) %>%
  ungroup(Sht)%>%
  summarise(avgCanopy = mean(Canopy))%>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(Height, aes(x = Treatment, y = avgCanopy)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Canopy Height (mm shoot"^-1, ")"))) + ylim(0,350)

#Anova
lm.can <- lm(data = Height, avgCanopy ~ Treatment * Site)

plot(simulateResiduals(lm.can)) #Passes

Anova(lm.can, type = 2)

#Post-hoc
em.can <- emmeans(lm.can, ~ Treatment * Site)

pairs(em.can)

plot(em.can, comparisons = T)

cld(em.can)

Height21 <- Morph2021 %>%
  group_by(ID, Site, Block, Patch, Shoot) %>%
  summarise(Canopy = max(Total_mm)) %>%
  ungroup(Shoot)%>%
  summarise(avgCanopy = mean(Canopy))%>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))

ggplot(Height21, aes(x = Treatment, y = avgCanopy)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Canopy Height (mm shoot"^-1, ")"))) + ylim(0,350)

#Anova
lm.can21 <- lm(data = Height21, avgCanopy ~ Treatment * Site)

plot(simulateResiduals(lm.can21)) #Passes

Anova(lm.can21, type = 2)

#Post-hoc
em.can21 <- emmeans(lm.can21, ~ Treatment * Site)

pairs(em.can21)

plot(em.can21, comparisons = T)

cld(em.can21)

## Above Biomass (Graphs Complete) ---------------------------------------------------------------
AbovetoAdd <- read.csv("D:/Projects/Blocks/Data/BM_AboveLost.csv") %>%
  left_join(Survival) %>%
  dplyr::select(Site = SITE, Block = TREATMENT, Patch = PATCH, Above = ABOVE_g, Shoots ) %>%
  mutate(Above = Above/Shoots) %>%
  dplyr::select(-Shoots)
Above_bm <- read.csv("D:/Projects/Blocks/Data/BM_Above.csv") %>%
  group_by(Site, Block, Patch, Sht) %>%
  summarise(Above = sum(Above_g)) %>%
  ungroup(Sht) %>%
  summarise(Above = mean(Above)) %>%
  rbind(AbovetoAdd) %>%
  group_by(Site, Block, Patch) %>%
  summarise(Above = sum(Above)*1000) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"))



ggplot(Above_bm, aes(Treatment, Above)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  ylim(0,100) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Aboveground Biomass (mg shoot"^-1, ")")))

lm.above <- lm(Above ~ Site * Treatment, Above_bm)

plot(simulateResiduals(lm.above))

Anova(lm.above, type = 2)

## Below Biomass (Graphs Complete) ---------------------------------------------------------------
Below_bm <- read.csv("D:/Projects/Blocks/Data/BM_Below.csv") %>%
  dplyr::select(ID, Below) %>%
  left_join(Survival) %>%
  mutate(Below = Below/Shoots*1000) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB")) %>%
  arrange(ID)

ggplot(Below_bm, aes(Treatment, Below)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Belowground Biomass (mg shoot"^-1, ")"))) + ylim(0,800)

lm.below <- lm(Below ~ Site * Treatment, Below_bm)

plot(simulateResiduals(lm.below))

Anova(lm.below, type = 2)

em.above <- emmeans(lm.above, ~ Treatment * Site)

pairs(em.above)

plot(em.above, comparisons = T)

cld(em.above)

## Epiphytes (Graphs Complete) -------------------------------------------------------------------

epiphyte_grass <- read.csv("D:/Projects/Blocks/Data/EpiphytesGrass.csv") %>% 
  dplyr::select(Site = SITE, Block = TREATMENT, Patch = PATCH, Epiphyte_g = Dry_g) %>%
  left_join(Survival) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"),
         Epiphyte = Epiphyte_g/Shoots*1000) %>%
  left_join(LAI) %>%
  mutate(Epiphyte = signif(Epiphyte/LAI, digits =  6))

ggplot(epiphyte_grass, aes(Treatment, Epiphyte)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Epiphytic Load (mg cm"^-2," shoot"^-1, ")")))

#Anova
lm.phyte <- lm(data = epiphyte_grass, Epiphyte ~ Treatment * Site)

plot(simulateResiduals(lm.phyte)) #Passes

Anova(lm.phyte, type = 2)

#Post-hoc
em.phyte <- emmeans(lm.phyte, ~ Treatment * Site)

pairs(em.phyte)

plot(em.phyte, comparisons = T)

cld(em.phyte)

epiphyte_grass21 <- read.csv("D:/Projects/Blocks/Data/EpiphytesGrass2021.csv") %>%
  filter(Block != "Nat") %>%
  left_join(Survival2021) %>%
  mutate(Treatment = case_when(startsWith(Block, "G") == T ~ "SG",
                               startsWith(Block, "O") == T ~ "OY",
                               startsWith(Block, "C") == T ~ "CB"),
         Epiphyte_g = Tin_Epiphyte_g - Tin_g,
         Epiphyte = Epiphyte_g/Shoots*1000) %>%
  left_join(LAI21) %>%
  mutate(Epiphyte = Epiphyte/LAI)
  
ggplot(epiphyte_grass21, aes(Treatment, Epiphyte)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  ylab(expression(paste("Epiphytic Load (mg cm"^-2," shoot"^-1, ")"))) +
  ylim(0,3)

lm.phyte21 <- lm(data = epiphyte_grass21, Epiphyte ~ Treatment * Site)

plot(simulateResiduals(lm.phyte21)) #Passes - ish

Anova(lm.phyte21, type = 3)

#Post-hoc
em.phyte21 <- emmeans(lm.phyte21, ~ Treatment * Site)

pairs(em.phyte21)

plot(em.phyte21, comparisons = T)

cld(em.phyte21)
