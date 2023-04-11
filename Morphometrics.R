
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

Survival2021 <- Morph2021 %>%
  group_by(ID, Site, Block, Patch) %>%
  summarise(Shoots = max(Shoot)) %>%
  full_join(Trans2021) %>%
  mutate(Treatment = case_when(startsWith(substr(ID,4,6), "G") == T ~ "SG",
                               startsWith(substr(ID,4,6), "O") == T ~ "OY",
                               startsWith(substr(ID,4,6), "C") == T ~ "CB"), 
         Survival = case_when(is.na(Shoots) == F ~ Shoots/30,
                              is.na(Shoots) == T ~ 0), Site = substr(ID, 1, 2))

site22.labs <- c("Far Pond", "Landscape Lab")
names(site22.labs) <- c("FP", "LL")

ggplot(Survival, aes(x = Treatment, y = Survival)) +
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

## Productivity (mm^2 day^-1 Shoot^-1) -------------------------------------------------
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
  facet_wrap(~Site) +
  theme_classic()

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
  facet_wrap(~Site) +
  theme_classic()

lm.branch21 <- lm(data = Branching21, Branch ~ Site * Treatment)

plot(simulateResiduals(lm.branch21)) #Passes

Anova(lm.branch21, type = 2)

em.branch21 <- emmeans(lm.branch21, ~ Treatment * Site)

pairs(em.branch21)

plot(em.branch21, comparisons = T)

cld(em.branch21)

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

## Biomass ---------------------------------------------------------------------



## Epiphytes -------------------------------------------------------------------



