
## PAR umol/(s * m^2)-----------------------------------------------------------
surveys <- c("2022-06-20","2022-07-08","2022-07-28","2022-08-16","2022-09-08","2022-09-30")
nmcals <- c("Timestamp", "UTC", "EST", "Bat_V", "Temp_C", "PAR", "AccX", "AccY", "AccZ")

Light_LL_1 <- read.csv("D:/Projects/Blocks/Data/PAR/LLCat1.csv", col.names = nmcals) %>%
  mutate(UTC = as.POSIXct(UTC), EST = as.POSIXct(EST))
Light_LL_2 <- read.csv("D:/Projects/Blocks/Data/PAR/LLCat2.csv", col.names = nmcals) %>%
  mutate(UTC = as.POSIXct(UTC), EST = as.POSIXct(EST))

Light_LL <- rbind(Light_LL_1, Light_LL_2) %>%
  mutate(Site = "LL")

Light_FP_1 <- read.csv("D:/Projects/Blocks/Data/PAR/FPCat1.csv", col.names = nmcals) %>%
  mutate(UTC = as.POSIXct(UTC), EST = as.POSIXct(EST))
Light_FP_2 <- read.csv("D:/Projects/Blocks/Data/PAR/FPCat2.csv", col.names = nmcals) %>%
  mutate(UTC = as.POSIXct(UTC), EST = as.POSIXct(EST))

Light_FP <- rbind(Light_FP_1, Light_FP_2) %>%
  mutate(Site = "FP")

Light <- rbind(Light_LL, Light_FP)

ggplot(Light, aes(EST, PAR)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct(surveys)) +
  facet_wrap(~Site) +
  theme_classic()

AvgDailyLight <- Light %>%
  mutate(MOD = minute(EST), HOD = hour(EST) + (MOD/60)) %>%
  group_by(Site, HOD) %>%
  summarise(DailyPAR = mean(PAR)) %>%
  ungroup()

ggplot(AvgDailyLight, aes(HOD, DailyPAR)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(breaks = seq(0, 24, by = 2)) +
  theme_classic() +
  labs(x = "Hour of the Day", y = expression(paste("PAR (", mu, "mol s"^-1, " m"^-2, ")"))) +
  facet_wrap(~Site, labeller = labeller(Site = site21.labs)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12))

start <-as.Date(c("2022-06-20","2022-07-08","2022-07-28", "2022-08-10","2022-08-16","2022-09-08"), "%Y-%m-%d")
end <- as.Date(c("2022-07-08","2022-07-28","2022-07-29", "2022-08-16","2022-09-08","2022-09-30"), "%Y-%m-%d")
survey <- c(1,2,3,3,4,5)
Survey <- data.frame(survey, start, end)
Survey

adj_light <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(adj_light) <- colnames(Light)

for (i in 1:6) {
  x <- subset(Light, EST >= Survey$start[i] & EST <= Survey$end[i]) %>%
    mutate(Survey = Survey$survey[i])
  
  adj_light <- rbind(adj_light, x)
}

DailyHsat <- adj_light %>%
  mutate(Date = as.Date(EST, format = "%Y-%m-%d")) %>%
  mutate(MOD = minute(EST), HOD = hour(EST) + (MOD/60)) %>%
  group_by(Site, Survey, HOD) %>%
  summarise(DailyPAR = mean(PAR)) %>%
  ungroup(HOD) %>%
  summarise(AvgHsat = sum(DailyPAR>100)/4)
  
ggplot(DailyHsat, aes(Survey, AvgHsat, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  scale_fill_grey() +
  theme_classic() +
  ylab(expression(paste("H"["sat"]," (hours)")))

SubDailyHsat <- adj_light %>%
  mutate(Date = as.Date(EST, format = "%Y-%m-%d")) %>%
  mutate(MOD = minute(EST), HOD = hour(EST) + (MOD/60), Day = day(EST)) %>%
  group_by(Site, Survey, Day) %>%
  summarise(SubdailyPAR = sum(PAR>100)/4) %>%
  mutate(Survey = as.factor(Survey))

ggplot(SubDailyHsat, aes(Survey, SubdailyPAR, fill = Site)) +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(alpha = 0.4, position=position_dodge(width=0.75)) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 20, by = 4)) +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80")) +
  labs(x = "Survey", y = expression(paste("Daily H"["sat"]," (hours)"))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12)) +
  geom_hline(yintercept = c(7.3, 10.6))

#Hsat - Saturation limit 100

lm.hsat <- lm(SubdailyPAR ~ Site * Survey, data = SubDailyHsat)

plot(simulateResiduals(lm.hsat))

Anova(lm.hsat, type = 2)

em.hsat <- emmeans(lm.hsat, ~ Survey * Site)

pairs(em.hsat)

plot(em.hsat, comparisons = T)

cld(em.hsat)

## Hobo Temp -------------------------------------------------------------------

Temp_FP <- read.csv("D:/Projects/Blocks/HoBos/2235047.csv") %>%
  mutate(EST = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M"), Site = "FP")
Temp_LL <- read.csv("D:/Projects/Blocks/HoBos/2240646.csv") %>%
  mutate(EST = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M"), Site = "LL")
Temp <- rbind(Temp_LL, Temp_FP)

ggplot(Temp, aes(EST, TempC)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct(surveys)) +
  geom_hline(yintercept = 25.0) +
  facet_wrap(~Site) +
  theme_classic()

#Hours over 25C
start <-as.Date(c("2022-06-20","2022-07-08","2022-07-28","2022-08-16","2022-09-08"), "%Y-%m-%d")
end <- as.Date(c("2022-07-08","2022-07-28","2022-08-16","2022-09-08","2022-09-30"), "%Y-%m-%d")
survey <- c(1,2,3,4,5)
Survey <- data.frame(survey, start, end)
Survey

adj_Temp <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(adj_Temp) <- colnames(Temp)

for (i in Survey$survey) {
  x <- subset(Temp, EST >= Survey$start[i] & EST <= Survey$end[i]) %>%
    mutate(Survey = i)
  
  adj_Temp <- rbind(adj_Temp, x)
}

Over25 <- adj_Temp %>%
  group_by(Site, Survey) %>%
  summarise(Over25 = sum(TempC>25)/4)
Over25

ggplot(Over25, aes(Survey, Over25, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  scale_fill_grey() +
  theme_classic() +
  ylab("Hours Over 25C")

SubDailyOver <- adj_Temp %>%
  mutate(Date = as.Date(EST, format = "%Y-%m-%d")) %>%
  mutate(MOD = minute(EST), HOD = hour(EST) + (MOD/60), Day = day(EST)) %>%
  group_by(Site, Survey, Day) %>%
  summarise(Subdaily25 = sum(TempC>25)/4) %>%
  mutate(Survey = as.factor(Survey)) %>%
  group_by(Site) %>% summarise(mean(Subdaily25)) %>% view()


ggplot(SubDailyOver, aes(Survey, Subdaily25, fill = Site)) +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(alpha = 0.4, position=position_dodge(width=0.75)) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 24, by = 4)) +
  scale_fill_manual(labels = c("Far Pond", "Landscape Lab"), values = c("grey50", "grey80")) +
  labs(x = "Survey", y = expression(paste("Exposure Over 25\u00B0C (hours)"))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12))

lm.temp <- lm(Subdaily25 ~ Site * Survey, data = SubDailyOver)

plot(simulateResiduals(lm.temp))

Anova(lm.temp, type = 2)

em.temp <- emmeans(lm.temp, ~ Survey * Site)

pairs(em.temp)

plot(em.temp, comparisons = T)

cld(em.temp)

# Wind ----
Wind <- read.csv("D:/Projects/Blocks/Data/Wind.csv") %>%
  mutate(EST = as.POSIXct(Date, format = "%m/%d/%Y"))

ggplot(Wind, aes(EST, Direction)) +
  geom_line() +
  theme_classic() +
  geom_hline(yintercept = c(225, 180))

#avg WindSpeed of days in "fetch zone"

fp.wind <- Wind %>%
  filter(Direction > 180 & Direction < 255 & WindSpeed > 12) %>%
  tally() %>%
  mutate(Site = "Far Pond")

ll.wind <- Wind %>%
  filter(Direction > 220 & Direction < 330 & WindSpeed > 12) %>%
  tally() %>%
  mutate(Site = "Landscape Lab")

waves <- rbind(fp.wind, ll.wind)

ggplot(waves, aes(Site, n)) +
  geom_col(fill = "grey80", color = "black") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  labs(x = "Site", y = "Days of High Wind Speed") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = + 6),
        axis.title.x = element_text(vjust = - 3),
        plot.margin = margin(b = 20,
                             l = 20),
        strip.text.x = element_text(size = 12))
  
