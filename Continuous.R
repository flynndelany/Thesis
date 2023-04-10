
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
  facet_wrap(~Site) +
  theme_classic() +
  labs(x = "Hour of the Day", y = expression(paste("PAR (", mu, "mol s"^-1, " m"^-2, ")")))

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
  ylab("Hsat (h)")

#Hsat - Saturation limit 100 - 300

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
