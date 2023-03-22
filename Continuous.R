

surveys <- c(c("2022-06-20","2022-07-08","2022-07-28","2022-08-16","2022-09-08","2022-09-30"))

#PAR umol/(s * m^2)
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

#Hobo Temp

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
