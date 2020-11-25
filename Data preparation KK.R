####### Making the best of a bad job? Chick mortality and flexible female brood care in Snowy Plovers ########
####### Kupan et al. 2019 ########


### Data set preparation for analysis

Broods <- read.csv("Broods_day_desertion.csv")

install.packages("easypackages")
library(easypackages)

## Installing and loading necessary packages
packages("dplyr", "ggplot2", "smart", "magrittr", "MASS",
         "data.table", "ggpubr", "lmodel2", "tidyr",
         "lubridate", "gtools")

library(dplyr)
library(ggplot2)
library(smatr)
library(magrittr)
library(MASS)
library(data.table)
library(ggpubr)
library(lmodel2)
library(tidyr)
library(lubridate)
library(gtools)

Broods_ind <- Broods %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) # 331


### Creating a full sequence of observations in brood file 

# Cutting off brood age at 25 days or the one next

Broods_a <- Broods %>% group_by(Nest_ID) %>% filter(Brood_age < 26)
Broods_b <- Broods %>% group_by(Nest_ID) %>% filter(Brood_age >= 26) %>% slice(which.min(Brood_age))
Broods <- smartbind(as.data.frame(Broods_a), as.data.frame(Broods_b))
Broods <- Broods %>% arrange(Nest_ID, Brood_age)

seq_brood_age <- function(x) seq(min(x), max(x), by = 1)

Broods_fullday <- do.call("rbind", by(Broods, Broods$Nest_ID, with,
                                               data.frame(Nest_ID = Nest_ID[1], 
                                                          Brood_age = seq_brood_age(as.numeric(Brood_age)))))

seq_brood_date <- function(x) seq(min(as.Date(x)), max(as.Date(x)), by = "days")

Broods_fulldate <- do.call("rbind", by(Broods, Broods$Nest_ID,
                                                with, data.frame(Nest_ID = Nest_ID[1], 
                                                                 Date2 = seq_brood_date(Date2))))

Broods_full <- merge(Broods_fullday , Broods_fulldate, by=0, all=TRUE)

Broods_full <- Broods_full %>% arrange(Nest_ID.x, Brood_age)

Broods_full <- subset(Broods_full, is.na(Brood_age) != TRUE)

Broods_full$dayID <- paste(Broods_full$Nest_ID.x, Broods_full$Brood_age, sep = "_")

Broods$dayID <- paste(Broods$Nest_ID, Broods$Brood_age, sep = "_")

Broods2 <- merge(Broods_full, Broods, by = "dayID" , all = TRUE) 

Broods2 <- Broods2[, !names(Broods2) %in% c("Nest_ID.y", "Nest_ID", "X.3", "X.2", "X.1", "X", "Date2.y", "Brood_age.y", "Row.names")]

colnames(Broods2) <- sub('.x', '', colnames(Broods2))

# Filling up the gaps 

Broods2 <- Broods2 %>% arrange(Nest_ID, Brood_age)

Broods2 <- Broods2 %>%
  group_by(Nest_ID) %>%
  fill(c(5:22, 24, 34:53)) %>%
  fill(c(5:22, 24, 34:53),  .direction = "up")

write.csv(Broods2, "Broods_desertion2.csv")


# Filling up _presence and _care data

Broods2 <- read.csv("Broods_desertion2.csv")

# Chick1 presence - based on observed number of chicks

Chick1_last_obs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick1_pres == "2") %>% slice(which.max(Brood_age))
Broods2$Chick1_last_obs <- as.character(Chick1_last_obs$Date2[match(Broods2$Nest_ID, Chick1_last_obs$Nest_ID)])
Broods2$Chick1_last_obs2 <-  as.character(as.Date(Broods2$Chick1_last_obs) + 1)
Broods2 <- Broods2 %>% group_by(Nest_ID) %>% mutate(Chick1_pres2 = ifelse(as.Date(Date2) <= 
                                                                        as.Date(Chick1_last_obs), 1, Chick1_pres))
Chick1_first_unobs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick1_pres2 == "0") %>%  slice(which.min(Brood_age))
Broods2$Chick1_first_unobs <- as.character(Chick1_first_unobs$Date2[match(Broods2$Nest_ID, Chick1_first_unobs$Nest_ID)])
Broods2$Chick1_first_unobs <-  as.Date(with(Broods2, ifelse(is.na(Chick1_first_unobs), Chick1_last_obs2, Chick1_first_unobs)), origin="1970-01-01")

Broods2$Chick1_min_age <- as.numeric(difftime(as.Date(Broods2$Chick1_last_obs), as.Date(Broods2$Chick1_HD),units = "days"))
Broods2$Chick1_max_age <- as.numeric(difftime(as.Date(Broods2$Chick1_first_unobs-1), as.Date(Broods2$Chick1_HD),units = "days"))
Broods2$Chick1_pres3 <- ceiling(rowMeans(Broods2[c("Chick1_min_age", "Chick1_max_age")], na.rm = TRUE))+1 # +1 is the day of the hatching


# Chick2 presence

Chick2_last_obs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick2_pres == "2") %>% slice(which.max(Brood_age))
Broods2$Chick2_last_obs <- as.character(Chick2_last_obs$Date2[match(Broods2$Nest_ID, Chick2_last_obs$Nest_ID)])
Broods2$Chick2_last_obs2 <-  as.character(as.Date(Broods2$Chick2_last_obs) + 1)
Broods2 <- Broods2 %>% group_by(Nest_ID) %>% mutate(Chick2_pres2 = ifelse(as.Date(Date2) <= 
                                                                            as.Date(Chick2_last_obs), 1, Chick2_pres))
Chick2_first_unobs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick2_pres2 == "0") %>%  slice(which.min(Brood_age))
Broods2$Chick2_first_unobs <- as.character(Chick2_first_unobs$Date2[match(Broods2$Nest_ID, Chick2_first_unobs$Nest_ID)])
Broods2$Chick2_first_unobs <-  as.Date(with(Broods2, ifelse(is.na(Chick2_first_unobs), Chick2_last_obs2, Chick2_first_unobs)), origin="1970-01-01")

Broods2$Chick2_min_age <- as.numeric(difftime(as.Date(Broods2$Chick2_last_obs), as.Date(Broods2$Chick2_HD),units = "days"))
Broods2$Chick2_max_age <- as.numeric(difftime(as.Date(Broods2$Chick2_first_unobs-1), as.Date(Broods2$Chick2_HD),units = "days"))
Broods2$Chick2_pres3 <- ceiling(rowMeans(Broods2[c("Chick2_min_age", "Chick2_max_age")], na.rm = TRUE))+1


# Chick3 presence

Chick3_last_obs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick3_pres == "2") %>% slice(which.max(Brood_age))
Broods2$Chick3_last_obs <- as.character(Chick3_last_obs$Date2[match(Broods2$Nest_ID,Chick3_last_obs$Nest_ID)])
Broods2$Chick3_last_obs2 <-  as.character(as.Date(Broods2$Chick3_last_obs) + 1)
Broods2 <- Broods2 %>% group_by(Nest_ID) %>% mutate(Chick3_pres2 = ifelse(as.Date(Date2) <= 
                                                                            as.Date(Chick3_last_obs), 1, Chick3_pres))
Chick3_first_unobs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick3_pres2 == "0") %>%  slice(which.min(Brood_age))
Broods2$Chick3_first_unobs <- as.character(Chick3_first_unobs$Date2[match(Broods2$Nest_ID, Chick3_first_unobs$Nest_ID)])
Broods2$Chick3_first_unobs <-  as.Date(with(Broods2, ifelse(is.na(Chick3_first_unobs), Chick3_last_obs2, Chick3_first_unobs)), origin="1970-01-01")

Broods2$Chick3_min_age <- as.numeric(difftime(as.Date(Broods2$Chick3_last_obs), as.Date(Broods2$Chick3_HD),units = "days"))
Broods2$Chick3_max_age <- as.numeric(difftime(as.Date(Broods2$Chick3_first_unobs-1), as.Date(Broods2$Chick3_HD),units = "days"))
Broods2$Chick3_pres3 <- ceiling(rowMeans(Broods2[c("Chick3_min_age", "Chick3_max_age")], na.rm = TRUE))+1

# Chick4 presence

Chick4_last_obs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick4_pres == "2") %>% slice(which.max(Brood_age))
Broods2$Chick4_last_obs <- as.character(Chick4_last_obs$Date2[match(Broods2$Nest_ID,Chick4_last_obs$Nest_ID)])
Broods2$Chick4_last_obs2 <-  as.character(as.Date(Broods2$Chick4_last_obs) + 1)
Broods2 <- Broods2 %>% group_by(Nest_ID) %>% mutate(Chick4_pres2 = ifelse(as.Date(Date2) <= 
                                                                            as.Date(Chick4_last_obs), 1, Chick4_pres))
Chick4_first_unobs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick4_pres2 == "0") %>%  slice(which.min(Brood_age))
Broods2$Chick4_first_unobs <- as.character(Chick4_first_unobs$Date2[match(Broods2$Nest_ID, Chick4_first_unobs$Nest_ID)])
Broods2$Chick4_first_unobs <-  as.Date(with(Broods2, ifelse(is.na(Chick4_first_unobs), Chick4_last_obs2, Chick4_first_unobs)), origin="1970-01-01")

Broods2$Chick4_min_age <- as.numeric(difftime(as.Date(Broods2$Chick4_last_obs), as.Date(Broods2$Chick4_HD),units = "days"))
Broods2$Chick4_max_age <- as.numeric(difftime(as.Date(Broods2$Chick4_first_unobs-1), as.Date(Broods2$Chick4_HD),units = "days"))
Broods2$Chick4_pres3 <- ceiling(rowMeans(Broods2[c("Chick4_min_age", "Chick4_max_age")], na.rm = TRUE))+1

# Chick5 presence

Chick5_last_obs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick5_pres == "2") %>% slice(which.max(Brood_age))
Broods2$Chick5_last_obs <- as.character(Chick5_last_obs$Date2[match(Broods2$Nest_ID,Chick5_last_obs$Nest_ID)])
Broods2$Chick5_last_obs2 <-  as.character(as.Date(Broods2$Chick5_last_obs) + 1)
Broods2 <- Broods2 %>% group_by(Nest_ID) %>% mutate(Chick5_pres2 = ifelse(as.Date(Date2) <= 
                                                                            as.Date(Chick5_last_obs), 1, Chick5_pres))
Chick5_first_unobs <-Broods2 %>% group_by(Nest_ID) %>% filter(Chick5_pres2 == "0") %>%  slice(which.min(Brood_age))
Broods2$Chick5_first_unobs <- as.character(Chick5_first_unobs$Date2[match(Broods2$Nest_ID, Chick5_first_unobs$Nest_ID)])
Broods2$Chick5_first_unobs <-  as.Date(with(Broods2, ifelse(is.na(Chick5_first_unobs), Chick5_last_obs2, Chick5_first_unobs)), origin="1970-01-01")

Broods2$Chick5_min_age <- as.numeric(difftime(as.Date(Broods2$Chick5_last_obs), as.Date(Broods2$Chick5_HD),units = "days"))
Broods2$Chick5_max_age <- as.numeric(difftime(as.Date(Broods2$Chick5_first_unobs-1), as.Date(Broods2$Chick5_HD),units = "days"))
Broods2$Chick5_pres3 <- ceiling(rowMeans(Broods2[c("Chick5_min_age", "Chick5_max_age")], na.rm = TRUE))+1

Broods2$Date2 <- as.Date(Broods2$Date2)
Broods2$Chick1_HD <- as.Date(Broods2$Chick1_HD)
Broods2$Chick2_HD <- as.Date(Broods2$Chick2_HD)
Broods2$Chick3_HD <- as.Date(Broods2$Chick3_HD)
Broods2$Chick4_HD <- as.Date(Broods2$Chick4_HD)
Broods2$Chick5_HD <- as.Date(Broods2$Chick5_HD)

Broods2$Chick1_last <- as.Date(Broods2$Chick1_HD + Broods2$Chick1_pres3)
Broods2$Chick2_last <- as.Date(Broods2$Chick2_HD + Broods2$Chick2_pres3)
Broods2$Chick3_last <- as.Date(Broods2$Chick3_HD + Broods2$Chick3_pres3)
Broods2$Chick4_last <- as.Date(Broods2$Chick4_HD + Broods2$Chick4_pres3)
Broods2$Chick5_last <- as.Date(Broods2$Chick5_HD + Broods2$Chick5_pres3)

Broods2$Chick1_pres_full <- ifelse(Broods2$Date2 >= Broods2$Chick1_HD & 
                                       Broods2$Date2 < Broods2$Chick1_last, 1, 0)
Broods2$Chick2_pres_full <- ifelse(Broods2$Date2 >= Broods2$Chick2_HD & 
                                     Broods2$Date2 < Broods2$Chick2_last, 1, 0)
Broods2$Chick3_pres_full <- ifelse(Broods2$Date2 >= Broods2$Chick3_HD & 
                                     Broods2$Date2 < Broods2$Chick3_last, 1, 0)
Broods2$Chick4_pres_full <- ifelse(Broods2$Date2 >= Broods2$Chick4_HD & 
                                     Broods2$Date2 < Broods2$Chick4_last, 1, 0)
Broods2$Chick5_pres_full <- ifelse(Broods2$Date2 >= Broods2$Chick5_HD & 
                                     Broods2$Date2 < Broods2$Chick5_last, 1, 0)

# Adding adopted chicks too to new chick presence variables

Broods2$Chick1_pres_full <- ifelse(Broods2$Chick1_pres %in% 1, 1, Broods2$Chick1_pres_full)
Broods2$Chick2_pres_full <- ifelse(Broods2$Chick2_pres %in% 1, 1, Broods2$Chick2_pres_full)
Broods2$Chick3_pres_full <- ifelse(Broods2$Chick3_pres %in% 1, 1, Broods2$Chick3_pres_full)
Broods2$Chick4_pres_full <- ifelse(Broods2$Chick4_pres %in% 1, 1, Broods2$Chick4_pres_full)
Broods2$Chick5_pres_full <- ifelse(Broods2$Chick5_pres %in% 1, 1, Broods2$Chick5_pres_full)


### Creating a new Chick_nr (daily chick observation) variable that containes all the chicks that were present - 
# independently wherther it was an own (2) or an adopted (1) chick.
# (sometimes the observer missed a chick but from the data later we can see that the chick was still around)

Broods2$Chick_nr2 <- rowSums(Broods2[c("Chick1_pres_full", "Chick2_pres_full", 
                                      "Chick3_pres_full", "Chick4_pres_full",
                                      "Chick5_pres_full")], na.rm = TRUE)

write.csv(Broods2, "Broods_desertion2.csv") 


### Male condition index

Broods2 <- read.csv("Broods_desertion2.csv")

Broods2$M_tarsus_mean <- rowMeans(Broods2[c("M_left_tarsus", "M_left_tarsus")], na.rm = TRUE)

# Removing measurement error

# Bivariate plotting
ggplot(Broods2)  + 
  geom_point(aes(x = M_tarsus_mean, y = M_weight))

# Remove outliers

Broods2_2 <- Broods2[!(Broods2$M_tarsus_mean > 26 & Broods2$M_weight < 35),]
Broods2_2 <- Broods2_2[!(Broods2_2$M_tarsus_mean < 24 & Broods2_2$M_weight > 40),]
Broods2_2 <- Broods2_2[!(Broods2_2$M_tarsus_mean <= 25 & Broods2_2$M_weight > 42.5),]
Broods2_2 <- Broods2_2[!(Broods2_2$M_weight >= 44),]

ggplot(Broods2_2)  + 
  geom_point(aes(x = M_tarsus_mean, y = M_weight))

plot(hist(as.numeric(Broods2_2$M_tarsus_mean)))
plot(hist(as.numeric(Broods2_2$M_weight)))


# Apply SMA regression

Ex2.res <- lmodel2(log(M_weight) ~ log(M_tarsus_mean), data=Broods2_2, "relative", "relative", 99)
Ex2.res
op <- par(mfrow = c(1,2))
plot(Ex2.res, "SMA")
plot(Ex2.res, "RMA")
par(op)

Tarsus_m_0 <- mean(Broods2$M_tarsus_mean, na.rm = TRUE)
b.msa.rob <- 2.1

# SMI.rob <- y * (x.0 / x) ^ b.msa.rob

SMI.rob <- Broods2$M_weight * (Tarsus_m_0 / Broods2$M_tarsus_mean) ^ b.msa.rob
SMI.rob <- unlist(SMI.rob)

Broods2$Male_CI <-  SMI.rob


###Female condition index

Broods2$F_tarsus_mean <- rowMeans(Broods2[c("F_left_tarsus", "F_left_tarsus")], na.rm = TRUE)

# removing measurement errors

# Bivariate plotting
ggplot(Broods2)  + 
  geom_point(aes(x = F_tarsus_mean, y = F_weight))

Broods2$F_tarsus_mean <- ifelse(Broods2$F_tarsus_mean > 28,
                               NA, Broods2$F_tarsus_mean )
Broods2$F_weight <- ifelse(Broods2$F_weight > 45,
                          NA, Broods2$F_weight )

# Remove outliers

Broods2_2 <- Broods2[!(Broods2$F_weight < 32.5),]
Broods2_2 <- Broods2_2[!(Broods2_2$F_tarsus_mean > 26 & Broods2_2$F_weight < 40),]
Broods2_2 <- Broods2_2[!(Broods2_2$F_weight > 44),]
Broods2_2 <- Broods2_2[!(Broods2_2$F_tarsus_mean <= 23.5),]

ggplot(Broods2_2)  + 
  geom_point(aes(x = F_tarsus_mean, y = F_weight))

plot(hist(as.numeric(Broods2_2$F_tarsus_mean)))
plot(hist(as.numeric(Broods2_2$F_weight)))

# Apply SMA regression

Ex2.res <- lmodel2(log(F_weight) ~ log(F_tarsus_mean), data=Broods2_2, "relative", "relative", 99)
Ex2.res
op <- par(mfrow = c(1,2))
plot(Ex2.res, "SMA")
plot(Ex2.res, "RMA")
par(op)

Tarsus_f_0 <- mean(Broods2$F_tarsus_mean, na.rm = TRUE)
b.msa.rob <- 2.6

# SMI.rob <- y * (x.0 / x) ^ b.msa.rob

SMI.rob <- Broods2$F_weight * (Tarsus_f_0 / Broods2$F_tarsus_mean) ^ b.msa.rob
SMI.rob <- unlist(SMI.rob)
Broods2$Female_CI <-  SMI.rob

boxplot(Broods2$Female_CI, Broods2$Male_CI)

Broods3 <- subset(Broods2, select = -c(M_weight, F_weight, M_right_tarsus,    
                                       F_right_tarsus,     M_left_tarsus,    F_left_tarsus, M_left_wing,      F_left_wing,       
                                       M_right_wing,       F_right_wing,       M_cap_date,         F_cap_date,      
                                       Chick1_last_obs2,   Chick1_pres2,       Chick1_first_unobs, Chick1_min_age, Chick1_max_age,  
                                       Chick1_pres3,      Chick2_last_obs2,   Chick2_pres2,  Chick2_first_unobs,
                                       Chick2_min_age,     Chick2_max_age ,    Chick2_pres3 ,         Chick3_last_obs2,  
                                       Chick3_pres2 ,      Chick3_first_unobs, Chick3_min_age,     Chick3_max_age ,    Chick3_pres3,      
                                       Chick4_last_obs2 ,  Chick4_pres2 ,      Chick4_first_unobs, Chick4_min_age,    
                                       Chick4_max_age ,    Chick4_pres3   ,        Chick5_last_obs2 ,  Chick5_pres2,      
                                       Chick5_first_unobs, Chick5_min_age  ,   Chick5_max_age ,    Chick5_pres3 ,      
                                       Chick1_last, Chick2_last , Chick3_last, Chick4_last ,       Chick5_last,
                                       Chick1_last_obs, Chick2_last_obs,
                                       Chick3_last_obs, Chick4_last_obs,
                                       Chick5_last_obs)) 

write.csv(Broods3, "Broods_day_desertion3.csv")


### Adding relative hatching date

Broods3 <- read.csv("Broods_day_desertion3.csv")

Broods3_ind <- Broods3 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))# 331

Broods3 <- Broods3 %>% arrange(Nest_ID, Brood_age)

Broods3$julian_hatching_date<-yday(Broods3$Hatching_date)
Broods3$julian_hatching_date<-as.integer(Broods3$julian_hatching_date)
Mean_hatching <- Broods3 %>% group_by(Year) %>% filter(!is.na(julian_hatching_date)) %>% 
  summarise(mean=mean(julian_hatching_date), sd=sd(julian_hatching_date))
Broods3$Mean_hatching_date<-Mean_hatching$mean[match(Broods3$Year, Mean_hatching$Year)]
Broods3$Sd <- Mean_hatching$sd[match(Broods3$Year, Mean_hatching$Year)]
Broods3$Relative_hatching_date <- (Broods3$julian_hatching_date - Broods3$Mean_hatching_date) / Broods3$Sd

### Calculating chick max presence - the last day when any chicks were still observed

Broods3[,"Chick_presence_max_age"] <- apply(Broods3[, c("Chick1_surv_age","Chick2_surv_age",
                                                      "Chick3_surv_age","Chick4_surv_age",
                                                      "Chick5_surv_age")], 1, max, na.rm = TRUE)


### Chick_presence_max_day: last day in brood age when a chick was still there

Chick_presence_max_day <- Broods3 %>% group_by(Nest_ID) %>% filter(Chick_nr2 != 0) %>% slice(which.max(Brood_age))

Broods3$Chick_presence_max_day <- Chick_presence_max_day$Brood_age[match(Broods3$Nest_ID, Chick_presence_max_day$Nest_ID)]

### Adjusting male and female obs data to Broods3 

# Changing male and female presence to 1 when they were there but not observed 

Broods3$Male_care_full <- ifelse(as.Date(Broods3$Date2) >= as.Date(Broods3$Hatching_date) & 
                                  Broods3$Brood_age <= ceiling(Broods3$Male_dur_care)-1, 1, 0)

Broods3$Female_care_full <- ifelse(as.Date(Broods3$Date2) >= as.Date(Broods3$Hatching_date) & 
                                    Broods3$Brood_age <= ceiling(Broods3$Female_dur_care)-1,1,0)

# Changing male and female presence to 0 when there were no chicks in the brood anymore

Broods3$Male_care_full <- ifelse(Broods3$Brood_age > Broods3$Chick_presence_max_day, 
                                0, Broods3$Male_care_full )

Broods3$Female_care_full<- ifelse(Broods3$Brood_age > Broods3$Chick_presence_max_day, 
                                 0, Broods3$Female_care_full )

# Changing female dur care at least to 1 for HD

Broods3$Female_care_full <- ifelse(Broods3$Brood_age == 0, 1, Broods3$Female_care_full)
Broods3$Female_dur_care <- ifelse(Broods3$Female_dur_care == 0, 1, Broods3$Female_dur_care)

# Changing female and male dur care in broods where they are bigger than chick presence max

Broods3$Female_dur_care <- ifelse(Broods3$Female_dur_care > (Broods3$Chick_presence_max_day+1), 
                                 (Broods3$Chick_presence_max_day + 1), Broods3$Female_dur_care) 

Broods3$Female_dur_care <- ifelse(Broods3$Female_dur_care > 26, 26, Broods3$Female_dur_care)

Broods3$Male_dur_care <- ifelse(Broods3$Male_dur_care > (Broods3$Chick_presence_max_day+1), 
                               (Broods3$Chick_presence_max_day + 1), Broods3$Male_dur_care) 

Broods3$Male_dur_care <- ifelse(Broods3$Male_dur_care > 26, 26, Broods3$Male_dur_care)

## Cutting off observations when noone was there anymore adding a 0 presence row when brood died

# Creating a general "Brood fate" variable based on the fate of the last chick in the brood

Broods3$Brood_fate <- ifelse(Broods3$Chick1_surv_age == Broods3$Chick_presence_max_age, as.character(Broods3$Chick1_Fate),
                             NA)

Broods3$Brood_fate <- ifelse(is.na(Broods3$Brood_fate) == TRUE & 
                               Broods3$Chick2_surv_age == Broods3$Chick_presence_max_age, 
                             as.character(Broods3$Chick2_Fate), Broods3$Brood_fate)

Broods3$Brood_fate <- ifelse(is.na(Broods3$Brood_fate) == TRUE & 
                               Broods3$Chick3_surv_age == Broods3$Chick_presence_max_age, 
                             as.character(Broods3$Chick3_Fate), Broods3$Brood_fate)


Broods3_a <- subset(Broods3, Brood_fate != "died")
Broods3_a <- Broods3_a %>% group_by(Nest_ID) %>% filter(Brood_age <= ceiling(Chick_presence_max_day))

Broods3_b <- subset(Broods3, Brood_fate == "died")
Broods3_b <- Broods3_b %>% group_by(Nest_ID) %>% filter(Brood_age <= ceiling(Chick_presence_max_day+1))

# Creating a row at the end with 0 chicks for broods that died for those that don't have 
Broods3_b_max <- Broods3_b %>% group_by(Nest_ID) %>% slice(which.max(Brood_age)) %>% filter(Chick_nr2 > 0)

Broods3_b_max$dayID <- NA
Broods3_b_max$Brood_age <- Broods3_b_max$Brood_age+1
Broods3_b_max$Date2 <- as.Date(Broods3_b_max$Date2) + 1
Broods3_b_max$Date <- NA
Broods3_b_max$Parents <- 0
Broods3_b_max$male_care <- NA
Broods3_b_max$female_care <- NA
Broods3_b_max$Chick1_pres_full <- 0
Broods3_b_max$Female_care_full <- 0
Broods3_b_max$Male_care_full <- 0
Broods3_b_max$Chick2_pres_full <- 0
Broods3_b_max$Chick3_pres_full <- 0
Broods3_b_max$Chick4_pres_full <- 0
Broods3_b_max$Chick5_pres_full <- 0
Broods3_b_max$Chick_nr2<- 0
Broods3_b_max$Chick1_pres <- NA
Broods3_b_max$Chick2_pres <- NA
Broods3_b_max$Chick3_pres <- NA
Broods3_b_max$Chick4_pres <- NA
Broods3_b_max$Chick5_pres <- NA

Broods4_b <- rbind(data.frame(Broods3_b), data.frame(Broods3_b_max))

Broods4 <- rbind(data.frame(Broods3_a),data.frame(Broods4_b))

Broods4 <- Broods4 %>% arrange(Nest_ID, Brood_age)

write.csv(Broods4, "Broods_day_desertion4.csv")


### Creating lead event for each chick and female presence

Broods4 <- read.csv("Broods_day_desertion4.csv")

Broods4 <- Broods4 %>% group_by(Nest_ID) %>% mutate(Chick1_pres_lead = 
                                                      lead(Chick1_pres_full, order_by=Nest_ID),
                                                    Chick2_pres_lead = 
                                                      lead(Chick2_pres_full, order_by=Nest_ID),
                                                    Chick3_pres_lead = 
                                                      lead(Chick3_pres_full, order_by=Nest_ID),
                                                    Chick4_pres_lead = 
                                                      lead(Chick4_pres_full, order_by=Nest_ID),
                                                    Chick5_pres_lead = 
                                                      lead(Chick5_pres_full, order_by=Nest_ID),
                                                    Female_care_lead = 
                                                      lead(Female_care_full, order_by=Nest_ID))


# Create chick change and female change variables

Broods4$Chick1_change_lead <- ifelse(Broods4$Chick1_pres_lead < Broods4$Chick1_pres_full, 1, 0)
Broods4$Chick2_change_lead <- ifelse(Broods4$Chick2_pres_lead < Broods4$Chick2_pres_full, 1, 0)
Broods4$Chick3_change_lead <- ifelse(Broods4$Chick3_pres_lead < Broods4$Chick3_pres_full, 1, 0)
Broods4$Chick4_change_lead <- ifelse(Broods4$Chick4_pres_lead < Broods4$Chick4_pres_full, 1, 0)
Broods4$Chick5_change_lead <- ifelse(Broods4$Chick5_pres_lead < Broods4$Chick5_pres_full, 1, 0)

Broods4$Chick_change_nr <- rowSums(Broods4[c("Chick1_change_lead", "Chick2_change_lead", 
                                             "Chick3_change_lead", "Chick4_change_lead",
                                             "Chick5_change_lead")], na.rm = TRUE)

Broods4$Chick_change <- ifelse(Broods4$Chick_change_nr >= 1, 1, 0)

Broods4$Female_change <- ifelse(Broods4$Female_care_lead < Broods4$Female_care_full, 1, 0)

Broods4_ind <- Broods4 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) # 331

# Excluding male desertion broods

male_desertion <- subset(Broods4, Male_dur_care < Female_dur_care)
length(unique(male_desertion$Nest_ID))  #7
Broods4 <- subset(Broods4, Male_dur_care >= Female_dur_care)

Broods4_ind <- Broods4 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) #324

# Exclude broods with only two days of observation (including 3 obs broods with 0 chick presence)

three_obs_broods <- Broods4 %>% group_by(Nest_ID) %>% filter(length(Nest_ID) == 3) %>% filter(!any(Chick_nr2 == 0))
Broods4 <- Broods4 %>% group_by(Nest_ID) %>% filter(length(Nest_ID) > 3)
Broods4 <- rbind(Broods4, three_obs_broods)
Broods4 <- Broods4 %>% arrange(Nest_ID, Brood_age)

Broods4_ind <- Broods4 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) ## 303

# Exclude broods in which chicks were present longer than parents because we don't know which parent deserted

Broods4_temp <- subset(Broods4, Chick_presence_max_day > (Male_dur_care-1) & 
                         Chick_presence_max_age < 26 & Male_dur_care == Female_dur_care)

Broods4_temp_ind <- Broods4_temp[!duplicated(Broods4_temp["Nest_ID"]),]

Broods4_temp_ind$Nest_ID
# 2006_D31  2006_D32  2006_D33  2010_A104

Broods4 <- subset(Broods4, !Nest_ID %in% Broods4_temp_ind$Nest_ID)

Broods4_ind <- Broods4 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) # 299

write.csv(Broods4, "Broods_desertion4.csv")

# filtering for broods where there is max 3 day observation gap at dissappearance of female and chick death

Broods4 <- read.csv("Broods_desertion4.csv")

Broods4_temp <- Broods4

Broods4_temp$missing_day <- ifelse(is.na(Broods4_temp$Date) == TRUE, 
                                  Broods4_temp$Brood_age, NA)

Broods4_temp$missing_day_id <- ifelse(is.na(Broods4_temp$missing_day) == TRUE, 100, 
                                         Broods4_temp$missing_day)

Broods4_temp$missing_day_id2 <- cumsum(c(1, abs(Broods4_temp$missing_day_id[-length(Broods4_temp$missing_day_id)]
                                                   - Broods4_temp$missing_day_id[-1]) > 1))

Broods4_temp$missing_day_id2 <- ifelse(Broods4_temp$missing_day_id == 100, 0, Broods4_temp$missing_day_id2)

Broods4_temp2a <- Broods4_temp %>% filter(missing_day_id2>0) %>% group_by(missing_day_id2) %>%
  filter(length(missing_day_id2)>3) %>% 
  filter(any(Female_change)>0)

Broods4_temp3a <- Broods4_temp2a[!duplicated(Broods4_temp2a["Nest_ID"]),]

# deleting broods in which female desertion fell into a longer than 3 days observation gap 

Broods4_temp3a$Nest_ID

# 2006_A103 2006_B207 2006_B209 2006_C-1  2006_C112 2007_A133 2007_A203 2007_A207 2007_A3   2007_A4   2007_B8   2007_C10 
# 2007_C301 2007_D-1  2007_D110 2007_E101 2007_E302 2007_F101 2008_A9   2008_B101 2008_C102 2008_C103 2008_C109 2008_C112
# 2008_D102 2009_A101 2009_B6   2009_D7   2010_A114 2010_A23  2010_A26  2010_B201 2010_D-2  2010_E103 2011_A10  2011_A5   2011_B1 

Broods5 <- subset(Broods4, !Nest_ID %in% Broods4_temp3a$Nest_ID) 

Broods5_ind <- Broods5 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) #262

write.csv(Broods5, "Broods_desertion5.csv")

### Broods with any estimated female termination of care

Broods4_temp2b <- Broods4_temp %>% filter(missing_day_id2>0) %>% group_by(missing_day_id2) %>%
   filter(length(missing_day_id2)>0) %>%
   filter(any(Female_change)>0)

Broods4_temp3b <- Broods4_temp2b[!duplicated(Broods4_temp2b["Nest_ID"]),]

Broods4_temp3b$Nest_ID

# 2006_A101  2006_A103  2006_A104  2006_B2    2006_B207  2006_B209  2006_B4    2006_B6    2006_C-1   2006_C10
# [11] 2006_C107  2006_C112  2006_C5    2006_C7    2006_D1    2006_D11   2006_D113  2006_D116  2006_D118  2006_D119
# [21] 2006_D2    2006_D202  2006_D34   2006_D4    2006_D6    2006_D7    2007_A1    2007_A110  2007_A113  2007_A119
# [31] 2007_A121  2007_A130  2007_A133  2007_A2    2007_A203  2007_A204  2007_A206  2007_A207  2007_A3    2007_A4
# [41] 2007_A7    2007_A9    2007_B-1   2007_B1    2007_B101  2007_B103  2007_B6    2007_B8    2007_C1    2007_C10
# [51] 2007_C13   2007_C14   2007_C301  2007_C7    2007_C8    2007_D-1   2007_D10   2007_D110  2007_D4    2007_D6
# [61] 2007_E1    2007_E101  2007_E2    2007_E302  2007_E4    2007_F101  2008_A102  2008_A2    2008_A4    2008_A9
# [71] 2008_B101  2008_B103  2008_C102  2008_C103  2008_C109  2008_C112  2008_C114  2008_D102  2008_E101  2008_E102
# [81] 2009_A1    2009_A101  2009_A201  2009_A4    2009_A5    2009_A7    2009_A8    2009_B6    2009_C-101 2009_C1
# [91] 2009_C101  2009_C102  2009_C3    2009_C4    2009_D102  2009_D103  2009_D104  2009_D105  2009_D107  2009_D4
# [101] 2009_E1    2010_A10   2010_A101  2010_A109  2010_A11   2010_A114  2010_A14   2010_A16   2010_A17   2010_A18
# [111] 2010_A20   2010_A21   2010_A23   2010_A26   2010_A28   2010_A30   2010_A5    2010_A6    2010_A9    2010_B201
# [121] 2010_C1    2010_C104  2010_C2    2010_C3    2010_D-2   2010_D1    2010_E-1   2010_E102  2010_E103  2011_A10
# [131] 2011_A104  2011_A106  2011_A108  2011_A12   2011_A16   2011_A2    2011_A3    2011_A5    2011_A7    2011_A8
# [141] 2011_B1    2011_B201  2011_B4    2011_E1    2011_E3    2011_E5

Broods5_no_est <- subset(Broods4, !Nest_ID %in% Broods4_temp3b$Nest_ID) 

Broods5_no_est_ind <- Broods5_no_est %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) #148

write.csv(Broods5_no_est, "Broods_desertion5_no_est.csv")


## Defining type of end of female care

Broods5 <- read.csv("Broods_desertion5.csv")
# Broods5 <- read.csv("Broods_desertion5_no_est.csv")


Broods5$End_of_care <- ifelse(Broods5$Female_dur_care < 26 & 
                                      Broods5$Female_dur_care < Broods5$Chick_presence_max_day, "Desertion", 
                                      ifelse(Broods5$Female_dur_care == 26, "Full term care", "Unfinished"))

Broods5$End_of_care <- ifelse(Broods5$Brood_fate == "died" & Broods5$End_of_care == "Unfinished", "Brood failure",
                                    Broods5$End_of_care)

# Cutting off brood ends at 25 days since this is our focus period

Broods5 <- Broods5 %>% group_by(Nest_ID) %>% filter(Brood_age <= 25)



## Generating unique IDs for adults with missing ID

Broods5_ind <- Broods5 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) # 262

# Males
Broods5_ind$Male_ID <- as.character(Broods5_ind$Male_ID)
sum(is.na(Broods5_ind$Male_ID)) #13 #9 - no est
Broods5_ind$Male_ID[is.na(Broods5_ind$Male_ID)] <- sample(1:13, size=sum(is.na(Broods5_ind$Male_ID)), replace=F)
# Broods5_ind$Male_ID[is.na(Broods5_ind$Male_ID)] <- sample(1:9, size=sum(is.na(Broods5_ind$Male_ID)), replace=F)

# Females
Broods5_ind$Female_ID <- as.character(Broods5_ind$Female_ID)
sum(is.na(Broods5_ind$Female_ID)) #12 #10 - no est
Broods5_ind$Female_ID[is.na(Broods5_ind$Female_ID)] <- sample(14:25, size=sum(is.na(Broods5_ind$Female_ID)), replace=F)
# Broods5_ind$Female_ID[is.na(Broods5_ind$Female_ID)] <- sample(10:19, size=sum(is.na(Broods5_ind$Female_ID)), replace=F)


Broods5$Male_ID <- Broods5_ind$Male_ID[match(Broods5$Nest_ID, Broods5_ind$Nest_ID )]

Broods5$Female_ID <- Broods5_ind$Female_ID[match(Broods5$Nest_ID, Broods5_ind$Nest_ID )]

Broods5$X.5 <- NULL
Broods5$X.4 <- NULL
Broods5$X.3 <- NULL
Broods5$X.2 <- NULL
Broods5$X.1 <- NULL
Broods5$X <- NULL

write.csv(Broods5, "Broods_desertion_final.csv")
# write.csv(Broods5, "Broods_desertion_final_no_est.csv")


#### Creating a chick based data set, in which the unite is a chick

# Reorganise according to chicks

library(dplyr)
Broods5 <- read.csv("Broods_desertion_final.csv")
Captures_Ceuta <- read.csv("Captures_Ceuta_2006-2012 copy.csv")

Broods5_ind <- Broods5 %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))

colnames(Broods5_ind)[which(names(Broods5_ind) == "Chick1_ID")] <- "Chick1_ch_ID"
colnames(Broods5_ind)[which(names(Broods5_ind) == "Chick2_ID")] <- "Chick2_ch_ID"
colnames(Broods5_ind)[which(names(Broods5_ind) == "Chick3_ID")] <- "Chick3_ch_ID"
colnames(Broods5_ind)[which(names(Broods5_ind) == "Chick4_ID")] <- "Chick4_ch_ID"
colnames(Broods5_ind)[which(names(Broods5_ind) == "Chick5_ID")] <- "Chick5_ch_ID"

library(data.table)
setDT(Broods5_ind)
Broods5_ch <- melt(Broods5_ind, id = c("Nest_ID", "Female_ID", "Male_ID", 
                                     "Female_dur_care","Male_dur_care",
                                     "Hatching_date", "Year"), 
                  measure.vars = patterns("_ch_ID", "_surv_age", "_HD", "_Fate"), 
                  value.name = c("Chick_ID", "Chick_surv_age", "Hatching_date_ch", "Fate"), 
                  variable.name = "Chick")

library(tidyr)
Broods5_ch <- Broods5_ch %>% drop_na(Chick_ID)

Broods5_ch <- Broods5_ch %>% drop_na(Chick_surv_age)

Broods5_ch <- data.frame(Broods5_ch)

## Adding morphological measurements from capture files

# Capture age variable
Captures_ch <- Captures_Ceuta[Captures_Ceuta$sex == "J",]
Captures_ch_fst <- Captures_ch %>% group_by(ring) %>% slice(which.min(date))
Captures_ch_fst$hatching_date <- Broods5$Hatching_date[match(Captures_ch_fst$ring, Broods5_ch$Chick_ID)]
Captures_ch_fst <- Captures_ch_fst[!(is.na(Captures_ch_fst$hatching_date == TRUE)),]
Captures_ch_fst$cap_date <- as.Date(paste(Captures_ch_fst$year, Captures_ch_fst$date, sep = "0"), "%Y%m%d")
Broods5_ch$Cap_date <- Captures_ch_fst$cap_date[match(Broods5_ch$Chick_ID, Captures_ch_fst$ring)]
Broods5_ch$Cap_age <- difftime(as.Date(Broods5_ch$Hatching_date_ch),as.Date(Broods5_ch$Cap_date), units = "days" )

## Dataset with no individual chick data - here if the HDs of the Broods5 were not on the same day we cannot always say how long the 
# the Broods5 have survived

Broods5_ch$Chick_weight <- Captures_ch_fst$weight[match(Broods5_ch$Chick_ID, Captures_ch_fst$ring)]
Broods5_ch$Left_tarsus_ch <- Captures_ch_fst$left_tarsus[match(Broods5_ch$Chick_ID, Captures_ch_fst$ring)]
Broods5_ch$Right_tarsus_ch <- Captures_ch_fst$right_tarsus[match(Broods5_ch$Chick_ID, Captures_ch_fst$ring)]
Broods5_ch$Mean_tarsus_ch <- rowMeans(Broods5_ch[c("Right_tarsus_ch","Left_tarsus_ch")], na.rm = TRUE)

Broods5_ch$Chick_weight <- ifelse(abs(Broods5_ch$Cap_age) > 1, NA, Broods5_ch$Chick_weight)
Broods5_ch$Chick_weight <- as.numeric(Broods5_ch$Chick_weight)
Broods5_ch$Mean_tarsus_ch <- ifelse(abs(Broods5_ch$Cap_age) > 1, NA, Broods5_ch$Mean_tarsus_ch)
Broods5_ch$Mean_tarsus_ch<- as.numeric(Broods5_ch$Mean_tarsus_ch)

write.csv(Broods5_ch, "Chicks_desertion.csv")


### Chick condition index calculation

Chicks_ch <- read.csv("Chicks_desertion.csv")

# Creating mean brood weight variable
CH_mean_weight <- Chicks_ch %>% group_by(Nest_ID) %>% filter(!is.na(Chick_weight)) %>% 
  summarise(mean=mean(Chick_weight))
Chicks_ch$Mean_brood_weight <- CH_mean_weight$mean[match(Chicks_ch$Nest_ID, CH_mean_weight$Nest_ID)]

# Creating mean chick and brood tarsus lenght variable
Mean_brood_tarsus <- Chicks_ch %>% group_by(Nest_ID) %>% filter(!is.na(Mean_tarsus_ch)) %>% 
  summarise(mean=mean(Mean_tarsus_ch))
Chicks_ch$Mean_brood_tarsus <- Mean_brood_tarsus$mean[match(Chicks_ch$Nest_ID, Mean_brood_tarsus$Nest_ID)]

# Remove potential measurement error

ggplot(Chicks_ch)  + 
  geom_point(aes(x = Mean_tarsus_ch, y = Chick_weight))

Chicks_ch$Mean_tarsus_ch <- ifelse(Chicks_ch$Mean_tarsus_ch >= 20 ,
                                   NA, Chicks_ch$Mean_tarsus_ch)

Chicks_ch$Chick_weight <- ifelse(Chicks_ch$Chick_weight > 7.5 ,
                                 NA, Chicks_ch$Chick_weight)


# Apply SMA regression

Ex2.res <- lmodel2(log(Chick_weight) ~ log(Mean_tarsus_ch), data=Chicks_ch, "relative", "relative", 99)
Ex2.res
op <- par(mfrow = c(1,2))
plot(Ex2.res, "SMA")
plot(Ex2.res, "RMA")
par(op)

Tarsus_ch_0 <- mean(Chicks_ch$Mean_brood_tarsus, na.rm = TRUE)
b.msa.rob <- 2.1 # SMA slope value of the regression

# SMI.rob <- y * (x.0 / x) ^ b.msa.rob

SMI.rob <- Chicks_ch$Chick_weight * (Tarsus_ch_0 / Chicks_ch$Mean_brood_tarsus) ^ b.msa.rob
SMI.rob <- unlist(SMI.rob)
Chicks_ch$Chick_CI <-  SMI.rob

# Mean brood CI
Mean_brood_CI <- Chicks_ch %>% group_by(Nest_ID) %>% filter(!is.na(Chick_CI)) %>% 
  summarise(mean=mean(Chick_CI))
Chicks_ch$Mean_brood_CI <- Mean_brood_CI$mean[match(Chicks_ch$Nest_ID, Mean_brood_CI$Nest_ID)]

# Max chick CI per brood
Chicks_ch$Chick_CI <- ifelse(is.na(Chicks_ch$Chick_CI), 0, Chicks_ch$Chick_CI)
Chicks_ch <- Chicks_ch %>% group_by(Nest_ID) %>% mutate(Max_chick_CI = max(Chick_CI))
Chicks_ch$Chick_CI <- ifelse(Chicks_ch$Chick_CI == 0, NA, Chicks_ch$Chick_CI)
Chicks_ch$Max_chick_CI <- ifelse(Chicks_ch$Max_chick_CI  == 0, NA, Chicks_ch$Max_chick_CI)

write.csv(Chicks_ch, "Chicks_desertion2.csv")


### Adding "Mean brood CI" and "Max_chick_CI" to Broods_day data set

Chicks_ch <- read.csv("Chicks_desertion2.csv")
Broods5 <- read.csv("Broods_desertion_final.csv")
Broods5 <- read.csv("Broods_desertion_final_no_est.csv")
BirdSOC <- read.csv("BirdSOC.csv")

# Adding manipulation type
BirdSOC$Nest_ID <- paste(BirdSOC$year, paste(BirdSOC$site, BirdSOC$nest, sep = ""), sep = "_")
Broods5$Manip_type <- BirdSOC$type[match(Broods5$Nest_ID, BirdSOC$Nest_ID)]

Broods5$Mean_brood_CI <- Chicks_ch$Mean_brood_CI[match(Broods5$Nest_ID, Chicks_ch$Nest_ID)]

Broods5$Max_chick_CI <- Chicks_ch$Max_chick_CI[match(Broods5$Nest_ID, Chicks_ch$Nest_ID)]

Broods5$Brood_max_size = rowSums(is.na(Broods5[,c("Chick1_ID", "Chick2_ID","Chick3_ID","Chick4_ID","Chick5_ID")]))

Broods5$Brood_max_size <- 5 - Broods5$Brood_max_size

Broods5_no_manip <- subset(Broods5, Manip_type == U)

Broods5$Chick1_change_lead <- NULL
Broods5$Chick2_change_lead <- NULL
Broods5$Chick3_change_lead <- NULL
Broods5$Chick4_change_lead <- NULL
Broods5$Chick5_change_lead <- NULL

Broods5$Chick1_pres_lead <- NULL
Broods5$Chick2_pres_lead <- NULL
Broods5$Chick3_pres_lead <- NULL
Broods5$Chick4_pres_lead <- NULL
Broods5$Chick5_pres_lead <- NULL

write.csv(Broods5, "Broods_desertion_final.csv")
write.csv(Broods5, "Broods_desertion_final_no_est.csv")
write.csv(Broods5_no_manip, "Broods_desertion_final_no_manip.csv")
