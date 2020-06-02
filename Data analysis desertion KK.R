####### Making the best of a bad job? Chick mortality and flexible female brood care in Snowy Plovers ########
####### Kupan et al. 2019 ########

Desertion <- read.csv("Broods_desertion_final.csv", header = TRUE) 

# Desertion <- read.csv("Broods_desertion_final_no_est.csv", header = TRUE)

####### I.	Female brood care #######

library(dplyr)
Desertion_ind <- Desertion %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) # 262

detach(package:dplyr, unload=TRUE)
library(plyr)
count(Desertion_ind, 'End_of_care')

# End_of_care freq
# 1                Brood failure   15
# 2                    Desertion  185
# 3               Full term care    5
# 4                   Unfinished   57


### Reproductive success and breeding effort - Supplemental Material

# Females that breeded multiple times a year

## Create "Desertion" group

Desertion$Chick1_fledged <- ifelse(Desertion$Chick1_surv_age == 26, 1,0)
Desertion$Chick2_fledged <- ifelse(Desertion$Chick2_surv_age == 26, 1,0)
Desertion$Chick3_fledged <- ifelse(Desertion$Chick3_surv_age == 26, 1,0)
Desertion$Chick4_fledged <- ifelse(Desertion$Chick4_surv_age == 26, 1,0)
Desertion$Chick5_fledged <- ifelse(Desertion$Chick5_surv_age == 26, 1,0)

Desertion[,"Fledgelings"] <- apply(Desertion[, c("Chick1_fledged","Chick2_fledged",
                                                        "Chick3_fledged","Chick4_fledged",
                                                        "Chick5_fledged")], 1, sum, na.rm = TRUE)


detach(package:plyr, unload=TRUE)
library(dplyr)
Desertion_ind <- Desertion %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
Desertion_rep_fm <- Desertion_ind %>% group_by(Year, Female_ID) %>% filter(n() > 1)

# Order of breeding events
Desertion_rep_fm <- Desertion_rep_fm %>%  
  arrange(Year, Female_ID, Relative_hatching_date) %>% group_by(Year, Female_ID) %>% 
  mutate(Female_order = rank(Relative_hatching_date))

Desertion_rep_fm <- Desertion_rep_fm %>% group_by(Year, Female_ID) %>% mutate(Fledgelings_total =
                                                                                sum(Fledgelings))

Desertion_rep_fm <- Desertion_rep_fm %>% group_by(Year, Female_ID) %>% mutate(first_hatching_date = min(as.Date(Hatching_date)))
Desertion_rep_fm <- Desertion_rep_fm %>% group_by(Year, Female_ID) %>% mutate(last_hatching_date = max(as.Date(Hatching_date)))

# Keep only the females that deserted the first brood
Desertion_rep_fm <- Desertion_rep_fm[!(Desertion_rep_fm$Female_order == 1 & Desertion_rep_fm$End_of_care != "Desertion"),]

# Remove broods with unknown brood fates
Desertion_rep_fm <- subset(Desertion_rep_fm, Brood_fate != "unknown")

# Keep females that still had multiple breeding events

Desertion_rep_fm <- Desertion_rep_fm %>% group_by(Year, Female_ID) %>% filter(n() > 1)

Desertion_rep_fm <- subset(Desertion_rep_fm, Female_order == 2)

Desertion_rep_fm$Female_total_care_length <- difftime(as.Date(Desertion_rep_fm$last_hatching_date),
                                                          as.Date(Desertion_rep_fm$first_hatching_date), 
                                                          units = "days") + 28 + Desertion_rep_fm$Female_dur_care

## Create "Full term care" group

Female_full_care <- Desertion_ind %>% group_by(Female_ID) %>% filter(End_of_care == "Full term care")

Female_full_care$Fledgelings_total <- Female_full_care$Fledgelings

Female_full_care$Female_total_care_length <- Female_full_care$Female_dur_care + 28


## Analyses

# Desertion group
Desertion_multi <- Desertion_rep_fm[,c("Female_ID", "Fledgelings_total", "Female_total_care_length", "End_of_care")]
Desertion_multi$type = 0

# Full term care group
Desertion_single <- Female_full_care[,c("Female_ID", "Fledgelings_total", "Female_total_care_length", "End_of_care")]
Desertion_single$type <- 1

#Joint data set
Desertion_single_multi <- rbind(data.frame(Desertion_single), data.frame(Desertion_multi))

write.csv(Desertion_single_multi, "Desertion_single_multi.csv")

### Models and distribution test

## Length of care

library(car)
library(MASS)

# Normal
qqp(Desertion_single_multi$Female_total_care_length[Desertion_single_multi$type == 0], "norm")
qqp(Desertion_single_multi$Female_total_care_length[Desertion_single_multi$type == 1], "norm")

# Lognormal
qqp(Desertion_single_multi$Female_total_care_length[Desertion_single_multi$type == 0], "lnorm")
qqp(Desertion_single_multi$Female_total_care_length[Desertion_single_multi$type == 1], "lnorm")

lm_single_multi <- lm(Female_total_care_length ~ type, data = Desertion_single_multi)
summary(lm_single_multi)
confint(lm_single_multi)

# Model testing

hist(resid(lm_single_multi))
qqnorm(resid(lm_single_multi))
qqline(resid(lm_single_multi))
plot(resid(lm_single_multi)~Desertion_single_multi$type)


## Reproductive success: Glm - Poisson

glm_single_multi_rep <- glm(Fledgelings_total ~ type, data = Desertion_single_multi, family = 'poisson')
summary(glm_single_multi_rep)
confint(glm_single_multi_rep)
anova(glm_single_multi_rep, test="Chisq")

# Test of fit

library(MASS)
library(grid)
library(vcd)

fit0 <- goodfit(Desertion_single_multi$Fledgelings_total[Desertion_single_multi$type == 0], "poisson") 
summary(fit0) 
rootogram(fit0)
Ord_plot(Desertion_single_multi$Fledgelings_total[Desertion_single_multi$type == 0])
distplot(desertion_single_multi$Fledgelings_total[Desertion_single_multi$type == 0])

fit1 <- goodfit(Desertion_single_multi$Fledgelings_total[Desertion_single_multi$type == 1], "poisson") 
summary(fit1) 
rootogram(fit1)
Ord_plot(Desertion_single_multi$Fledgelings_total[Desertion_single_multi$type == 1])
distplot(Desertion_single_multi$Fledgelings_total[Desertion_single_multi$type == 1])

# Testing for overdispersion - no overdispersion

library(AER)
deviance(glm_single_multi_rep)/glm_single_multi_rep$df.residual
dispersiontest(glm_single_multi_rep)

library(car)
influencePlot(mod2)

# Zero inflation test

library(pscl)
mod1 <- zeroinfl(fm_rep_sum ~ type, data = Desertion_single_multi, dist="poisson")
AIC(mod1)



####### II. Predictors of the length of female brood care #######

library(dplyr)
library(car)
library(Matrix)
library(lme4)
library(MASS)
library(mlmRev)
library(rstan)
library(rstanarm)
library(arm)
library(shinystan)
library(reshape2)

# Presence/absence matrix

Desertion <- read.csv("Broods_desertion_final.csv")

y_des<- Desertion[,c("Nest_ID", "Female_care_full", "Brood_age")]
y_des <- acast(y_des, Nest_ID ~ Brood_age, value.var="Female_care_full")
y_des <- y_des[,-27]

# Determining last days - always the last observation day with 0 if female was not there and 1 if it was there

last_1 <- data.frame(rowSums(y_des==1, na.rm=TRUE))
last_0 <- data.frame(rowSums(y_des==0, na.rm=TRUE))
colnames(last_1)[which(names(last_1) == "rowSums.y_des....1..na.rm...TRUE.")] <- "sum_1"
colnames(last_0)[which(names(last_0) == "rowSums.y_des....0..na.rm...TRUE.")] <- "sum_0"

last_des_1_0 <- cbind(last_1, last_0)
last_des_df <- as.data.frame(last_des_1_0)
last_des_df$Nest_ID = rownames(last_des_df)
rownames(last_des_df) = NULL
last_des_df <- last_des_df %>% mutate(last = if_else(last_des_df$sum_0 == 0, last_des_df$sum_1, last_des_df$sum_1 + 1))
last_des <- as.list(last_des_df["last"])
last_des <- unlist(last_des)

y_des[is.na(y_des) == TRUE] <- 0

max_age_des <- 26

Nests_des <- length(last_des_df$last)

# Current brood size matrix

Desertion$Chick_nr2_sc <- as.numeric(scale(Desertion$Chick_nr2))
CBS_des <- Desertion[,c("Nest_ID", "Chick_nr2_sc", "Brood_age")]
CBS_des <- acast(CBS_des, Nest_ID ~ Brood_age, value.var="Chick_nr2_sc")
CBS_des[is.na(CBS_des) == TRUE] <- 0
CBS_des <- CBS_des[,-27]

# Brood age list

BA_des <- as.list(round(scale(0:25), digits = 2))
BA_des <- unlist(BA_des)

# Relative hatching date list

RHD_des <- Desertion[,c("Nest_ID", "Relative_hatching_date", "Brood_age")]
RHD_des <- RHD_des %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
RHD_des <- as.list(RHD_des[,"Relative_hatching_date"])
RHD_des <- unlist(RHD_des)

# Male tarsus mean length list (NAs are substituted with the mean)

MTM_des <- Desertion[,c("Nest_ID", "M_tarsus_mean", "Brood_age")]
MTM_des <- MTM_des %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
MTM_des <- as.list(MTM_des["M_tarsus_mean"])
MTM_des <- unlist(MTM_des)
MTM_des[ is.na(MTM_des) ] <- mean(MTM_des, na.rm = TRUE)
MTM_des <- as.numeric(scale(MTM_des))
MTM_des <- unlist(MTM_des)

# M_condition

MCI <- Desertion[,c("Nest_ID", "Male_CI", "Brood_age")]
MCI <- MCI %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
MCI <- as.list(MCI["Male_CI"])
MCI <- unlist(MCI)
MCI[ is.na(MCI) ] <- mean(MCI, na.rm = TRUE)
MCI <- as.numeric(scale(MCI))
MCI <- unlist(MCI)

# F_condition

FCI <- Desertion[,c("Nest_ID", "Female_CI", "Brood_age")]
FCI <- FCI %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
FCI <- as.list(FCI["Female_CI"])
FCI <- unlist(FCI)
FCI[ is.na(FCI) ] <- mean(FCI, na.rm = TRUE)
FCI <- as.numeric(scale(FCI))
FCI <- unlist(FCI)


# CH_condition_max

CH_CI_max <- Desertion[,c("Nest_ID", "Max_chick_CI", "Brood_age")]
CH_CI_max <- CH_CI_max %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
CH_CI_max <- as.list(CH_CI_max["Max_chick_CI"])
CH_CI_max <- unlist(CH_CI_max)
CH_CI_max[ is.na(CH_CI_max) ] <- mean(CH_CI_max, na.rm = TRUE)
CH_CI_max <- as.numeric(scale(CH_CI_max))
CH_CI_max <- unlist(CH_CI_max)


# Manip

Desertion$Manip_num  <- ifelse(Desertion$Manipulation == "M", 1, 0)
Manip <- Desertion[,c("Nest_ID", "Manip_num", "Brood_age")]
Manip <- Manip %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
Manip <- as.list(Manip["Manip_num"])
Manip <- unlist(Manip)


## Female ID as numeric string - random variable

Desertion$Female_ID <- as.factor(Desertion$Female_ID)
levels(Desertion$Female_ID) <- 1:length(levels(Desertion$Female_ID))
Desertion$Female_ID <- as.numeric(Desertion$Female_ID)

Female_ID <- Desertion[,c("Nest_ID", "Female_ID", "Brood_age")]
Female_ID <- Female_ID %>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
Female_ID <- as.list(Female_ID["Female_ID"])
Female_ID <- as.numeric(unlist(Female_ID))

NFemale <- as.numeric(length(unique(Female_ID)))

## Year as numeric string - random variable

Desertion$Year <- as.factor(Desertion$Year)
levels(Desertion$Year) <- 1:length(levels(Desertion$Year))
Desertion$Year <- as.numeric(Desertion$Year)

Year <- Desertion[,c("Nest_ID", "Year", "Brood_age")]
Year <- Year%>% group_by(Nest_ID) %>% slice(which.min(Brood_age))
Year <- as.list(Year["Year"])
Year <- as.integer(unlist(Year))

NYears <- 7


datax_des <- c("y_des", "Nests_des", "last_des", "max_age_des", "CBS_des","BA_des",
               "RHD_des",   "CH_CI_max", "FCI","MTM_des", "MCI", "NFemale",
               "Female_ID", "Year", "NYears") # "Manip" 


## Model

mod_desertion <- stan("Model_desertion_Kupan.stan", data=datax_des, 
                      chains=5,warmup = 2000, iter=4000, control=list(adapt_delta=0.99,
                                                                      max_treedepth = 11), init = "random")


saveRDS(mod_desertion, "mod_des_desertion.rds")
# saveRDS(mod_desertion, "mod_des_desertion_no_est.rds")
# saveRDS(mod_desertion, "mod_des_desertion_manip.rds")

print(mod_desertion, pars = c("b", "sigma_female", "sigma_year"), probs = c(0.025, 0.5, 0.975))

launch_shinystan(mod_desertion)

pairs(mod_desertion, pars = c("b", "sigma_year", "sigma_year"))



####### III.	Termination of care and chick mortality #######

library(dplyr)
library(survival)

## All broods without Unfinished ones

# Cut brood age at end of female care

Desertion_short <- subset(Desertion, Female_care_full == 1)

# Remove unfinished broods

Desertion_short_fin <- subset(Desertion_short, End_of_care != "Unfinished")

# Model

model_cooc <- clogit(Female_change ~ Chick_change + strata(Nest_ID),
                                  data = Desertion_short_fin)
summary(model_cooc)

xtabs(~ Female_change + Chick_change, data = Desertion_short_fin)

length(unique(Desertion_short_fin$Nest_ID)) # 205

## Only deserted broods

Desertion_short_fin_des <- subset(Desertion_short_fin, End_of_care == "Desertion")

# Model

model_cooc_des <- clogit(Female_change ~ Chick_change + strata(Nest_ID),
                                  data = Desertion_short_fin_des)
summary(model_cooc_des)

xtabs(~ Female_change + Chick_change, data = Desertion_short_fin_des)


## Deserted broods without current brood size = 1

Desertion_short_fin_des2 <- subset(Desertion_short_fin_des, Chick_nr2 != 1)

# Model

model_cooc_des2 <- clogit(Female_change ~ Chick_change + strata(Nest_ID),
                         data = Desertion_short_fin_des2)
summary(model_cooc_des2)

xtabs(~ Female_change + Chick_change, data = Desertion_short_fin_des2)


## All broods without Unfinished ones - according to current brood size at termination of care - Supplemental Material

Desertion_short_fin$Chick_nr2_des <- ifelse(Desertion_short_fin$Female_change == 1, Desertion_short_fin$Chick_nr2,
                                                  0)
library(dplyr)
Desertion_short_fin <- Desertion_short_fin %>% group_by(Nest_ID) %>% mutate(Chick_nr2_des = max(Chick_nr2_des))
Desertion_short_fin <- Desertion_short_fin %>% group_by(Nest_ID) %>% mutate(min_brood_size = min(Chick_nr2))
Desertion_short_fin$Chick_nr2_des <- ifelse(Desertion_short_fin$End_of_care == "Full term care", Desertion_short_fin$min_brood_size,
                                                  Desertion_short_fin$Chick_nr2_des)
Desertion_short_fin$min_brood_size <- NULL
write.csv(Desertion_short_fin, "Desertion_short_fin.csv")

# Current brood size 1

one_chick <- subset(Desertion_short_fin, Chick_nr2_des == 1)

one_chick <- one_chick %>% arrange(Nest_ID, Brood_age)

one_chick_model <- clogit(Female_change ~ Chick_change + strata(Nest_ID),
                                data = one_chick)

summary(one_chick_model)

xtabs(~ Female_change + Chick_change, data = one_chick)

length(unique(one_chick$Nest_ID)) #49

# Current brood size 2

two_chick <- subset(Desertion_short_fin, Chick_nr2_des == 2)

two_chick_model <- clogit(Female_change ~ Chick_change + strata(Nest_ID),
                                data = two_chick)

summary(two_chick_model)

xtabs(~ Female_change + Chick_change, data = two_chick)

length(unique(two_chick$Nest_ID)) #93

# Current brood size 3

three_chick <- subset(Desertion_short_fin, Chick_nr2_des == 3)

three_chick_model <- clogit(Female_change ~ Chick_change + strata(Nest_ID),
                                  data = three_chick)
summary(three_chick_model)

xtabs(~ Female_change + Chick_change, data = three_chick)

length(unique(three_chick$Nest_ID)) #58

## 4-5 chicks broods
# Female care end

four_chick <- subset(Desertion_short_fin, Chick_nr2_des > 3)

four_chick_brood_model <- clogit(Female_change ~ Chick_change+ strata(Nest_ID),
                                 data = four_chick)
summary(four_chick_brood_model)

xtabs(~ Female_change + Chick_change, data = four_chick)

length(unique(four_chick$Nest_ID)) #5



