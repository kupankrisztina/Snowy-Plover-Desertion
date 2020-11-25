####### Making the best of a bad job? Chick mortality and flexible female brood care in Snowy Plovers ########
####### Kupan et al. 2020 ########

####### Figures #######

install.packages("easypackages")
library(easypackages)

packages("dplyr", "plyr", "ggplot2", "RColorBrewer", 
         "cowplot", "bayesplot", "rstanarm", "ggplot2", "MCMCvis", 
         "tidyr", "dplyr", "reshape2", "data.table", 
         "tidyverse", "patternplot", "png")

Desertion <- read.csv("Broods_desertion_final.csv", header = TRUE)
Desertion_broods <- Desertion %>% group_by(Nest_ID) %>% slice(which.min(Brood_age)) #262
# Desertion <- read.csv("Broods_desertion_final_no_est.csv", header = TRUE)


#### Reproductive success and breeding effort (Figure 2)

Desertion_single_multi <- read.csv("Desertion_single_multi.csv") # from Data analysis file

Desertion_single_multi$type <- ifelse(Desertion_single_multi$type == 0, "Desertion", "Full term care")

library(RColorBrewer)
plot_palette2a <- c(brewer.pal(name = "Set1", n = 9)[2],
                    brewer.pal(name ="Set1", n = 9)[4])

library(cowplot)

R_effort <- ggplot(Desertion_single_multi, aes(x=type, y=Female_total_care_length, fill = type)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=19, position=position_jitter(0.2)) +
  scale_fill_manual(values=plot_palette2a) +  
  theme_minimal()+ 
  theme(legend.position = "none")+
  labs(x = "Parental care strategy",
       y = "Reproductive effort (days)") +
  scale_y_continuous(breaks = seq(20, 80, 10)) +
  theme(axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11),
        axis.text.x = element_text(size=11, face="bold"),
        axis.text.y = element_text(size=11, face="bold"))

R_success <- ggplot(Desertion_single_multi, aes(x=type, y=Fledgelings_total, fill = type)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0.2,width = 0.08, shape=16) +
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Parental care strategy",
       y = "Reproductive_success (# fledged chicks)") +
  scale_fill_manual(values=plot_palette2a) +
  scale_y_continuous(breaks = seq(0, 5, 1)) +
  theme(axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size=11, face="bold"),
        axis.text.y = element_text(size =11, face="bold"))

plot_grid(R_success, R_effort, labels = "AUTO")


####### II.	Predictors of the length of female brood care #######

#### Effect plot of the predictors (Figure 3a)

library(bayesplot)
library(rstanarm)
library(ggplot2)
library(dplyr)
library(MCMCvis)
library(cowplot)

## Original figure with all data and all predictors (Figure 3a)

model_desertion <- readRDS("mod_des_desertion.rds")

model_desertion_mat <- as.matrix(model_desertion, pars = c("b", "sigma_female", "sigma_year")) 

my_labels <- c("Intercept", "Present brood size", "Hatching date", "Chick condition",
                    "Male tarsus length", "Female condition index",
                   "Male condition index", "Brood age", "Brood age squared", "Sigma_female", "Sigma_year")

MCMC_plot <- MCMCplot(model_desertion_mat, 
                      params = c('b', "sigma_female","sigma_year"),
                      ref_ovl = TRUE,
                      xlim = c(-0.5, 2.5),
                      main = 'a)',
                      labels = c(my_labels), 
                      sz_labels = 1,
                      sz_ax = 1,
                      sz_ax_txt = 1,
                      sz_tick_txt = 1)

# Figure without estimated desertion data (Supplemental figure)

model_desertion <- readRDS("mod_des_desertion_no_est.rds")

model_desertion_mat <- as.matrix(model_desertion, pars = c("b", "sigma_female", "sigma_year")) 

my_labels <- c("Intercept", "Present brood size", "Hatching date", "Chick condition",
                    "Male tarsus length", "Female condition index",
                   "Male condition index", "Brood age", "Brood age squared", "Sigma_female", "Sigma_year")

MCMC_plot <- MCMCplot(model_desertion_mat, 
                      params = c('b', "sigma_female","sigma_year"),
                      ref_ovl = TRUE,
                      xlim = c(-0.5, 2.5),
                      main = 'a)',
                      labels = c(my_labels), 
                      sz_labels = 1,
                      sz_ax = 1,
                      sz_ax_txt = 1,
                      sz_tick_txt = 1)

# Figures with manipulation as factor (i) and without all manipulated broods (ii) - (Supplemental figure)

# i)
model_desertion <- readRDS("mod_des_desertion_manip.rds")

model_desertion_mat <- as.matrix(model_desertion, pars = c("b", "sigma_female", "sigma_year")) 

my_labels <- c("Intercept", "Present brood size", "Hatching date", "Chick condition",
                    "Male tarsus length", "Female condition index",
                   "Male condition index", "Brood age", "Brood age squared", "Manipulation", "Sigma_female", "Sigma_year")

MCMC_plot <- MCMCplot(model_desertion_mat, 
                      params = c('b', "sigma_female","sigma_year"),
                      ref_ovl = TRUE,
                      xlim = c(-0.5, 2.5),
                      main = 'b)',
                      labels = c(my_labels), 
                      sz_labels = 1,
                      sz_ax = 1,
                      sz_ax_txt = 1,
                      sz_tick_txt = 1)

# ii)
model_desertion <- readRDS("mod_des_desertion_no_manip.rds")

model_desertion_mat <- as.matrix(model_desertion, pars = c("b", "sigma_female", "sigma_year")) 

my_labels <- c("Intercept", "Present brood size", "Hatching date", "Chick condition",
                    "Male tarsus length", "Female condition index",
                   "Male condition index", "Brood age", "Brood age squared", "Sigma_female", "Sigma_year")

MCMC_plot <- MCMCplot(model_desertion_mat, 
                      params = c('b', "sigma_female","sigma_year"),
                      ref_ovl = TRUE,
                      xlim = c(-0.5, 2.5),
                      main = 'b)',
                      labels = c(my_labels), 
                      sz_labels = 1,
                      sz_ax = 1,
                      sz_ax_txt = 1,
                      sz_tick_txt = 1)


## Effect plots for individual predictors (Figure 3b,c,d)

line = 1
cex = 0.8
side = 3
adj=-0.05
par(mfrow=c(1,3), oma=c(1,6,1,1))

bsim <- as.data.frame(model_desertion)
nsim <- nrow(bsim)

# Hatching date (b)

newdat1 <- data.frame(RHD = seq(min(Desertion$Relative_hatching_date), max(Desertion$Relative_hatching_date), length=100))
Xmat <- model.matrix(~RHD, data=newdat1)
fitmat1 <- matrix(ncol=nsim, nrow=nrow(newdat1))
for(i in 1:nsim) fitmat1[,i] <- plogis(Xmat%*%as.numeric(bsim[i,c(1,3)]))
newdat1$fit <- apply(fitmat1, 1, median)
newdat1$lwr <- apply(fitmat1, 1, quantile, prob=0.025)
newdat1$upr <- apply(fitmat1, 1, quantile, prob=0.975)

plot(newdat1$RHD, newdat1$fit, ylim=range(c(0,1)), type="l",
     las=1, ylab="Probability of care", xlab="Hatching date")
lines(newdat1$RHD, newdat1$lwr, lty=3)
lines(newdat1$RHD, newdat1$upr, lty=3)
mtext("b)", side=side, line=line, cex=cex, adj=adj)

# Current brood size (c)

newdat2 <- data.frame(CBS = 1:5)
newdat2$CBS.sc <- (newdat2$CBS-mean(1:5))/sd((1:5))
Xmat2 <- model.matrix(~CBS.sc, data=newdat2)
fitmat2 <- matrix(ncol=nsim, nrow=nrow(newdat2))
for(i in 1:nsim) fitmat2[,i] <- plogis(Xmat2%*%as.numeric(bsim[i,c(1,2)]))
newdat2$fit <- apply(fitmat2, 1, median)
newdat2$lwr <- apply(fitmat2, 1, quantile, prob=0.025)
newdat2$upr <- apply(fitmat2, 1, quantile, prob=0.975)

plot(newdat2$CBS, newdat2$fit,type="n",ylim=range(c(0, 1)),
     las=1, ylab= "", xlab="Present brood size")
axis(1,1:5)
segments(newdat2$CBS,newdat2$lwr,newdat2$CBS,newdat2$upr)
points(newdat2$CBS,newdat2$fit,pch=21,bg="gray")
mtext("c)", side=side, line=line, cex=cex, adj=adj)

# Brood age (d)

newdat3 <- data.frame(age = seq(0,25, length=100))
newdat3$age.sc <- (newdat3$age-mean(0:25))/sd((0:25))
Xmat <- model.matrix(~age.sc+I(age.sc^2), data=newdat3) 
fitmat3 <- matrix(ncol=nsim, nrow=nrow(newdat3))
for(i in 1:nsim) fitmat3[,i] <- plogis(Xmat%*%as.numeric(bsim[i,c(1,8,9)]))
newdat3$fit <- apply(fitmat3, 1, median)
newdat3$lwr <- apply(fitmat3, 1, quantile, prob=0.025)
newdat3$upr <- apply(fitmat3, 1, quantile, prob=0.975)

plot(newdat3$age, newdat3$fit, ylim=range(c(newdat3$lwr,newdat3$upr)), type="l",
     las=1,ylab= "", xlab="Brood age (days)")
lines(newdat3$age, newdat3$lwr, lty=3)
lines(newdat3$age, newdat3$upr, lty=3)
mtext("d)", side=side, line=line, cex=cex, adj=adj)

####### III.	Termination of care and chick mortality #######

## Preparing the data set with the chick that died (or fledged) closest to the female termination of care event 

Chicks_ch <- read.csv("Chicks_desertion2.csv")
Desertion <- read.csv("Broods_desertion_final.csv", header = TRUE)

library(tidyr)
library(dplyr)

# Keeping only the broods which are also in the Desertion data set

Chicks_ch$Nest_ID <- Desertion$Nest_ID[match(Chicks_ch$Nest_ID, Desertion$Nest_ID)]
Chicks_ch <- Chicks_ch %>% drop_na(Nest_ID)
length(unique(Chicks_ch$Nest_ID))

# Exclude "Unfinished" broods
Chicks_ch$End_of_care <- Desertion$End_of_care[match(Chicks_ch$Nest_ID, Desertion$Nest_ID)]
Chicks_ch_fin <- subset(Chicks_ch, End_of_care != "Unfinished")

# Excluding broods in which the fate of the last chick is unknown

Exc_broods <- Chicks_ch_fin %>% group_by(Nest_ID) %>% slice(which.max(Chick_surv_age)) %>% filter(Fate == "unknown")

Exc_broods$Nest_ID #35

Chicks_ch_fin <- subset(Chicks_ch_fin, !Nest_ID %in% Exc_broods$Nest_ID)

## Calculate day difference between female termination of care and chick death of fledging event
# Based on Chicks surv age

Female_last_obs <- Desertion %>% group_by(Nest_ID) %>% filter(Female_care_full == "1") %>%
  dplyr::slice(which.max(Brood_age))
Chicks_ch_fin$Chick_last_day_orig <- as.Date(Chicks_ch_fin$Hatching_date_ch) + (ceiling(Chicks_ch_fin$Chick_surv_age) - 1)
Chicks_ch_fin$Chick_death_age_orig <- round(as.numeric(as.character(difftime(Chicks_ch_fin$Chick_last_day, Chicks_ch_fin$Hatching_date, units = "days") + 1)))

# Time diff between female last day and chick last day
Chicks_ch_fin$Time_diff <- difftime(as.Date(Chicks_ch_fin$Chick_last_day), as.Date(Chicks_ch_fin$Female_last_day), units = "day")

# Find the chick with the closest death or fledging event to the female termination of care event
Chicks_ch_fin2 <- Chicks_ch_fin %>% group_by(Nest_ID) %>% slice(which.min(abs(Time_diff))) #170


#### Preparing corrected data set (last seen for chick death instead of surv age) ####

## Calculate day difference between female termination of care and chick death of fledging event
# Based on Chicks last day 

Chick1_last_obs <- Desertion %>% group_by(Nest_ID) %>% filter(Chick1_pres_full == "1") %>%
  dplyr::slice(which.max(Brood_age))

Chick2_last_obs <- Desertion %>% group_by(Nest_ID) %>% filter(Chick2_pres_full == "1") %>%
  dplyr::slice(which.max(Brood_age))

Chick3_last_obs <- Desertion %>% group_by(Nest_ID) %>% filter(Chick3_pres_full == "1") %>%
  dplyr::slice(which.max(Brood_age))

Chick4_last_obs <- Desertion %>% group_by(Nest_ID) %>% filter(Chick4_pres_full == "1") %>%
  dplyr::slice(which.max(Brood_age))

Chick5_last_obs <- Desertion %>% group_by(Nest_ID) %>% filter(Chick5_pres_full == "1") %>%
  dplyr::slice(which.max(Brood_age))

Chicks_ch_fin$Chick_last_day <- Chick1_last_obs$Date2[match(Chicks_ch_fin$Chick_ID,
                                                            Chick1_last_obs$Chick1_ID)]

Chicks_ch_fin$Chick_last_day2 <- Chick2_last_obs$Date2[match(Chicks_ch_fin$Chick_ID,
                                                             Chick2_last_obs$Chick2_ID)]
Chicks_ch_fin$Chick_last_day <- ifelse(is.na(Chicks_ch_fin$Chick_last_day)==TRUE,
                                       Chicks_ch_fin$Chick_last_day2, Chicks_ch_fin$Chick_last_day)

Chicks_ch_fin$Chick_last_day2 <- Chick3_last_obs$Date2[match(Chicks_ch_fin$Chick_ID,
                                                             Chick3_last_obs$Chick3_ID)]
Chicks_ch_fin$Chick_last_day <- ifelse(is.na(Chicks_ch_fin$Chick_last_day)==TRUE,
                                       Chicks_ch_fin$Chick_last_day2, Chicks_ch_fin$Chick_last_day)

Chicks_ch_fin$Chick_last_day2 <- Chick4_last_obs$Date2[match(Chicks_ch_fin$Chick_ID,
                                                             Chick4_last_obs$Chick4_ID)]
Chicks_ch_fin$Chick_last_day <- ifelse(is.na(Chicks_ch_fin$Chick_last_day)==TRUE,
                                       Chicks_ch_fin$Chick_last_day2, Chicks_ch_fin$Chick_last_day)

Chicks_ch_fin$Chick_last_day2 <- Chick5_last_obs$Date2[match(Chicks_ch_fin$Chick_ID,
                                                             Chick5_last_obs$Chick5_ID)]
Chicks_ch_fin$Chick_last_day <- ifelse(is.na(Chicks_ch_fin$Chick_last_day)==TRUE,
                                       Chicks_ch_fin$Chick_last_day2, Chicks_ch_fin$Chick_last_day)

Chicks_ch_fin$Chick_last_day2 <- NULL

Chicks_ch_fin$Female_last_day <- Female_last_obs$Date2[match(Chicks_ch_fin$Nest_ID,
                                                             Female_last_obs$Nest_ID)]

# Time diff between female last day and chick last day
Chicks_ch_fin$Time_diff_orig <- difftime(as.Date(Chicks_ch_fin$Chick_last_day_orig), 
                                         as.Date(Chicks_ch_fin$Female_last_day), units = "day")

Chicks_ch_fin$Time_diff <- difftime(as.Date(Chicks_ch_fin$Chick_last_day), 
                                    as.Date(Chicks_ch_fin$Female_last_day), units = "day")

# Find the chick with the closest death or fledging event to the female termination of care event

Chicks_ch_fin2 <- Chicks_ch_fin %>% group_by(Nest_ID) %>% dplyr::slice(which.min(abs(Time_diff))) #170
# Chicks_ch_fin2 <- subset(Chicks_ch_fin2, Fate != "unknown") 


#### Co-occurrence of chick mortality and female termination of care barplot (Figure 4)

## Prepare frequencies of time differences  

# Exclude full term care providing females

Chicks_ch_fin3 <- subset(Chicks_ch_fin2, End_of_care != "Full term care")

library(reshape2)
chick_diff_freq <-  dcast(data=Chicks_ch_fin3,
                          Time_diff ~ End_of_care,
                          fun.aggregate = length,
                          value.var = "End_of_care")

library(data.table)
setDT(chick_diff_freq )
chick_diff_freq <- melt(chick_diff_freq, id.vars =  "Time_diff",
                        measure.vars =  c("Brood failure", "Desertion"),
                        value.name = c("count"),
                        variable.name = c("End_of_care"))
chick_diff_freq <- as.data.frame(chick_diff_freq)
chick_diff_freq$group <- ifelse(chick_diff_freq$Time_diff <= 0, "During female care", "After female care")
chick_diff_freq$group <- as.factor(chick_diff_freq$group)

library(MASS)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(patternplot)
library(png)

plot_palette2 <- c(brewer.pal(name = "Set1", n = 9)[5],
                   brewer.pal(name ="Set1", n = 9)[2])

ggplot(chick_diff_freq, aes(Time_diff, count, fill = End_of_care, 
                            color = End_of_care, alpha = group)) +
  geom_bar(stat = "identity", color = "black")+
  scale_alpha_discrete(range =c(0.4,1)) +
  guides(alpha = FALSE) +
  scale_fill_manual(values =  plot_palette2) +
  theme_minimal() +
  labs(x = "Co-occurence interval (days)",
       y = "Count of broods",
       fill = "End of care",
       alpha = "") +
  scale_x_continuous(breaks = seq(-25, 25, 5))+
  scale_y_continuous(breaks = seq(0, 50, 5))+
  theme(axis.title.x = element_text(size=12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size =12),
        legend.title = element_text(size=12,  face = "bold"),
        legend.text = element_text(size = 12))


#### Co-occurrence of chick death or fledging and female termination of care - scatterplot (Figure 5)

Chicks_ch_fin2$Chick_fate <- ifelse(Chicks_ch_fin2$Fate == "fledged", "Fledged","Died")
Chicks_ch_fin2$Chick_fate <- factor(Chicks_ch_fin2$Chick_fate, levels = c("Fledged","Died"))

library(RColorBrewer)
plot_palette3b <- c(brewer.pal(name = "Set1", n = 9)[5],
                    brewer.pal(name = "Set1", n = 9)[2],
                    brewer.pal(name = "Set1", n = 9)[4])

ggplot(Chicks_ch_fin2, aes(Female_dur_care-1, Chick_death_age-1, color = End_of_care, alpha = Chick_fate))  + 
  geom_count() +
  geom_abline(color = "black")+
  scale_alpha_manual(values = c(0.4, 1))+
  scale_color_manual(values=c(plot_palette3b))+
  guides(color = guide_legend(order = 1, override.aes = list(size=3)),
         alpha = guide_legend(order = 2, override.aes = list(size=3))) +
  theme_minimal()+ 
  labs(x = "End of female care",
       y = "Chick final age (days)",
       color = "End of care",
       alpha = "Chick fate") +
  scale_x_continuous(breaks = seq(0, 25, 5))+
  scale_y_continuous(breaks = seq(0, 25, 5))+
  theme(axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size =12))


#### Co-occurrence of chick death or fledging and female termination of care by current brood size - scatterplot (Figure 6)

library(dplyr)
Desertion_short_fin <- read.csv("Desertion_short_fin.csv")

Chicks_ch_fin2$Chick_nr2_des <- Desertion_short_fin$Chick_nr2_des[match
                                                (Chicks_ch_fin2$Nest_ID, Desertion_short_fin$Nest_ID)]
Chicks_ch_fin2$Chick_nr2_des <- ifelse(Chicks_ch_fin2$Chick_nr2_des <= 3, 
                                                Chicks_ch_fin2$Chick_nr2_des, "4-5")
Chicks_ch_fin2$Chick_nr2_des <- as.factor(Chicks_ch_fin2$Chick_nr2_des)

ggplot(Chicks_ch_fin2, aes(Female_dur_care-1, Chick_death_age-1, color = End_of_care, alpha = Chick_fate)) + 
  geom_count() +
  scale_shape_manual(values=c(1, 19))+
  scale_alpha_manual(values = c(0.4, 1))+
  theme_minimal()+
  guides(color = guide_legend(order = 1, override.aes = list(size=2)),
         alpha = guide_legend(order = 2, override.aes = list(size=2))) +
  facet_wrap(~Chick_nr2_des, ncol = 2) +
  geom_abline(color = "grey")+
  scale_color_manual(values=plot_palette3b)+
  theme( strip.background = element_rect(colour= "white", fill="grey90"),
         strip.text.x = element_text(size=10, face="bold"))+
  labs(x = "End of female care (days)",
       y = "Chick final age (days)",
       color = "End of care",
       alpha = "Chick fate") +
  scale_x_continuous(breaks = seq(0, 25, 5))+
  scale_y_continuous(breaks = seq(0, 25, 5))+
  theme(axis.title.x = element_text(size=11, face = "bold"),
        axis.title.y = element_text(size=11, face = "bold"),
        legend.title = element_text(size=11, face = "bold"),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size =11))
