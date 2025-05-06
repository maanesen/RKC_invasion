#clear memory
rm(list = ls())

######## Load package to enable R to read excel tables ##########################
pacman::p_load(
  tidyverse,    # data management packages 
  readxl        # handle Excel data
)

data <- read_excel("data_RKC_all_stakeholders_NEW.xlsx")

######### extracting subsets ##########

data <- subset(data, data$seaurch < 8)
data <- subset(data, data$omradeD < 5)
data_public <- subset(data, data$stakeholder == 3)

###############################################################################

summary(data)
names(data)
attach(data)

######### socio-demographics; input to table A1 ########
mean(AGE, na.rm=TRUE)


################### MIGRATION #################################

############### Testing mean score and variance ################################
as.numeric(data$omradeA)
as.numeric(data$omradeB)
as.numeric(data$omradeC)
as.numeric(data$omradeD)

mean(data$omradeA)
mean(data$omradeD[stakeholder== "1"])
mean(data$omradeD[stakeholder== "2"])
mean(data_public$omradeD)

var(data$omradeD[stakeholder== "1"])
var(data$omradeD[stakeholder== "2"])
var(data_public$omradeD)

t.test(data$omradeD[stakeholder== "1"], data$omradeD[stakeholder== "2"])
t.test(data_public$omradeD, data$omradeD[stakeholder== "1"])


############# Testing independence of answers to area-question, Pearson's chi-square and Fisher's exact tests ##############
areaA_R_F <- as.matrix(c(2,2,8,6,1,4,4,8,7,0))
dim(areaA_R_F)<-c(5, 2)
chisq.test(areaA_R_F, correct=F)
fisher.test(areaA_R_F) #, simulate.p.value = TRUE)

areaA_R_P <- as.matrix(c(2,2,8,6,1,0,15,20,18,0))
dim(areaA_R_P)<-c(5, 2)
fisher.test(areaA_R_P)
chisq.test(areaA_R_P)

areaA_F_P <- as.matrix(c(4,4,8,7,0,15,20,18))
dim(areaA_F_P)<-c(4, 2)
fisher.test(areaA_F_P)
chisq.test(areaA_F_P)
chisq.test(areaC_F_P)$expected

areaA_R_F_P <- as.matrix(c(2, 2, 8, 6, 1, 4, 4, 8, 7, 1, 0, 16, 22, 18, 0))
dim(areaA_R_F_P) <- c(5,3)
chisq.test(areaA_R_F_P)
fisher.test(areaA_R_F_P)
 
########## testing differences across groups in assessing migration possibilities by ordered probit models ################
library(MASS)

data <-subset(data, data$omradeA < 5)

omradeA <- factor(omradeA, levels = c("1", "2", "3", "4"), ordered=is.ordered(omradeA))
omradeB <- factor(omradeB, levels = c("1", "2", "3", "4"), ordered=is.ordered(omradeB))
omradeC <- factor(omradeC, levels = c("1", "2", "3", "4"), ordered=is.ordered(omradeC))
omradeD <- factor(omradeD, levels = c("1", "2", "3", "4"), ordered=is.ordered(omradeD))

data$omradeA <- as.factor(data$omradeA)
data$omradeB <- as.factor(data$omradeB)
data$omradeC <- as.factor(data$omradeC)
data$omradeD <- as.factor(data$omradeD)

str(data)
attach(data)
names(data)

migration_D <- polr(omradeD ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(migration_D)

migration_D_dum <- polr(omradeD ~ fisker_dum + public_dum + biologi+okosystem+fiskeriene+marked+management, data=data, method="probit")
summary(migration_D_dum)


##############################THREATS AND BENEFITS ######################################

######################### testing differences in mean and variance #####################

data <- subset(data, data$recreation < 8)
data_public <- subset(data, data$stakeholder == 3)
mean(data$recreation[stakeholder == "1"])
mean(data$recreation[stakeholder == "2"])
mean(data_public$recreation)

var(data$recreation[stakeholder== "1"])
var(data$recreation[stakeholder== "2"])
var(data_public$recreation)

t.test(data$recreation[stakeholder== "1"], data$recreation[stakeholder== "2"])
t.test(data_public$recreation, data$recreation[stakeholder== "2"])

##################### testing differences in distribution of answers; Pearson chi square and Fisher #########################
seabed_R_P = as.matrix(c(4, 7, 6, 1, 0, 0, 11, 21, 13, 6, 1, 1))
dim(seabed_R_P) = c(6,2)
chisq.test(seabed_R_P)
fisher.test(seabed_R_P) #, simulate.p.value=TRUE)

cod_P_F = as.matrix(c(7, 16, 17, 11, 2, 0, 1, 7, 6, 4, 3, 2))
dim(cod_P_F) = c(6,2)
chisq.test(cod_P_F)
fisher.test(cod_P_F) #, simulate.p.value=TRUE)

haddock_F_R = as.matrix(c(0, 6, 6, 2, 3, 5, 7, 6, 2, 3))
dim(haddock_F_R) = c(5,2)
chisq.test(haddock_F_R)
fisher.test(haddock_F_R) #, simulate.p.value=TRUE)

flounder_R_P = as.matrix(c(0, 2, 8, 3, 2, 2, 0, 8, 17, 14, 8, 2, 0, 1))
dim(flounder_R_P) = c(7,2)
chisq.test(flounder_R_P)
fisher.test(flounder_R_P) #, simulate.p.value=TRUE)

capelin_F_R = as.matrix(c(1, 5, 7, 3, 3, 3, 0, 2, 2, 8, 2, 2))
dim(capelin_F_R) = c(6,2)
chisq.test(capelin_F_R)
fisher.test(capelin_F_R) #, simulate.p.value=TRUE)

cleanerfish_R_P = as.matrix(c(6, 15, 11, 8, 3, 0, 0, 2, 7, 3, 1, 1))
dim(cleanerfish_R_P) = c(6,2)
chisq.test(cleanerfish_R_P)
fisher.test(cleanerfish_R_P)

seaurchin_F_R = as.matrix(c(4, 8, 9, 1, 1, 0, 3, 8, 4, 0, 0, 2))
dim(seaurchin_F_R) = c(6,2)
chisq.test(seaurchin_F_R)
fisher.test(seaurchin_F_R)

oxygen_R_P = as.matrix(c(2, 6, 5, 3, 1, 0, 10, 8, 17, 7, 5, 2))
dim(oxygen_R_P) = c(6,2)
chisq.test(oxygen_R_P)
fisher.test(oxygen_R_P)

wolffish_F_R = as.matrix(c(1, 5, 9, 4, 1, 1, 0, 2, 5, 4, 2, 0))
dim(wolffish_F_R) = c(6,2)
chisq.test(wolffish_F_R)
fisher.test(wolffish_F_R)

kelp_R_P = as.matrix(c(6, 6, 2, 1, 2, 0, 3, 10, 17, 4, 4, 3))
dim(kelp_R_P) = c(6,2)
chisq.test(kelp_R_P)
fisher.test(kelp_R_P)

fisheries_F_R = as.matrix(c(6, 8, 3, 3, 2, 1, 6, 10, 1, 0))
dim(fisheries_F_R) = c(5,2)
chisq.test(fisheries_F_R)
fisher.test(fisheries_F_R)

tourism_R_P = as.matrix(c(4, 12, 2, 0, 0, 1, 15, 21, 7, 3, 3, 0))
dim(tourism_R_P) = c(6,2)
chisq.test(tourism_R_P)
fisher.test(tourism_R_P)

recreation_R_F = as.matrix(c(1, 8, 9, 0, 0, 4, 4, 7, 5, 1))
dim(recreation_R_F) = c(5,2)
chisq.test(recreation_R_F)
fisher.test(recreation_R_F)

########## script for ordered probit models ###########

seabed <- factor(seabed, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(seabed))
cod <- factor(cod, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(cod))
haddock <- factor(haddock, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(haddock))
flounder <- factor(flounder, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(flounder))
capelin <- factor(capelin, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(capelin))
cleaner <- factor(cleanerfish, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(cleanerfish))
seaurch <- factor(seaurch, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(seaurch))
oxygen <- factor(oxygen, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(oxygen))
wolffish <- factor(wolffish, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(wolffish))
kelp <- factor(kelp, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(kelp))
fisheries <- factor(fisheries, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(fisheries))
tourism <- factor(tourism, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(tourism))
recreation <- factor(recreation, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered=is.ordered(recreation))


data$seabed <- as.factor(data$seabed)
data$cod <- as.factor(data$cod)
data$haddock <- as.factor(data$haddock)
data$flounder <- as.factor(data$flounder)
data$capelin <- as.factor(data$capelin)
data$cleanerfish <- as.factor(data$cleanerfish)
data$seaurch <- as.factor(data$seaurch)
data$oxygen <- as.factor(data$oxygen)
data$wolffish <- as.factor(data$wolffish)
data$kelp <- as.factor(data$kelp)
data$fisheries <- as.factor(data$fisheries)
data$tourism <- as.factor(data$tourism)
data$recreation <- as.factor(data$recreation)


str(data)

seabed_harm <- polr(seabed ~ biologi + okosystem + fiskeriene + marked + management, data=data, method="probit")
summary(seabed_harm)

seabed_harm_dum <- polr(seabed ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(seabed_harm_dum)

cod_harm <- polr(cod ~ biologi + okosystem + fiskeriene + marked + management, data=data, method="probit")
summary(cod_harm)

cod_harm_dum <- polr(cod ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(cod_harm_dum)

haddock_harm_dum <- polr(haddock ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(haddock_harm_dum)

flounder_harm_dum <- polr(flounder ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(flounder_harm_dum)

capelin_harm_dum <- polr(capelin ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(capelin_harm_dum)

cleaner_harm_dum <- polr(cleanerfish ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(cleaner_harm_dum)

seaurch_harm_dum <- polr(seaurch ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(seaurch_harm_dum)

oxygen_harm_dum <- polr(oxygen ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(oxygen_harm_dum)

wolffish_opp_dum <- polr(wolffish ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(wolffish_opp_dum)

kelp_opp_dum <- polr(kelp ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(kelp_opp_dum)

fisheries_opp_dum <- polr(fisheries ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(fisheries_opp_dum)

tourism_opp_dum <- polr(tourism ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit")
summary(tourism_opp_dum)

recreation_opp_dum <- polr(recreation ~ biologi + okosystem + fiskeriene + marked + management + fisker_dum + public_dum, data=data, method="probit", na.rm=TRUE)
summary(recreation_opp_dum)


###################### Rank tests ################################
library(VGAM)

area_ranking_R <- c(1,2,3,4)
area_ranking_F <- c(1,2,3,4)
area_ranking_P <- c(1,2,3,4)

threats_ranking_R <- c(1, 6, 8, 5, 7, 4, 2, 3) # seabed, cod, haddock, flounder, capelin, cleanerfish, seaurchin, oxygen
threats_ranking_F <- c(2, 5, 8, 4, 7, 3, 1, 6)
threats_ranking_P <- c(1, 5, 6, 3, 8, 4, 2, 7)

kendall.tau(threats_ranking_F, threats_ranking_R)
kendall.tau(area_ranking_R, area_ranking_P)

cor.test(threats_ranking_F, threats_ranking_R, method="kendall", alternative = "greater", exact=FALSE)
cor.test(riskO, phO, method="kendall", alternative = "greater", exact=FALSE)

cor.test(threats_ranking_F, threats_ranking_P, method="spearm", alternative = "g")
cor.test(riskT, phT, method="spearm", alternative = "g")

opportunities_ranking_R <- c(5, 4, 1, 3, 2) # wolffish, kelp, fisheries, toruism, recreation
opportunities_ranking_F <- c(3, 3, 1, 2, 2)
opportunities_ranking_P <- c(5, 4, 3, 2, 1)

cor.test(opportunities_ranking_R, opportunities_ranking_P, method="kendall", alternative = "greater", exact=FALSE)
cor.test(opportunities_ranking_R, opportunities_ranking_P, method="spearm", alternative = "g")


######################## wilcoxson rank-sum test - NOT USED ############################

wilcox.test(data$omradeA[stakeholder == "1"], data$omradeA[stakeholder == "2"])

wilcox.test(flounder_r_adj, flounder_ph)

wilcox.test(cod_r, cod_ph)
wilcox.test(cod_r_adj, cod_ph)


############ Kolmogorov-Smirnov test  - NOT USED ####################
data <- subset(data, data$seabed < 10)

A <- data$seabed[data$stakeholder=="3"]
B <- data$seabed[data$stakeholder== "1"]

t.test(A,B)
ks.test(A,B)
var.test(A,B)

################## making the boxplots in Figures 4, 6, 8 #####################################

data$stakeholder = as.factor(data$stakeholder)

dataCod = subset(data, data$cod < 8)
dataHad = subset(data, data$haddock < 8)
dataFlo = subset(data, data$flounder < 8)
dataCap = subset(data, data$capelin < 8)
dataLump = subset(data, data$cleanerfish < 8)
dataSea = subset(data, data$seaurch < 8)
dataOx = subset(data, data$oxygen < 8)
dataBed = subset(data, data$seabed < 8)
dataWolf = subset(data, data$wolffish < 8, na.rm=TRUE)
dataKelp = subset(data, data$kelp < 8, na.rm=TRUE)
dataFish = subset(data, data$fisheries < 8, na.rm=TRUE)
dataTou = subset(data, data$tourism < 8, na.rm=TRUE)
dataRec = subset(data, data$recreation < 8)

dataA = subset(data, data$omradeA < 5, na.rm=TRUE)
dataB = subset(data, data$omradeB < 5, na.rm=TRUE)
dataC = subset(data, data$omradeC < 5, na.rm=TRUE)
dataD = subset(data, data$omradeD < 5, na.rm=TRUE)

index = order(tapply(data$omradeA, data$stakeholder, mean))
ordered = factor(rep(index, rep(97, 3)))
boxplot(data$omradeA~ordered, notch=T, names=as.character(index), xlab="stakeholder", ylab="probability")

means = tapply(data$omradeA, data$stakeholder, mean)
means

aov(data$omradeA~data$stakeholder) ##### testing means#######

library(car) 

leveneTest(omradeA~stakeholder, data=data)   ### testing variance

library(ggplot2)
library(dplyr)

name_vector = c("Scientists"=1, "Fishers"=2, "The public"=3)

dataSea$stakeholder= names(name_vector)[match(dataSea$stakeholder, name_vector)]
ggplot(dataSea, aes(dataSea$stakeholder, dataSea$seaurch))+ geom_boxplot()+stat_summary(fun=mean, geom="point", shape=20, size=10, color="red") + labs(title="Sea urchin", x="", y="answer category") + theme(text=element_text(size=20))

dataTou$stakeholder= names(name_vector)[match(dataTou$stakeholder, name_vector)]
ggplot(dataTou, aes(dataTou$stakeholder, dataTou$tourism))+ geom_boxplot()+stat_summary(fun=mean, geom="point", shape=20, size=10, color="red") + labs(title="Tourism", x="", y="") + theme(text=element_text(size=20))

dataC$stakeholder= names(name_vector)[match(dataC$stakeholder, name_vector)]
ggplot(dataC, aes(dataC$stakeholder, dataC$omradeC))+ geom_boxplot()+stat_summary(fun=mean, geom="point", shape=20, size=10, color="red") + labs(title="Area C", x="", y="answer category") + theme(text=element_text(size=20))

dataD$stakeholder= names(name_vector)[match(dataD$stakeholder, name_vector)]
ggplot(dataD, aes(dataD$stakeholder, dataD$omradeD))+ geom_boxplot()+stat_summary(fun=mean, geom="point", shape=20, size=10, color="red") + labs(title="Area D", x="", y="") + theme(text=element_text(size=20))


################ making the graphs for figures 2, 3, 5, 7 ##########################################
setwd

# Load packages
library(tidyverse)
library(scales)

# Read in the data
db <- read_rds(file.path("Data", "data-stakeholders.rds"))

# Knowledge ----
knowledge <- db |> 
  select(stakeholder_group, starts_with("knowledge")) |> 
  pivot_longer(cols = starts_with("knowledge_"), names_to = "about", values_to = "extent") |> 
  mutate(
    about = factor(about, 
                   levels = c(
                     "knowledge_biology", "knowledge_ecosystem_impacts", "knowledge_fishery_impacts",
                     "knowledge_market", "knowledge_management"
                   ),
                   labels = c(
                     "Biology", "Ecosystem impacts", "Fishery impacts", "Markets", "Management and regulation"
                   ))
  ) |> 
  filter(!is.na(extent)) |> 
  group_by(stakeholder_group, about, extent) |> 
  count() |> 
  ungroup() |>
  group_by(stakeholder_group, about) |> 
  mutate(
    percent_response = n / sum(n), 
    # percent_response = ifelse(extent %in% c("Unconcerned", "Somewhat unconcerned", "Neither concerned nor unconcerned"), percent_response, -percent_response)
  ) |> 
  ungroup() |> 
  mutate(
    percent_response_label = percent(abs(percent_response), accuracy = 1)
  ) |> 
  ggplot(aes(x = stakeholder_group, y = percent_response, fill = extent)) +
  geom_col() + 
  geom_text(aes(label = percent_response_label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) + 
  geom_hline(yintercept = 0, color =c("white")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 1, 0.1) * 100, "%")) +
  coord_flip() +
  scale_fill_discrete("Percent") + 
  facet_wrap(vars(about), ncol = 2) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

knowledge  

ggsave("knowledge-plot.jpg", plot = knowledge)


# Spread --- 
spread <- db |> 
  select(stakeholder_group, starts_with("spread")) |> 
  pivot_longer(cols = starts_with("spread"), names_to = "about", values_to = "extent") |> 
  mutate(
    about = factor(about, 
                   levels = c(
                     "spread_a", "spread_b", "spread_c", "spread_d"
                   ),
                   labels = c(
                     "Spread A", "Spread B", "Spread C", "Spread D"
                   ))
  ) |> 
  filter(!is.na(extent)) |> 
  group_by(stakeholder_group, about, extent) |> 
  count() |> 
  ungroup() |>
  group_by(stakeholder_group, about) |> 
  mutate(
    percent_response = n / sum(n), 
    # percent_response = ifelse(extent %in% c("Unconcerned", "Somewhat unconcerned", "Neither concerned nor unconcerned"), percent_response, -percent_response)
  ) |> 
  ungroup() |> 
  mutate(
    percent_response_label = percent(abs(percent_response), accuracy = 1)
  ) |> 
  ggplot(aes(x = stakeholder_group, y = percent_response, fill = (extent))) +
  geom_col() + 
  geom_text(aes(label = percent_response_label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 8) + 
  geom_hline(yintercept = 0, color =c("white")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 1, 0.1) * 100, "%")) +
  # coord_flip() +
  scale_fill_discrete("Percent") + 
  facet_wrap(vars(about), ncol = 2) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

spread 

ggsave("spread-plot.jpg", plot = spread)


# Threats ----
threats <- db |> 
  select(stakeholder_group, starts_with("threat_")) |> 
  pivot_longer(cols = starts_with("threat_"), names_to = "about", values_to = "extent") |> 
  mutate(
    about = factor(about,
                   levels = c("threat_seabed", "threat_cod", "threat_haddock", "threat_flounder", 
                              "threat_capelin", "threat_cleanerfish", "threat_sea_urchin", 
                              "threat_oxygen_transport")
                   ,labels = c("Seabed", "Cod", "Haddock", "Flounder", "Capelin", "Cleanerfish",
                               "Sea urchin", "Oxygen transport")),
    extent = factor(extent, levels = c("I don't know",
                                       "Opposite", 
                                       "None", 
                                       "Very small", 
                                       "Small", 
                                       "Moderate",
                                       "Large", 
                                       "Very large"))
  ) |> 
  filter(!is.na(extent) & extent != "I don't know") |> 
  group_by(stakeholder_group, about, extent) |> 
  count() |> 
  ungroup() |>
  group_by(stakeholder_group, about) |> 
  mutate(
    percent_response = n / sum(n), 
    percent_response = ifelse(extent %in% c("Moderate", "Large", "Very large"), percent_response, -percent_response)
  ) |> 
  ungroup() |> 
  mutate(
    percent_response_label = percent(abs(percent_response), accuracy = 1)
  ) |> 
  arrange(extent) |> 
  ggplot(aes(x = stakeholder_group, y = percent_response, fill = fct_relevel(extent, 
                                                                             "I don't know",
                                                                             "Opposite", 
                                                                             "None", 
                                                                             "Very small", 
                                                                             "Small", 
                                                                             "Very large",
                                                                             "Large",
                                                                             "Moderate",
                                                                             
                                                                             
  ))) +
  geom_col(position = position_stack(reverse = FALSE)) + 
  geom_text(aes(label = percent_response_label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) + 
  geom_hline(yintercept = 0, color =c("white")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), labels = paste0(seq(-1, 1, 0.5) * 100, "%")) +
  coord_flip() +
  scale_fill_discrete("Percent") + 
  facet_wrap(vars(about), ncol = 2) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

threats

ggsave("threats-plot.jpg", plot = threats)

# Opportunities ----
opportunities <- db |> 
  select(stakeholder_group, starts_with("opportunity")) |> 
  pivot_longer(cols = starts_with("opportunity"), names_to = "about", values_to = "extent") |> 
  mutate(
    about = factor(about,
                   levels = c("opportunity_wolffish", "opportunity_kelp", "opportunity_fisheries", 
                              "opportunity_tourism", "opportunity_recreation")
                   ,labels = c("Wolf fish", "Kelp", "Fisheries", "Tourism", "Recreation")),
    extent = factor(extent, levels = c("I don't know",
                                       "Opposite", 
                                       "None", 
                                       "Very small", 
                                       "Small", 
                                       "Moderate",
                                       "Large", 
                                       "Very large"))
  ) |> 
  filter(!is.na(extent) & extent != "I don't know") |> 
  group_by(stakeholder_group, about, extent) |> 
  count() |> 
  ungroup() |>
  group_by(stakeholder_group, about) |> 
  mutate(
    percent_response = n / sum(n), 
    percent_response = ifelse(extent %in% c("Moderate", "Large", "Very large"), percent_response, -percent_response)
  ) |> 
  ungroup() |> 
  mutate(
    percent_response_label = percent(abs(percent_response), accuracy = 1)
  ) |> 
  arrange(extent) |> 
  ggplot(aes(x = stakeholder_group, y = percent_response, fill = fct_relevel(extent, 
                                                                             "I don't know",
                                                                             "Opposite", 
                                                                             "None", 
                                                                             "Very small", 
                                                                             "Small", 
                                                                             "Very large",
                                                                             "Large",
                                                                             "Moderate",
                                                                             
                                                                             
  ))) +
  geom_col(position = position_stack(reverse = FALSE)) + 
  geom_text(aes(label = percent_response_label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) + 
  geom_hline(yintercept = 0, color =c("white")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), labels = paste0(seq(-1, 1, 0.5) * 100, "%")) +
  coord_flip() +
  scale_fill_discrete("Percent") + 
  facet_wrap(vars(about), ncol = 2) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

opportunities

ggsave("opportunities-plot.jpg", plot = opportunities)














