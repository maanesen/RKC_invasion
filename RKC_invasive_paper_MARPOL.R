# clear memory
rm(list = ls())

# 1. Load package
pacman::p_load(
  tidyverse, # data management packages
  readxl # handle Excel data
)


#######################################################################
data <- read_excel("data_RKC_survey_OAS_13025.xlsx")

dataFisker <- subset(data, data$fisker_dum==1)
dataForsker <- subset(data, data$forsker_dum == 1)
dataPublic <- subset(data, data$public_dum ==1)

dataA <- subset(data, data$omradeA)

summary(data)

attach(data)

######### socio-demographics & knowledge; input to tables 2-4 ########
mean(AGE, na.rm = TRUE)
# can also use summary!

# level of knowledge
table(biologi, stakeholder)
table(ecosystem, stakeholder)

######## migration #################################
table(omradeA, stakeholder)

####### ecosystem impacts #########################
table(recreation, stakeholder)

############### Testing mean score and variance ################################
as.numeric(data$omradeA)
as.numeric(data$omradeB)
as.numeric(data$omradeC)
as.numeric(data$omradeD)

data <- subset(data, data$omradeD < 5)
data_public <- subset(data, data$stakeholder == 3)

mean(data$omradeA)
mean(data$omradeD[stakeholder == "1"])
mean(data$omradeD[stakeholder == "2"])
mean(data_public$omradeD)

var(data$omradeD[stakeholder == "1"])
var(data$omradeD[stakeholder == "2"])
var(data_public$omradeD)

t.test(data$omradeC[data$stakeholder == "3"], data$omradeC[data$stakeholder == "2"])
t.test(data_public$omradeD, data$omradeD[stakeholder == "1"])

data <- subset(data, data$recreation < 8)
data_public <- subset(data, data$stakeholder == 3)
mean(data$recreation[stakeholder == "1"])
mean(data$recreation[stakeholder == "2"])
mean(data_public$recreation)

var(data$recreation[stakeholder == "1"])
var(data$recreation[stakeholder == "2"])
var(data_public$recreation)

t.test(data$recreation[stakeholder == "1"], data$recreation[stakeholder == "2"])
t.test(data_public$recreation, data$recreation[stakeholder == "2"])

pvalsRec <- c(0.71, 0.037, 0.13)
p.adjust(pvalsRec, method = "holm")


############# Testing independence of answers to area-question, Pearson's chi-square and Fisher's exact tests ##############
#### areaA ######
library(DescTools)

areaA_R_F <- as.matrix(c(2, 2, 8, 6, 4, 4, 8, 7))
dim(areaA_R_F) <- c(4, 2)
chisq.test(areaA_R_F, correct = F)
fisher.test(areaA_R_F) # , simulate.p.value = TRUE)
CramerV(areaA_R_F)

areaA_R_P <- as.matrix(c(2, 2, 8, 6, 0, 16, 22, 18))
dim(areaA_R_P) <- c(4, 2)
fisher.test(areaA_R_P)
chisq.test(areaA_R_P)
CramerV(areaA_R_P)

areaA_F_P <- as.matrix(c(4, 4, 8, 7, 0, 16, 22, 18))
dim(areaA_F_P) <- c(4, 2)
fisher.test(areaA_F_P)
chisq.test(areaA_F_P)
chisq.test(areaA_F_P)$expected
CramerV(areaA_F_P)

areaA_R_F_P <- as.matrix(c(2, 2, 8, 6, 1, 4, 4, 8, 7, 1, 0, 16, 22, 18, 0))
dim(areaA_R_F_P) <- c(5, 3)
chisq.test(areaA_R_F_P)
fisher.test(areaA_R_F_P)
CramerV(areaA_R_F_P)

### areaB ######
areaB_R_F <- as.matrix(c(3, 3, 10, 2, 0, 6, 6, 2, 4, 5))
dim(areaB_R_F) <- c(5, 2)
chisq.test(areaB_R_F, correct = F)
fisher.test(areaB_R_F) # , simulate.p.value = TRUE)
CramerV(areaB_R_F)

areaB_R_P <- as.matrix(c(3, 3, 10, 2, 2, 19, 24, 11))
dim(areaB_R_P) <- c(4, 2)
fisher.test(areaB_R_P)
chisq.test(areaB_R_P)
CramerV(areaB_R_P)

areaB_F_P <- as.matrix(c(6, 6, 2, 4, 5, 2, 19, 24, 11, 0))
dim(areaB_F_P) <- c(5, 2)
fisher.test(areaB_F_P)
chisq.test(areaB_F_P)
chisq.test(areaB_F_P)$expected
CramerV(areaB_F_P)

areaB_R_F_P <- as.matrix(c(2, 2, 8, 6, 1, 4, 4, 8, 7, 1, 0, 16, 22, 18, 0))
dim(areaB_R_F_P) <- c(5, 3)
chisq.test(areaB_R_F_P)
fisher.test(areaB_R_F_P)
CramerV(areaB_R_F_P)

##### areaC ######
areaB_R_F <- as.matrix(c(5, 8, 5, 0, 0, 11, 4, 3, 4, 1))
dim(areaB_R_F) <- c(5, 2)
chisq.test(areaB_R_F, correct = F)
fisher.test(areaB_R_F) # , simulate.p.value = TRUE)
CramerV(areaB_R_F)

areaB_R_P <- as.matrix(c(5, 8, 5, 0, 7, 22, 20, 7))
dim(areaB_R_P) <- c(4, 2)
fisher.test(areaB_R_P)
chisq.test(areaB_R_P)
CramerV(areaB_R_P)

areaB_F_P <- as.matrix(c(11, 4, 3, 4, 1, 7, 22, 20, 7, 0))
dim(areaB_F_P) <- c(5, 2)
fisher.test(areaB_F_P)
chisq.test(areaB_F_P)
chisq.test(areaB_F_P)$expected
CramerV(areaB_F_P)

areaB_R_F_P <- as.matrix(c(2, 2, 8, 6, 1, 4, 4, 8, 7, 1, 0, 16, 22, 18, 0))
dim(areaB_R_F_P) <- c(5, 3)
chisq.test(areaB_R_F_P)
fisher.test(areaB_R_F_P)
CramerV(areaB_R_F_P)

#### area D #####
areaB_R_F <- as.matrix(c(9, 7, 2, 0, 12, 5, 4, 2))
dim(areaB_R_F) <- c(4, 2)
chisq.test(areaB_R_F, correct = F)
fisher.test(areaB_R_F) # , simulate.p.value = TRUE)
CramerV(areaB_R_F)

areaB_R_P <- as.matrix(c(9, 7, 2, 0, 16, 18, 16, 6))
dim(areaB_R_P) <- c(4, 2)
fisher.test(areaB_R_P)
chisq.test(areaB_R_P)
CramerV(areaB_R_P)

areaB_F_P <- as.matrix(c(12, 5, 4, 0, 2, 16, 18, 16, 6, 0))
dim(areaB_F_P) <- c(5, 2)
fisher.test(areaB_F_P)
chisq.test(areaB_F_P)
#chisq.test(areaB_F_P)$expected
CramerV(areaB_F_P)

areaB_R_F_P <- as.matrix(c(2, 2, 8, 6, 1, 4, 4, 8, 7, 1, 0, 16, 22, 18, 0))
dim(areaB_R_F_P) <- c(5, 3)
chisq.test(areaB_R_F_P)
fisher.test(areaB_R_F_P)
CramerV(areaB_R_F_P)


#################### Cramer's V for given test statistics ####################
stat <- 5.62
obs <- 69
V <- sqrt((stat)/obs*1)
V




##################### testing differences in threats & benefits #########################
seabed_R_F <- as.matrix(c(4, 7, 6, 1, 0, 5, 4, 11, 2, 1))
dim(seabed_R_F) <- c(5, 2)
chisq.test(seabed_R_F)
fisher.test(seabed_R_F) # , simulate.p.value=TRUE)
CramerV(seabed_R_F)


cod_F_R <- as.matrix(c(1, 7, 6, 4, 3, 2, 0, 0, 0, 4, 2, 7, 2, 1, 1, 1))
dim(cod_F_R) <- c(8, 2)
chisq.test(cod_F_R)
fisher.test(cod_F_R) # , simulate.p.value=TRUE)
CramerV(cod_F_R)


haddock_P_F <- as.matrix(c(7, 12, 14, 12, 2, 1, 6, 0, 5, 7, 6, 2, 3, 0))
dim(haddock_P_F) <- c(7, 2)
chisq.test(haddock_P_F)
fisher.test(haddock_P_F) # , simulate.p.value=TRUE)
CramerV(haddock_P_F)


flounder_F_R <- as.matrix(c(2, 9, 4, 4, 2, 2, 0, 2, 8, 3, 2, 2))
dim(flounder_F_R) <- c(6, 2)
chisq.test(flounder_F_R)
fisher.test(flounder_F_R) # , simulate.p.value=TRUE)
CramerV(flounder_F_R)


capelin_R_P <- as.matrix(c(0, 2, 2, 8, 2, 2, 2, 4, 10, 13, 9, 6, 0, 14))
dim(capelin_R_P) <- c(7, 2)
chisq.test(capelin_R_P)
fisher.test(capelin_R_P) # , simulate.p.value=TRUE)
CramerV(capelin_R_P)

cleanerfish_F_R <- as.matrix(c(2, 7, 3, 1, 1, 4, 7, 10, 3, 2, 1, 0))
dim(cleanerfish_F_R) <- c(6, 2)
chisq.test(cleanerfish_F_R)
fisher.test(cleanerfish_F_R)
CramerV(cleanerfish_F_R)

seaurchin_R_P <- as.matrix(c(3, 8, 4, 0, 0, 2, 1, 10, 18, 16, 4, 1, 1, 6))
dim(seaurchin_R_P) <- c(7, 2)
chisq.test(seaurchin_R_P)
fisher.test(seaurchin_R_P)
CramerV(seaurchin_R_P)


oxygen_F_R <- as.matrix(c(1, 2, 9, 3, 3, 2, 1, 2, 2, 6, 5, 3, 1, 0, 0, 1))
dim(oxygen_F_R) <- c(8, 2)
chisq.test(oxygen_F_R)
fisher.test(oxygen_F_R)
CramerV(oxygen_F_R)

wolffish_R_P <- as.matrix(c(0, 2, 5, 4, 2, 0, 5, 1, 12, 17, 4, 3, 3, 16))
dim(wolffish_R_P) <- c(7, 2)
chisq.test(wolffish_R_P)
fisher.test(wolffish_R_P)
CramerV(wolffish_R_P)


kelp_F_R <- as.matrix(c(1, 2, 3, 6, 3, 2, 2, 4, 0, 6, 6, 2, 1, 2, 0, 1))
dim(kelp_F_R) <- c(8, 2)
chisq.test(kelp_F_R)
fisher.test(kelp_F_R)
CramerV(kelp_F_R)

fisheries_R_P <- as.matrix(c(1, 6, 10, 1, 0, 0, 0, 0, 2, 11, 26, 6, 4, 2, 2, 3))
dim(fisheries_R_P) <- c(8, 2)
chisq.test(fisheries_R_P)
fisher.test(fisheries_R_P)
CramerV(fisheries_R_P)

tourism_F_R <- as.matrix(c(3, 8, 5, 4, 1, 2, 0, 4, 12, 2, 0, 0))
dim(tourism_F_R) <- c(6, 2)
chisq.test(tourism_F_R)
fisher.test(tourism_F_R)
CramerV(tourism_F_R)

recreation_F_P <- as.matrix(c(4, 4, 7, 5, 0, 0, 1, 2, 2, 21, 16, 7, 2, 2, 1, 5))
dim(recreation_F_P) <- c(8, 2)
chisq.test(recreation_F_P)
fisher.test(recreation_F_P)
CramerV(recreation_F_P)

################## test population in east vs west ############################
install.packages("DescTools")
library(DescTools)

rec <- as.matrix(c(1, 17, 12, 6, 2, 1, 1, 1, 4, 4, 1, 0, 1, 0))
dim(rec) <- c(7, 2)
chisq.test(rec)
fisher.test(rec) # , simulate.p.value=TRUE)
CramerV(rec)

tou <- as.matrix(c(9, 18, 6, 3, 2, 6, 3, 1, 0, 1))
dim(tou) <- c(5, 2)
chisq.test(tou)
fisher.test(tou) # , simulate.p.value=TRUE)
CramerV(tou)

fish <- as.matrix(c(1, 9, 20, 5, 4, 1, 2, 1, 2, 6, 1, 0, 1, 0))
dim(fish) <- c(7, 2)
chisq.test(fish)
fisher.test(fish) # , simulate.p.value=TRUE)
CramerV(fish)

kelp <- as.matrix(c(3, 9, 12, 4, 4, 1, 0, 1, 5, 0, 0, 2))
dim(kelp) <- c(6, 2)
chisq.test(kelp)
fisher.test(kelp) # , simulate.p.value=TRUE)
CramerV(kelp)

wolf <- as.matrix(c(1, 10, 13, 3, 3, 2, 0, 2, 4, 1, 0, 1))
dim(wolf) <- c(6, 2)
chisq.test(wolf)
fisher.test(wolf) # , simulate.p.value=TRUE)
CramerV(wolf)

sea <- as.matrix(c(8, 16, 11, 6, 1, 0,3, 5, 2, 0, 0, 1))
dim(sea) <- c(6, 2)
chisq.test(sea)
fisher.test(sea) # , simulate.p.value=TRUE)
CramerV(sea)

cod <- as.matrix(c(5, 13, 13, 10, 1, 2, 3, 4, 1, 1))
dim(cod) <- c(5, 2)
chisq.test(cod)
fisher.test(cod) # , simulate.p.value=TRUE)
CramerV(cod)

haddock <- as.matrix(c(5, 10, 11, 11, 1, 1, 2, 2, 3, 1, 1, 0))
dim(haddock) <- c(6, 2)
chisq.test(haddock)
fisher.test(haddock) # , simulate.p.value=TRUE)
CramerV(haddock)

seaurc <- as.matrix(c(8, 14, 13, 4, 0, 1, 2, 4, 3, 0, 1, 0))
dim(seaurc) <- c(6, 2)
chisq.test(seaurc)
fisher.test(seaurc) # , simulate.p.value=TRUE)
CramerV(seaurc)

oxygen <- as.matrix(c(7, 6, 14, 6, 4, 2, 3, 2, 3, 1, 1, 0))
dim(oxygen) <- c(6, 2)
chisq.test(oxygen)
fisher.test(oxygen) # , simulate.p.value=TRUE)
CramerV(oxygen)

cleaner <- as.matrix(c(4, 13, 8, 6, 2, 2, 2, 3, 2, 1))
dim(cleaner) <- c(5, 2)
chisq.test(cleaner)
fisher.test(cleaner) # , simulate.p.value=TRUE)
CramerV(cleaner)

flounder <- as.matrix(c(6, 14, 10, 7, 1, 1, 2, 3, 4, 1, 1, 0))
dim(flounder) <- c(6, 2)
chisq.test(flounder)
fisher.test(flounder) # , simulate.p.value=TRUE)
CramerV(flounder)

pvalsRec <- c(0.056, 0.164, 0.47)
p.adjust(pvalsRec, method = "holm")

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














