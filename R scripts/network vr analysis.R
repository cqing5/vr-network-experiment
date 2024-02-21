# packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(effectsize)
library(lsr)
library(ggpubr)
library(ltm)


# read in data
data <- read.csv("Network Visualization App Survey.csv", header=TRUE, na.strings=c(""," ","NA"))
data <- data[-c(1:2),] # delete rows 1 and 2 (headers)
data <- data[, -c(1:7)] # remove columns not necessary for analysis

data[2:20] <- lapply(data[2:20], as.numeric) # change from chr to num
data[30] <- lapply(data[30], as.numeric) # change from chr to num

# rename conditions to be more descriptive
data$p_cond[data$p_cond == "A"] <- "Desktop"
data$p_cond[data$p_cond == "B"] <- "Immersive VR"

# create variables for mean scores
# arcs items
arcs <- c("arcs_confidence1", "arcs_confidence2", "arcs_confidence3", "arcs_satisfaction1", "arcs_satisfaction2", "arcs_satisfaction3", "arcs_relevance1", "arcs_relevance2", "arcs_relevance3", "arcs_attention1", "arcs_attention2", "arcs_attention3")
data[ ,arcs]
data$arcs <- apply(data[ ,arcs], 1, mean)
# effectiveness
effective <- c("eff_comp", "eff_apply", "eff_engage")
data[ ,effective]
data$effective <- apply(data[ ,effective], 1, mean)
# presence
presence <- c("presence_2", "presence_1")
data[ ,presence]
data$presence <- apply(data[ ,presence], 1, mean)

# t test on arcs
t.test(arcs ~ p_cond, data = data)
cohens_d(arcs ~ p_cond, data = data)
#t test on effectiveness
t.test(effective ~ p_cond, data = data)
cohens_d(effective ~ p_cond, data = data)
#t test on presence
t.test(presence ~ p_cond, data = data)
cohens_d(presence ~ p_cond, data = data)

#correlation btwn presence and motivation
pres_arcs <- cor.test(data$presence, data$arcs, 
                     method = "pearson")
pres_arcs

#internal reliability
arcs_data <- data.frame(data[,5:13])
cronbach.alpha(arcs_data)

pres_data <- data.frame(data[,17:18])
cronbach.alpha(pres_data)

data %>%
  group_by(p_cond) %>%
  summarise(M = mean(presence),
              SD = sd(presence)) -> mean_presence
# ARCS bar graph
data %>%
  group_by(p_cond) %>%
  summarise(M = mean(arcs),
  SD = sd(arcs),
  count = n(),
  ci_arcs = 1.96 * sd(arcs) / sqrt(count)) -> mean_arcs

ggplot(data = mean_arcs, aes(x = p_cond, y = M, ymin = M-ci_arcs, ymax= M+ci_arcs)) +
  geom_bar(stat = "identity", width = 0.5, fill="darkgrey") +
  geom_errorbar(width = 0.05) +
  coord_cartesian(ylim = c(1,5)) +
  ylab("Mean Learning Motivation") +
  xlab("Condition") +
  theme_minimal()

# PROCESS Mediation: VR condition --> Motivation --> Assessment Score  
data$VR <-ifelse(data$p_cond =="VR",1,
                     ifelse(data$p_cond=="Desktop",0,NA))
process(data=data,y="total",x="VR",m="arcs",model=4,boot=10000,total=1,seed=17623)
