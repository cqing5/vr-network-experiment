assess_dat <- read.csv("Assessment Scores.csv", header=TRUE, na.strings=c(""," ","NA"))

#rename conditions to be more descriptive
assess_dat$p_cond[assess_dat$p_cond == "A"] <- "Desktop"
assess_dat$p_cond[assess_dat$p_cond == "B"] <- "Immersive VR"

#t test
t.test(total ~ p_cond, data = assess_dat)
cohens_d(total ~ p_cond, data = assess_dat)

#correlation
pres_ass <- cor.test(assess_dat$presence, data$total, 
                      method = "pearson")
pres_ass

#bar graph
assess_dat %>%
  group_by(p_cond) %>%
  summarise(M = mean(total),
            SD = sd(total),
            count = n(),
            ci_total = 1.96 * sd(total) / sqrt(count)) -> mean_total

ggplot(data = mean_total, aes(x = p_cond, y = M, ymin = M-ci_total, ymax= M+ci_total)) +
  geom_bar(stat = "identity", width = 0.5, fill="darkgrey") +
  geom_errorbar(width = 0.05) +
  coord_cartesian(ylim = c(1,11)) +
  ylab("Mean Assessment Score") +
  xlab("Condition") +
  theme_minimal()

#inter rater reliability
kappa2(assess_dat[,3:4], weight = c("squared"), sort.levels = FALSE)
kappa2(assess_dat[,5:6], weight = c("squared"), sort.levels = FALSE)
kappa2(assess_dat[,7:8], weight = c("squared"), sort.levels = FALSE)
