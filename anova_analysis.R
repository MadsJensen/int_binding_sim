
# LME4 --------------------------------------------------------------------
library(lme4)

data2 <- as.data.frame(data_grp)

m1 <- lmer(value ~ 1 + (1|id), data = data2, REML = FALSE)
m2 <- update(m1, .~. + condition)
m3 <- update(m2, .~. + modality)
m4 <- update(m3, .~. + condition:modality)

anova(m1, m2, m3, m4)


# ez ----------------------------------------------------------------------


library(ez)


anova_data <- as.data.frame(data_grp[,1:4])
anova_data$condition <- factor(anova_data$condition)
anova_data$modality <- factor(anova_data$modality)
anova_data$id <- factor(anova_data$id)

anova_model <- ezANOVA(
    data = anova_data,
    dv = .(value),
    wid = .(id),
    within = .(condition, modality),
    type = 3,
    detailed = TRUE
)
print(anova_model)


fooTablefoo <- anova_model$ANOVA
fooTablefoo$p <- round(fooTablefoo$p, 4)
fooTablefoo$F <- round(fooTablefoo$F, 4)
fooTablefoo$SSn <- round(fooTablefoo$SSn, 4)
fooTablefoo$SSd <- round(fooTablefoo$SSd, 4)
fooTablefoo$ges <- round(fooTablefoo$ges, 4)
print(fooTablefoo)
