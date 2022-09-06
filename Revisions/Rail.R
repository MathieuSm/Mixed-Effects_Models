library(nlme)

# create a data frame from the grouped data object, omitting the ordering of rails
rail.df <- data.frame(Rail)
rail.df$Rail <- factor(as.numeric(levels(rail.df$Rail))[rail.df$Rail], ordered = FALSE)

rail.lme <- lme(travel ~ 1, random = ~ 1 | Rail, data = rail.df)
summary(rail.lme)

library(lme4)
rail.lmer <- lmer(travel ~ 1 + (1 | Rail), data=rail.df)
rail.lmer

X = getME(rail.lmer,"X")
Z = getME(rail.lmer,"Z")

printSpMatrix(Z)