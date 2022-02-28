library(nlme)


# create a data frame from the grouped data object, omitting the ordering of rails
rail.df <- data.frame(Rail)
rail.df$Rail <- factor(as.numeric(levels(rail.df$Rail))[rail.df$Rail], ordered = FALSE)


# model with fixed effects

rail.lm.1 <- lm(travel ~ Rail - 1, data = rail.df)
rail.lm.1
summary(rail.lm.1)

rail.lm.0 <- lm(travel ~ 1, data = rail.df)
anova(rail.lm.0, rail.lm.1)
# same global test of the rail effect:
summary(lm(travel ~ Rail, data = rail.df))
anova(lm(travel ~ Rail, data = rail.df))
summary(aov(travel ~ Rail, data = rail.df))


# model with random effects

rail.lme <- lme(travel ~ 1, random = ~ 1 | Rail, data = rail.df)
summary(rail.lme)


detach(package:nlme)
library(lme4)
rail.lmer <- lmer(travel ~ 1 + (1 | Rail), data = rail.df)
summary(rail.lmer)
detach(package:lme4)
library(nlme)
